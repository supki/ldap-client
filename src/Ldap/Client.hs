{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | This module is intended to be imported qualified
--
-- @
-- import qualified Ldap.Client as Ldap
-- @
module Ldap.Client
  ( with
  , Host(..)
  , PortNumber
  , Ldap
  , LdapError(..)
  , ResponseError(..)
  , Type.ResultCode(..)
    -- * Bind
  , Password(..)
  , bind
  , close
    -- * Search
  , search
  , SearchEntry(..)
    -- ** Search modifiers
  , Search
  , Mod
  , Type.Scope(..)
  , open
  , scope
  , size
  , time
  , typesOnly
  , Type.DerefAliases(..)
  , derefAliases
  , Filter(..)
    -- * Modify
  , modify
  , Operation(..)
    -- * Add
  , add
    -- * Delete
  , delete
    -- * ModifyDn
  , RelativeDn(..)
  , modifyDn
    -- * Compare
  , compare
    -- * Extended
  , Oid(..)
  , extended
    -- * Miscellanous
  , Dn(..)
  , Attr(..)
  , AttrValue
  , AttrList
    -- * Re-exports
  , NonEmpty
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>), (<*>))
#endif
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (atomically, throwSTM)
import           Control.Concurrent.STM.TMVar (putTMVar)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Exception (Exception, Handler(..), bracket, throwIO, catch, catches)
import           Control.Monad (forever, forM)
import qualified Data.ASN1.BinaryEncoding as Asn1
import qualified Data.ASN1.Encoding as Asn1
import qualified Data.ASN1.Error as Asn1
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           Data.Foldable (asum)
import           Data.Function (fix)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as Map
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (mempty)
#endif
import           Data.String (fromString)
import           Data.Text (Text)
#if __GLASGOW_HASKELL__ < 710
import           Data.Traversable (traverse)
#endif
import           Data.Typeable (Typeable)
import           Network.Connection (Connection)
import qualified Network.Connection as Conn
import           Prelude hiding (compare)
import qualified System.IO.Error as IO

import           Ldap.Asn1.FromAsn1 (FromAsn1, parseAsn1)
import           Ldap.Asn1.ToAsn1 (encode)
import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Asn1.ToAsn1 (ToAsn1(toAsn1))
import           Ldap.Client.Internal
import           Ldap.Client.Bind (Password(..), bind)
import           Ldap.Client.Search
  ( search
  , Search
  , Mod
  , scope
  , size
  , time
  , typesOnly
  , derefAliases
  , Filter(..)
  , SearchEntry(..)
  )
import           Ldap.Client.Modify (Operation(..), modify, RelativeDn(..), modifyDn)
import           Ldap.Client.Add (add)
import           Ldap.Client.Delete (delete)
import           Ldap.Client.Compare (compare)
import           Ldap.Client.Extended (Oid(..), extended)

{-# ANN module "HLint: ignore Use first" #-}


newLdap :: Connection -> IO Ldap
newLdap conn = do
  inq  <- newTQueueIO
  outq <- newTQueueIO
  client <- newTQueueIO
  as   <- traverse Async.async
    [ input inq conn
    , output outq conn
    , dispatch client inq outq
    ]
  tvar  <- newTVarIO (Type.Id 0)
  return $ Ldap client tvar conn as

-- | Various failures that can happen when working with LDAP.
data LdapError =
    IOError IOError             -- ^ Network failure.
  | ParseError Asn1.ASN1Error   -- ^ Invalid ASN.1 data received from the server.
  | ResponseError ResponseError -- ^ An LDAP operation failed.
  | DisconnectError Disconnect  -- ^ Notice of Disconnection has been received.
    deriving (Show, Eq)

newtype WrappedIOError = WrappedIOError IOError
    deriving (Show, Eq, Typeable)

instance Exception WrappedIOError

data Disconnect = Disconnect Type.ResultCode Dn Text
    deriving (Show, Eq, Typeable)

instance Exception Disconnect

open :: Host -> PortNumber -> IO Ldap
open host port = do
  context <- Conn.initConnectionContext
  conn <- Conn.connectTo context (params host port)
  l <- newLdap conn
  return l

close :: Ldap -> IO ()
close l@(Ldap client v conn threads) = do
  unbindAsync l
  forM threads Async.cancel
  Conn.connectionClose conn

-- | The entrypoint into LDAP.
--
-- It catches all LDAP-related exceptions.
with :: Host -> PortNumber -> (Ldap -> IO a) -> IO (Either LdapError a)
with host port f = do
  bracket (open host port) close (\l -> do
    as   <- Async.async $ f l
    fmap Right (Async.wait as))
 `catches`
  [ Handler (\(WrappedIOError e) -> return (Left (IOError e)))
  , Handler (return . Left . ParseError)
  , Handler (return . Left . ResponseError)
  ]

params :: Host -> PortNumber -> Conn.ConnectionParams
params host port = Conn.ConnectionParams
  { Conn.connectionHostname =
      case host of
        Plain    h -> h
        Secure   h -> h
        Insecure h -> h
  , Conn.connectionPort = port
  , Conn.connectionUseSecure =
      case host of
        Plain  _ -> Nothing
        Secure _ -> Just Conn.TLSSettingsSimple
          { Conn.settingDisableCertificateValidation = False
          , Conn.settingDisableSession = False
          , Conn.settingUseServerName = False
          }
        Insecure _ -> Just Conn.TLSSettingsSimple
          { Conn.settingDisableCertificateValidation = True
          , Conn.settingDisableSession = False
          , Conn.settingUseServerName = False
          }
  , Conn.connectionUseSocks = Nothing
  }

input :: FromAsn1 a => TQueue a -> Connection -> IO b
input inq conn = wrap . flip fix [] $ \loop chunks -> do
  chunk <- Conn.connectionGet conn 8192
  case ByteString.length chunk of
    0 -> throwIO (IO.mkIOError IO.eofErrorType "Ldap.Client.input" Nothing Nothing)
    _ -> do
      let chunks' = chunk : chunks
      case Asn1.decodeASN1 Asn1.DER (ByteString.Lazy.fromChunks (reverse chunks')) of
        Left  Asn1.ParsingPartial
                   -> loop chunks'
        Left  e    -> throwIO e
        Right asn1 -> do
          flip fix asn1 $ \loop' asn1' ->
            case parseAsn1 asn1' of
              Nothing -> return ()
              Just (asn1'', a) -> do
                atomically (writeTQueue inq a)
                loop' asn1''
          loop []

output :: ToAsn1 a => TQueue a -> Connection -> IO b
output out conn = wrap . forever $ do
  msg <- atomically (readTQueue out)
  Conn.connectionPut conn (ByteString.Lazy.toStrict (encode (toAsn1 mempty msg)))

dispatch
  :: TQueue ClientMessage
  -> TQueue (Type.LdapMessage Type.ProtocolServerOp)
  -> TQueue (Type.LdapMessage Request)
  -> IO a
dispatch client inq outq =
  flip fix Map.empty $ \loop !req ->
    loop =<< atomically (asum
      [ do New mid new var <- readTQueue client
           writeTQueue outq (Type.LdapMessage mid new Nothing)
           return (Map.insert mid ([], var) req)
      , do Type.LdapMessage mid op _
               <- readTQueue inq
           res <- case op of
             Type.BindResponse {}          -> done mid op req
             Type.SearchResultEntry {}     -> saveUp mid op req
             Type.SearchResultReference {} -> return req
             Type.SearchResultDone {}      -> done mid op req
             Type.ModifyResponse {}        -> done mid op req
             Type.AddResponse {}           -> done mid op req
             Type.DeleteResponse {}        -> done mid op req
             Type.ModifyDnResponse {}      -> done mid op req
             Type.CompareResponse {}       -> done mid op req
             Type.ExtendedResponse {}      -> probablyDisconnect mid op req
             Type.IntermediateResponse {}  -> saveUp mid op req
           return res
      ])
 where
  saveUp mid op res =
    return (Map.adjust (\(stack, var) -> (op : stack, var)) mid res)

  done mid op req =
    case Map.lookup mid req of
      Nothing -> return req
      Just (stack, var) -> do
        putTMVar var (op :| stack)
        return (Map.delete mid req)

  probablyDisconnect (Type.Id 0)
                     (Type.ExtendedResponse
                       (Type.LdapResult code
                                        (Type.LdapDn (Type.LdapString dn))
                                        (Type.LdapString reason)
                                        _)
                       moid _)
                     req =
    case moid of
      Just (Type.LdapOid oid)
        | oid == noticeOfDisconnection -> throwSTM (Disconnect code (Dn dn) reason)
      _ -> return req
  probablyDisconnect mid op req = done mid op req

  noticeOfDisconnection :: Text
  noticeOfDisconnection = fromString "1.3.6.1.4.1.1466.20036"

wrap :: IO a -> IO a
wrap m = m `catch` (throwIO . WrappedIOError)
