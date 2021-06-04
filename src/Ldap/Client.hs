{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module is intended to be imported qualified
--
-- @
-- import qualified Ldap.Client as Ldap
-- @
module Ldap.Client
  ( with
  , with'
  , runsIn
  , runsInEither
  , open
  , openFromConnection
  , close
  , Host(..)
  , defaultTlsSettings
  , insecureTlsSettings
  , PortNumber
  , Ldap
  , LdapH
  , LdapError(..)
  , ResponseError(..)
  , Type.ResultCode(..)
    -- * Bind
  , Password(..)
  , bind
  , externalBind
    -- * Search
  , search
  , SearchEntry(..)
    -- ** Search modifiers
  , Search
  , Mod
  , Type.Scope(..)
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
import           Control.Applicative ((<$>))
#endif
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (atomically, throwSTM)
import           Control.Concurrent.STM.TMVar (putTMVar)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import           Control.Exception (Exception, bracket, throwIO, SomeException, fromException, throw, Handler(..))
import           Control.Monad (forever)
import           Data.Void (Void)
import qualified Data.ASN1.BinaryEncoding as Asn1
import qualified Data.ASN1.Encoding as Asn1
import qualified Data.ASN1.Error as Asn1
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           Data.Foldable (asum)
import           Data.Function (fix)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as Map
import           Data.Monoid (Endo(appEndo))
import           Data.Text (Text)
#if __GLASGOW_HASKELL__ < 710
import           Data.Traversable (traverse)
#endif
import           Data.Typeable (Typeable)
import           Network.Connection (Connection)
import qualified Network.Connection as Conn
import           Prelude hiding (compare)
import qualified System.IO.Error as IO

import           Ldap.Asn1.ToAsn1 (ToAsn1(toAsn1))
import           Ldap.Asn1.FromAsn1 (FromAsn1, parseAsn1)
import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal
import           Ldap.Client.Bind (Password(..), bind, externalBind)
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
import           Ldap.Client.Extended (Oid(..), extended, noticeOfDisconnectionOid)

{-# ANN module ("HLint: ignore Use first" :: String) #-}


-- | Various failures that can happen when working with LDAP.
data LdapError
  = IOError !IOError             -- ^ Network failure.
  | ParseError !Asn1.ASN1Error   -- ^ Invalid ASN.1 data received from the server.
  | ResponseError !ResponseError -- ^ An LDAP operation failed.
  | DisconnectError !Disconnect  -- ^ Notice of Disconnection has been received.
    deriving (Show, Eq)

instance Exception LdapError

data Disconnect = Disconnect !Type.ResultCode !Dn !Text
    deriving (Show, Eq, Typeable)

instance Exception Disconnect

newtype LdapH = LdapH Ldap

-- | Provide a 'LdapH' to a function needing an 'Ldap' handle.
runsIn :: (Ldap -> IO a)
       -> LdapH
       -> IO a
runsIn act (LdapH ldap) = do
  actor <- Async.async (act ldap)
  r <- Async.waitEitherCatch (workers ldap) actor
  case r of
    Left (Right _a) -> error "Unreachable"
    Left (Left e)   -> throwIO =<< catchesHandler workerErr e
    Right (Right r') -> pure r'
    Right (Left e)  -> throwIO =<< catchesHandler respErr e

-- | Provide a 'LdapH' to a function needing an 'Ldap' handle
runsInEither :: (Ldap -> IO a)
             -> LdapH
             -> IO (Either LdapError a)
runsInEither act (LdapH ldap) = do
  actor <- Async.async (act ldap)
  r <- Async.waitEitherCatch (workers ldap) actor
  case r of
    Left (Right _a) -> error "Unreachable"
    Left (Left e)   -> do Left <$> catchesHandler workerErr e
    Right (Right r') -> pure (Right r')
    Right (Left e)  -> do Left <$> catchesHandler respErr e


workerErr :: [Handler LdapError]
workerErr = [ Handler (\(ex :: IOError) -> pure (IOError ex))
            , Handler (\(ex :: Asn1.ASN1Error) -> pure (ParseError ex))
            , Handler (\(ex :: Disconnect) -> pure (DisconnectError ex))
            ]

respErr :: [Handler LdapError]
respErr = [ Handler (\(ex :: ResponseError) -> pure (ResponseError ex))
          ]

catchesHandler :: [Handler a] -> SomeException -> IO a
catchesHandler handlers e = foldr tryHandler (throw e) handlers
    where tryHandler (Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res

-- | The entrypoint into LDAP.
with' :: Host -> PortNumber -> (Ldap -> IO a) -> IO a
with' host port act = bracket (open host port) close (runsIn act)

with :: Host -> PortNumber -> (Ldap -> IO a) -> IO (Either LdapError a)
with host port act = bracket (open host port) close (runsInEither act)

-- | Creates an LDAP handle. This action is useful for creating your own resource
-- management, such as with 'resource-pool'. The handle must be manually closed
-- with 'close'.
open :: Host -> PortNumber -> IO (LdapH)
open host port = do
  context <- Conn.initConnectionContext
  conn <- Conn.connectTo context params
  openFromConnection conn
 where
  params = Conn.ConnectionParams
    { Conn.connectionHostname =
        case host of
          Plain h -> h
          Tls   h _ -> h
    , Conn.connectionPort = port
    , Conn.connectionUseSecure =
        case host of
          Plain  _ -> Nothing
          Tls _ settings -> pure settings
    , Conn.connectionUseSocks = Nothing
    }

openFromConnection :: Connection -> IO (LdapH)
openFromConnection conn = do
  reqQ <- newTQueueIO
  inQ  <- newTQueueIO
  outQ <- newTQueueIO

  -- The input worker that reads data off the network.
  (inW :: Async.Async Void)   <- Async.async (input inQ conn)

  -- The output worker that sends data onto the network.
  (outW :: Async.Async Void)  <- Async.async (output outQ conn)

  -- The dispatch worker that sends data between the three queues.
  (dispW :: Async.Async Void) <- Async.async (dispatch reqQ inQ outQ)

  -- We use this to propagate exceptions between the workers. The `workers` Async is just a tool to
  -- exchange exceptions between the entire worker group and another thread.
  workers <- Async.async (snd <$> Async.waitAnyCancel [inW, outW, dispW])

  pure (LdapH (Ldap reqQ workers conn))

-- | Closes an LDAP connection.
-- This is to be used in together with 'open'.
close :: LdapH -> IO ()
close (LdapH ldap) = do
  unbindAsync ldap
  Conn.connectionClose (conn ldap)
  Async.cancel (workers ldap)

defaultTlsSettings :: Conn.TLSSettings
defaultTlsSettings = Conn.TLSSettingsSimple
  { Conn.settingDisableCertificateValidation = False
  , Conn.settingDisableSession = False
  , Conn.settingUseServerName = False
  }

insecureTlsSettings :: Conn.TLSSettings
insecureTlsSettings = Conn.TLSSettingsSimple
  { Conn.settingDisableCertificateValidation = True
  , Conn.settingDisableSession = False
  , Conn.settingUseServerName = False
  }

-- | Reads Asn1 BER encoded chunks off a connection into a TQueue.
input :: FromAsn1 a => TQueue a -> Connection -> IO b
input inq conn = loop []
  where
    loop chunks = do
      chunk <- Conn.connectionGet conn 8192
      case ByteString.length chunk of
        0 -> throwIO (IO.mkIOError IO.eofErrorType "Ldap.Client.input" Nothing Nothing)
        _ -> do
          let chunks' = chunk : chunks
          case Asn1.decodeASN1 Asn1.BER (ByteString.Lazy.fromChunks (reverse chunks')) of
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

-- | Transmits Asn1 DER encoded data from a TQueue into a Connection.
output :: ToAsn1 a => TQueue a -> Connection -> IO b
output out conn = forever $ do
  msg <- atomically (readTQueue out)
  Conn.connectionPut conn (encode (toAsn1 msg))
 where
  encode x = Asn1.encodeASN1' Asn1.DER (appEndo x [])

dispatch
  :: TQueue ClientMessage
  -> TQueue (Type.LdapMessage Type.ProtocolServerOp)
  -> TQueue (Type.LdapMessage Request)
  -> IO a
dispatch reqq inq outq = loop (Map.empty, 1)
  where
    saveUp mid op res = return (Map.adjust (\(stack, var) -> (op : stack, var)) mid res)

    loop (!req, !counter) =
      loop =<< atomically (asum
        [ do New new var <- readTQueue reqq
             writeTQueue outq (Type.LdapMessage (Type.Id counter) new Nothing)
             return (Map.insert (Type.Id counter) ([], var) req, counter + 1)
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
             return (res, counter)
        ])

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
          | Oid oid == noticeOfDisconnectionOid -> throwSTM (Disconnect code (Dn dn) reason)
        _ -> return req
    probablyDisconnect mid op req = done mid op req
