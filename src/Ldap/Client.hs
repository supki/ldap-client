{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
module Ldap.Client
  ( Host(..)
  , PortNumber
  , Ldap
  , LdapError(..)
  , Type.ResultCode(..)
  , Async
  , with
    -- * Bind Request
  , Dn(..)
  , Password(..)
  , BindError(..)
  , bind
    -- * Search Request
  , Attr(..)
  , SearchError(..)
  , search
  , Search
  , scope
  , Type.Scope(..)
  , size
  , time
  , typesOnly
  , derefAliases
  , Filter(..)
  , SearchEntry(..)
    -- * Add Request
  , AttrList
  , AddError(..)
  , add
  , addEither
  , addAsync
  , addAsyncSTM
    -- * Delete Request
  , DeleteError(..)
  , delete
  , deleteEither
  , deleteAsync
  , deleteAsyncSTM
    -- * Waiting for Request Completion
  , wait
  , waitSTM
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar (putTMVar)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import           Control.Exception (Handler(..), bracket, throwIO, catches)
import           Control.Monad (forever)
import qualified Data.ASN1.BinaryEncoding as Asn1
import qualified Data.ASN1.Encoding as Asn1
import qualified Data.ASN1.Error as Asn1
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           Data.Foldable (traverse_, asum)
import           Data.Function (fix)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as Map
import           Data.Monoid (Endo(appEndo))
import           Network.Connection (Connection)
import qualified Network.Connection as Conn
import qualified System.IO.Error as IO

import           Ldap.Asn1.ToAsn1 (ToAsn1(toAsn1))
import           Ldap.Asn1.FromAsn1 (FromAsn1, parseAsn1)
import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal
import           Ldap.Client.Bind (BindError(..), bind, unbindAsync)
import           Ldap.Client.Search
  ( SearchError(..)
  , search
  , Search
  , scope
  , size
  , time
  , typesOnly
  , derefAliases
  , Filter(..)
  , SearchEntry(..)
  )


newLdap :: IO Ldap
newLdap = Ldap
  <$> newTQueueIO

data LdapError =
    IOError IOError
  | ParseError Asn1.ASN1Error
  | BindError BindError
  | SearchError SearchError
  | AddError AddError
  | DeleteError DeleteError
    deriving (Show, Eq)

-- | The entrypoint into LDAP.
with :: Host -> PortNumber -> (Ldap -> IO a) -> IO (Either LdapError a)
with host port f = do
  context <- Conn.initConnectionContext
  bracket (Conn.connectTo context params) Conn.connectionClose (\conn ->
    bracket newLdap unbindAsync (\l -> do
      inq  <- newTQueueIO
      outq <- newTQueueIO
      Async.withAsync (input inq conn) $ \i ->
        Async.withAsync (output outq conn) $ \o ->
          Async.withAsync (dispatch l inq outq) $ \d ->
            Async.withAsync (f l) $ \u ->
              fmap (Right . snd) (Async.waitAnyCancel [i, o, d, u])))
 `catches`
  [ Handler (return . Left . IOError)
  , Handler (return . Left . ParseError)
  , Handler (return . Left . BindError)
  , Handler (return . Left . SearchError)
  , Handler (return . Left . AddError)
  , Handler (return . Left . DeleteError)
  ]
 where
  params = Conn.ConnectionParams
    { Conn.connectionHostname =
        case host of
          Plain  h -> h
          Secure h -> h
    , Conn.connectionPort = port
    , Conn.connectionUseSecure =
        case host of
          Plain  _ -> Nothing
          Secure _ -> Just Conn.TLSSettingsSimple
            { Conn.settingDisableCertificateValidation = False
            , Conn.settingDisableSession = False
            , Conn.settingUseServerName = False
            }
    , Conn.connectionUseSocks = Nothing
    }

input :: FromAsn1 a => TQueue a -> Connection -> IO b
input inq conn = flip fix [] $ \loop chunks -> do
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
output out conn = forever $ do
  msg <- atomically (readTQueue out)
  Conn.connectionPut conn (encode (toAsn1 msg))
 where
  encode x = Asn1.encodeASN1' Asn1.DER (appEndo x [])

dispatch
  :: Ldap
  -> TQueue (Type.LdapMessage Type.ProtocolServerOp)
  -> TQueue (Type.LdapMessage Request)
  -> IO a
dispatch Ldap { client } inq outq =
  flip fix (Map.empty, Map.empty, 1) $ \loop (!got, !results, !counter) -> do
    loop =<< atomically (asum
      [ do New new var <- readTQueue client
           writeTQueue outq (Type.LdapMessage (Type.Id counter) new Nothing)
           return (got, Map.insert (Type.Id counter) var results, counter + 1)
      , do Type.LdapMessage mid op _ <- readTQueue inq
           case op of
             Type.BindResponse {} -> do
               traverse_ (\var -> putTMVar var (op :| [])) (Map.lookup mid results)
               return (Map.delete mid got, Map.delete mid results, counter)
             Type.SearchResultEntry {} -> do
               return (Map.insertWith (++) mid [op] got, results, counter)
             Type.SearchResultReference {} -> do
               return (got, results, counter)
             Type.SearchResultDone {} -> do
               let stack = Map.findWithDefault [] mid got
               traverse_ (\var -> putTMVar var (op :| stack)) (Map.lookup mid results)
               return (Map.delete mid got, Map.delete mid results, counter)
             Type.AddResponse {} -> do
               traverse_ (\var -> putTMVar var (op :| [])) (Map.lookup mid results)
               return (Map.delete mid got, Map.delete mid results, counter)
             Type.DeleteResponse {} -> do
               traverse_ (\var -> putTMVar var (op :| [])) (Map.lookup mid results)
               return (Map.delete mid got, Map.delete mid results, counter)
      ])
