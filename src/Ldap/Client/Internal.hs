{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
module Ldap.Client.Internal
  ( Host(..)
  , PortNumber
  , Ldap(..)
  , ClientMessage(..)
  , Type.ResultCode(..)
  , Async
  , AttrList
    -- * Waiting for Request Completion
  , wait
  , waitSTM
    -- * Misc
  , Response
  , ResponseError(..)
  , Request
  , raise
  , sendRequest
  , Dn(..)
  , Attr(..)
  , AttrValue
  , unAttr
    -- * Unbind operation
  , unbindAsync
  , unbindAsyncSTM
  ) where

import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, readTMVar)
import           Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import           Control.Exception (Exception, throwIO)
import           Control.Monad (void)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Network (PortNumber)
import           Network.Connection (TLSSettings)

import qualified Ldap.Asn1.Type as Type


-- | LDAP host.
data Host =
    Plain String    -- ^ Plain LDAP. Do not use!
  | Insecure String -- ^ LDAP over TLS without the certificate validity check.
                    --   Only use for testing!
  | Secure String   -- ^ LDAP over TLS. Use!
    | SecureWithTLSSettings String TLSSettings -- ^ LDAP over TLS with the ability to specify detailed TLS settings
    deriving (Show)

-- | A token. All functions that interact with the Directory require one.
data Ldap = Ldap
  { client  :: TQueue ClientMessage
  } deriving (Eq)

data ClientMessage = New Request (TMVar (NonEmpty Type.ProtocolServerOp))
type Request = Type.ProtocolClientOp
type InMessage = Type.ProtocolServerOp
type Response = NonEmpty InMessage

-- | Asynchronous LDAP operation. Use 'wait' or 'waitSTM' to wait for its completion.
data Async a = Async (STM (Either ResponseError a))

instance Functor Async where
  fmap f (Async stm) = Async (fmap (fmap f) stm)

-- | Unique identifier of an LDAP entry.
newtype Dn = Dn Text
    deriving (Show, Eq)

-- | Response indicates a failed operation.
data ResponseError =
    ResponseInvalid Request Response -- ^ LDAP server did not follow the protocol, so @ldap-client@ couldn't make sense of the response.
  | ResponseErrorCode Request Type.ResultCode Dn Text -- ^ The response contains a result code indicating failure and an error message.
    deriving (Show, Eq, Typeable)

instance Exception ResponseError

-- | Attribute name.
newtype Attr = Attr Text
    deriving (Show, Eq)

-- | Attribute value.
type AttrValue = ByteString

-- | List of attributes and their values. @f@ is the structure these
-- values are in, e.g. 'NonEmpty'.
type AttrList f = [(Attr, f AttrValue)]

-- 'Attr' unwrapper. This is a separate function not to turn 'Attr''s
-- 'Show' instance into complete and utter shit.
unAttr :: Attr -> Text
unAttr (Attr a) = a

-- | Wait for operation completion.
wait :: Async a -> IO (Either ResponseError a)
wait = atomically . waitSTM

-- | Wait for operation completion inside 'STM'.
--
-- Do not use this inside the same 'STM' transaction the operation was
-- requested in! To give LDAP the chance to respond to it that transaction
-- should commit. After that, applying 'waitSTM' to the corresponding 'Async'
-- starts to make sense.
waitSTM :: Async a -> STM (Either ResponseError a)
waitSTM (Async stm) = stm

sendRequest :: Ldap -> (Response -> Either ResponseError a) -> Request -> STM (Async a)
sendRequest l p msg =
  do var <- newEmptyTMVar
     writeRequest l var msg
     return (Async (fmap p (readTMVar var)))

writeRequest :: Ldap -> TMVar Response -> Request -> STM ()
writeRequest Ldap { client } var msg = writeTQueue client (New msg var)

raise :: Exception e => Either e a -> IO a
raise = either throwIO return


-- | Terminate the connection to the Directory.
--
-- Note that 'unbindAsync' does not return an 'Async',
-- because LDAP server never responds to @UnbindRequest@s, hence
-- a call to 'wait' on a hypothetical 'Async' would have resulted
-- in an exception anyway.
unbindAsync :: Ldap -> IO ()
unbindAsync =
  atomically . unbindAsyncSTM

-- | Terminate the connection to the Directory.
--
-- Note that 'unbindAsyncSTM' does not return an 'Async',
-- because LDAP server never responds to @UnbindRequest@s, hence
-- a call to 'wait' on a hypothetical 'Async' would have resulted
-- in an exception anyway.
unbindAsyncSTM :: Ldap -> STM ()
unbindAsyncSTM l =
  void (sendRequest l die Type.UnbindRequest)
 where
  die = error "Ldap.Client: do not wait for the response to UnbindRequest"
