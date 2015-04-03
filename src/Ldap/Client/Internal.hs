{-# LANGUAGE BangPatterns #-}
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
  , Password(..)
  , Attr(..)
  , unAttr
  ) where

import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, readTMVar)
import           Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import           Control.Exception (Exception, throwIO)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Network (PortNumber)

import qualified Ldap.Asn1.Type as Type


data Host =
    Plain String
  | Secure String
  | Insecure String
    deriving (Show, Eq, Ord)

data Ldap = Ldap
  { client  :: TQueue ClientMessage
  } deriving (Eq)

data ClientMessage = New Request (TMVar (NonEmpty Type.ProtocolServerOp))
type Request = Type.ProtocolClientOp
type InMessage = Type.ProtocolServerOp
type Response = NonEmpty InMessage

data Async a = Async (STM (Either ResponseError a))

instance Functor Async where
  fmap f (Async stm) = Async (fmap (fmap f) stm)


newtype Dn = Dn Text
    deriving (Show, Eq)
newtype Password = Password ByteString
    deriving (Show, Eq)


data ResponseError =
    ResponseInvalid Request Response
  | ResponseErrorCode Request Type.ResultCode Dn Text
    deriving (Show, Eq)

instance Exception ResponseError



newtype Attr = Attr Text
    deriving (Show, Eq)

type AttrList f = [(Attr, f ByteString)]

-- 'Attr' unwrapper. This is a separate function not to turn 'Attr''s
-- 'Show' instance into complete and utter shit.
unAttr :: Attr -> Text
unAttr (Attr a) = a


wait :: Async a -> IO (Either ResponseError a)
wait = atomically . waitSTM

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
