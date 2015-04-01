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
    -- * Misc
  , Response
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
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Network (PortNumber)

import qualified Ldap.Asn1.Type as Type


data Host =
    Plain String
  | Secure String
    deriving (Show, Eq, Ord)

data Ldap = Ldap
  { client  :: TQueue ClientMessage
  } deriving (Eq)

data ClientMessage = New Request (TMVar (NonEmpty Type.ProtocolServerOp))
type Request = Type.ProtocolClientOp
type InMessage = Type.ProtocolServerOp
type Response = NonEmpty InMessage

data Async e a = Async (STM (Either e a))

instance Functor (Async e) where
  fmap f (Async stm) = Async (fmap (fmap f) stm)


newtype Dn = Dn Text
    deriving (Show, Eq)
newtype Password = Password ByteString
    deriving (Show, Eq)




newtype Attr = Attr Text
    deriving (Show, Eq)

type AttrList f = [(Attr, f ByteString)]

data AddError =
    AddInvalidResponse Response
  | AddErrorCode Type.ResultCode
    deriving (Show, Eq, Typeable)

instance Exception AddError

add :: Ldap -> Dn -> AttrList NonEmpty -> IO ()
add l dn as =
  raise =<< addEither l dn as

addEither :: Ldap -> Dn -> AttrList NonEmpty -> IO (Either AddError ())
addEither l dn as =
  wait =<< addAsync l dn as

addAsync :: Ldap -> Dn -> AttrList NonEmpty -> IO (Async AddError ())
addAsync l dn as =
  atomically (addAsyncSTM l dn as)

addAsyncSTM :: Ldap -> Dn -> AttrList NonEmpty -> STM (Async AddError ())
addAsyncSTM l (Dn dn) as =
  sendRequest l addResult
                (Type.AddRequest (Type.LdapDn (Type.LdapString dn))
                                 (Type.AttributeList (map f as)))
 where
  f (Attr x, xs) = Type.Attribute (Type.AttributeDescription (Type.LdapString x))
                                  (fmap Type.AttributeValue xs)

addResult :: Response -> Either AddError ()
addResult (Type.AddResponse (Type.LdapResult code _ _ _) :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (AddErrorCode code)
addResult res = Left (AddInvalidResponse res)

-- 'Attr' unwrapper. This is a separate function not to turn 'Attr''s
-- 'Show' instance into complete and utter shit.
unAttr :: Attr -> Text
unAttr (Attr a) = a


data DeleteError =
    DeleteInvalidResponse Response
  | DeleteErrorCode Type.ResultCode
    deriving (Show, Eq, Typeable)

instance Exception DeleteError

delete :: Ldap -> Dn -> IO ()
delete l dn =
  raise =<< deleteEither l dn

deleteEither :: Ldap -> Dn -> IO (Either DeleteError ())
deleteEither l dn =
  wait =<< deleteAsync l dn

deleteAsync :: Ldap -> Dn -> IO (Async DeleteError ())
deleteAsync l dn =
  atomically (deleteAsyncSTM l dn)

deleteAsyncSTM :: Ldap -> Dn -> STM (Async DeleteError ())
deleteAsyncSTM l (Dn dn) =
  sendRequest l deleteResult
                (Type.DeleteRequest (Type.LdapDn (Type.LdapString dn)))

deleteResult :: Response -> Either DeleteError ()
deleteResult (Type.DeleteResponse (Type.LdapResult code _ _ _) :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (DeleteErrorCode code)
deleteResult res = Left (DeleteInvalidResponse res)


wait :: Async e a -> IO (Either e a)
wait = atomically . waitSTM

waitSTM :: Async e a -> STM (Either e a)
waitSTM (Async stm) = stm


sendRequest :: Ldap -> (Response -> Either e a) -> Request -> STM (Async e a)
sendRequest l p msg =
  do var <- newEmptyTMVar
     writeRequest l var msg
     return (Async (fmap p (readTMVar var)))

writeRequest :: Ldap -> TMVar Response -> Request -> STM ()
writeRequest Ldap { client } var msg = writeTQueue client (New msg var)

raise :: Exception e => Either e a -> IO a
raise = either throwIO return
