module Ldap.Client.Modify
  ( Operation(..)
  , modify
  , modifyEither
  , modifyAsync
  , modifyAsyncSTM
  ) where

import           Control.Monad.STM (STM, atomically)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty((:|)))

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


data Operation =
    Delete Attr [ByteString]
  | Add Attr [ByteString]
  | Replace Attr [ByteString]
    deriving (Show, Eq)

modify :: Ldap -> Dn -> [Operation] -> IO ()
modify l dn as =
  raise =<< modifyEither l dn as

modifyEither :: Ldap -> Dn -> [Operation] -> IO (Either ResponseError ())
modifyEither l dn as =
  wait =<< modifyAsync l dn as

modifyAsync :: Ldap -> Dn -> [Operation] -> IO (Async ())
modifyAsync l dn as =
  atomically (modifyAsyncSTM l dn as)

modifyAsyncSTM :: Ldap -> Dn -> [Operation] -> STM (Async ())
modifyAsyncSTM l dn xs =
  let req = modifyRequest dn xs in sendRequest l (modifyResult req) req

modifyRequest :: Dn -> [Operation] -> Request
modifyRequest (Dn dn) xs =
  Type.ModifyRequest (Type.LdapDn (Type.LdapString dn)) (map f xs)
 where
  f (Delete (Attr k) vs) =
    (Type.Delete, Type.PartialAttribute (Type.AttributeDescription (Type.LdapString k))
                                        (map Type.AttributeValue vs))
  f (Add (Attr k) vs) =
    (Type.Add, Type.PartialAttribute (Type.AttributeDescription (Type.LdapString k))
                                     (map Type.AttributeValue vs))
  f (Replace (Attr k) vs) =
    (Type.Replace, Type.PartialAttribute (Type.AttributeDescription (Type.LdapString k))
                                         (map Type.AttributeValue vs))

modifyResult :: Request -> Response -> Either ResponseError ()
modifyResult req (Type.ModifyResponse (Type.LdapResult code (Type.LdapDn (Type.LdapString dn)) (Type.LdapString msg) _) :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (ResponseErrorCode req code (Dn dn) msg)
modifyResult req res = Left (ResponseInvalid req res)
