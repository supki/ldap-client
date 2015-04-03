module Ldap.Client.Add
  ( add
  , addEither
  , addAsync
  , addAsyncSTM
  ) where

import           Control.Monad.STM (STM, atomically)
import           Data.List.NonEmpty (NonEmpty((:|)))

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


add :: Ldap -> Dn -> AttrList NonEmpty -> IO ()
add l dn as =
  raise =<< addEither l dn as

addEither :: Ldap -> Dn -> AttrList NonEmpty -> IO (Either ResponseError ())
addEither l dn as =
  wait =<< addAsync l dn as

addAsync :: Ldap -> Dn -> AttrList NonEmpty -> IO (Async ())
addAsync l dn as =
  atomically (addAsyncSTM l dn as)

addAsyncSTM :: Ldap -> Dn -> AttrList NonEmpty -> STM (Async ())
addAsyncSTM l dn as =
  let req = addRequest dn as in sendRequest l (addResult req) req

addRequest :: Dn -> AttrList NonEmpty -> Request
addRequest (Dn dn) as =
  Type.AddRequest (Type.LdapDn (Type.LdapString dn))
                  (Type.AttributeList (map f as))
 where
  f (Attr x, xs) = Type.Attribute (Type.AttributeDescription (Type.LdapString x))
                                  (fmap Type.AttributeValue xs)

addResult :: Request -> Response -> Either ResponseError ()
addResult req (Type.AddResponse (Type.LdapResult code (Type.LdapDn (Type.LdapString dn))
                                                      (Type.LdapString msg) _) :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (ResponseErrorCode req code (Dn dn) msg)
addResult req res = Left (ResponseInvalid req res)
