module Ldap.Client.Compare
  ( compare
  , compareEither
  , compareAsync
  , compareAsyncSTM
  ) where

import           Control.Monad.STM (STM, atomically)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Prelude hiding (compare)

import           Ldap.Client.Internal
import qualified Ldap.Asn1.Type as Type


compare :: Ldap -> Dn -> Attr -> ByteString -> IO Bool
compare l dn k v =
  raise =<< compareEither l dn k v

compareEither :: Ldap -> Dn -> Attr -> ByteString -> IO (Either ResponseError Bool)
compareEither l dn k v =
  wait =<< compareAsync l dn k v

compareAsync :: Ldap -> Dn -> Attr -> ByteString -> IO (Async Bool)
compareAsync l dn k v =
  atomically (compareAsyncSTM l dn k v)

compareAsyncSTM :: Ldap -> Dn -> Attr -> ByteString -> STM (Async Bool)
compareAsyncSTM l dn k v =
  let req = compareRequest dn k v in sendRequest l (compareResult req) req

compareRequest :: Dn -> Attr -> ByteString -> Request
compareRequest (Dn dn) (Attr k) v =
  Type.CompareRequest (Type.LdapDn (Type.LdapString dn))
                      (Type.AttributeValueAssertion
                        (Type.AttributeDescription (Type.LdapString k))
                        (Type.AssertionValue v))

compareResult :: Request -> Response -> Either ResponseError Bool
compareResult req (Type.CompareResponse (Type.LdapResult code (Type.LdapDn (Type.LdapString dn))
                                                              (Type.LdapString msg) _) :| [])
  | Type.CompareTrue  <- code = Right True
  | Type.CompareFalse <- code = Right False
  | otherwise = Left (ResponseErrorCode req code (Dn dn) msg)
compareResult req res = Left (ResponseInvalid req res)
