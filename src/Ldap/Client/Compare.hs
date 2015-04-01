module Ldap.Client.Compare
  ( CompareError(..)
  , compare
  , compareEither
  , compareAsync
  , compareAsyncSTM
  ) where

import           Control.Exception (Exception)
import           Control.Monad.STM (STM, atomically)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Typeable (Typeable)
import           Prelude hiding (compare)

import           Ldap.Client.Internal
import qualified Ldap.Asn1.Type as Type


data CompareError =
    CompareInvalidResponse Response
  | CompareErrorCode Type.ResultCode
    deriving (Show, Eq, Typeable)

instance Exception CompareError

compare :: Ldap -> Dn -> Attr -> ByteString -> IO Bool
compare l dn k v =
  raise =<< compareEither l dn k v

compareEither :: Ldap -> Dn -> Attr -> ByteString -> IO (Either CompareError Bool)
compareEither l dn k v =
  wait =<< compareAsync l dn k v

compareAsync :: Ldap -> Dn -> Attr -> ByteString -> IO (Async CompareError Bool)
compareAsync l dn k v =
  atomically (compareAsyncSTM l dn k v)

compareAsyncSTM :: Ldap -> Dn -> Attr -> ByteString -> STM (Async CompareError Bool)
compareAsyncSTM l dn k v =
  sendRequest l compareResult (compareRequest dn k v)

compareRequest :: Dn -> Attr -> ByteString -> Request
compareRequest (Dn dn) (Attr k) v =
  Type.CompareRequest (Type.LdapDn (Type.LdapString dn))
                      (Type.AttributeValueAssertion
                        (Type.AttributeDescription (Type.LdapString k))
                        (Type.AssertionValue v))

compareResult :: Response -> Either CompareError Bool
compareResult (Type.CompareResponse (Type.LdapResult code _ _ _) :| [])
  | Type.CompareTrue  <- code = Right True
  | Type.CompareFalse <- code = Right False
  | otherwise = Left (CompareErrorCode code)
compareResult res = Left (CompareInvalidResponse res)
