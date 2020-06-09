-- | <https://tools.ietf.org/html/rfc4511#section-4.10 Compare> operation.
--
-- This operation comes in four flavours:
--
--   * synchronous, exception throwing ('compare')
--
--   * synchronous, returning 'Either' 'ResponseError' @()@ ('compareEither')
--
--   * asynchronous, 'IO' based ('compareAsync')
--
--   * asynchronous, 'STM' based ('compareAsyncSTM')
--
-- Of those, the first one ('compare') is probably the most useful for the
-- typical usecase.
module Ldap.Client.Compare
  ( compare
  , compareEither
  , compareAsync
  , compareAsyncSTM
  , Async
  , wait
  , waitSTM
  ) where

import           Control.Monad.STM (STM, atomically)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Prelude hiding (compare)

import           Ldap.Client.Internal
import qualified Ldap.Asn1.Type as Type


-- | Perform the Compare operation synchronously. Raises 'ResponseError' on failures.
compare :: Ldap -> Dn -> Attr -> AttrValue -> IO Bool
compare l dn k v =
  eitherToIO =<< compareEither l dn k v

-- | Perform the Compare operation synchronously. Returns @Left e@ where
-- @e@ is a 'ResponseError' on failures.
compareEither :: Ldap -> Dn -> Attr -> AttrValue -> IO (Either ResponseError Bool)
compareEither l dn k v =
  wait =<< compareAsync l dn k v

-- | Perform the Compare operation asynchronously. Call 'Ldap.Client.wait' to wait
-- for its completion.
compareAsync :: Ldap -> Dn -> Attr -> AttrValue -> IO (Async Bool)
compareAsync l dn k v =
  atomically (compareAsyncSTM l dn k v)

-- | Perform the Compare operation asynchronously.
--
-- Don't wait for its completion (with 'Ldap.Client.waitSTM') in the
-- same transaction you've performed it in.
compareAsyncSTM :: Ldap -> Dn -> Attr -> AttrValue -> STM (Async Bool)
compareAsyncSTM l dn k v =
  let req = compareRequest dn k v in sendRequest l (compareResult req) req

compareRequest :: Dn -> Attr -> AttrValue -> Request
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
