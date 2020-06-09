-- | <https://tools.ietf.org/html/rfc4511#section-4.7 Add> operation.
--
-- This operation comes in four flavours:
--
--   * synchronous, exception throwing ('add')
--
--   * synchronous, returning 'Either' 'ResponseError' @()@ ('addEither')
--
--   * asynchronous, 'IO' based ('addAsync')
--
--   * asynchronous, 'STM' based ('addAsyncSTM')
--
-- Of those, the first one ('add') is probably the most useful for the typical usecase.
module Ldap.Client.Add
  ( add
  , addEither
  , addAsync
  , addAsyncSTM
  , Async
  , wait
  , waitSTM
  ) where

import           Control.Monad.STM (STM, atomically)
import           Data.List.NonEmpty (NonEmpty((:|)))

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


-- | Perform the Add operation synchronously. Raises 'ResponseError' on failures.
add :: Ldap -> Dn -> AttrList NonEmpty -> IO ()
add l dn as =
  eitherToIO =<< addEither l dn as

-- | Perform the Add operation synchronously. Returns @Left e@ where
-- @e@ is a 'ResponseError' on failures.
addEither :: Ldap -> Dn -> AttrList NonEmpty -> IO (Either ResponseError ())
addEither l dn as =
  wait =<< addAsync l dn as

-- | Perform the Add operation asynchronously. Call 'Ldap.Client.wait' to wait
-- for its completion.
addAsync :: Ldap -> Dn -> AttrList NonEmpty -> IO (Async ())
addAsync l dn as =
  atomically (addAsyncSTM l dn as)

-- | Perform the Add operation asynchronously.
--
-- Don't wait for its completion (with 'Ldap.Client.waitSTM') in the
-- same transaction you've performed it in.
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
