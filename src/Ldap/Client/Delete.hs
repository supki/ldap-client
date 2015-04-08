-- | <https://tools.ietf.org/html/rfc4511#section-4.8 Delete> operation.
--
-- This operation comes in four flavours:
--
--   * synchronous, exception throwing ('delete')
--
--   * synchronous, returning 'Either' 'ResponseError' @()@ ('deleteEither')
--
--   * asynchronous, 'IO' based ('deleteAsync')
--
--   * asynchronous, 'STM' based ('deleteAsyncSTM')
--
-- Of those, the first one ('delete') is probably the most useful for the typical usecase.
module Ldap.Client.Delete
  ( delete
  , deleteEither
  , deleteAsync
  , deleteAsyncSTM
  ) where

import           Control.Concurrent.STM (STM, atomically)
import           Data.List.NonEmpty (NonEmpty((:|)))

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


-- | Perform the Delete operation synchronously. Raises 'ResponseError' on failures.
delete :: Ldap -> Dn -> IO ()
delete l dn =
  raise =<< deleteEither l dn

-- | Perform the Delete operation synchronously. Returns @Left e@ where
-- @e@ is a 'ResponseError' on failures.
deleteEither :: Ldap -> Dn -> IO (Either ResponseError ())
deleteEither l dn =
  wait =<< deleteAsync l dn

-- | Perform the Delete operation asynchronously. Call 'Ldap.Client.wait' to wait
-- for its completion.
deleteAsync :: Ldap -> Dn -> IO (Async ())
deleteAsync l dn =
  atomically (deleteAsyncSTM l dn)

-- | Perform the Delete operation asynchronously.
--
-- Don't wait for its completion (with 'Ldap.Client.waitSTM') in the
-- same transaction you've performed it in.
deleteAsyncSTM :: Ldap -> Dn -> STM (Async ())
deleteAsyncSTM l dn =
  let req = deleteRequest dn in sendRequest l (deleteResult req) req

deleteRequest :: Dn -> Request
deleteRequest (Dn dn) =
  Type.DeleteRequest (Type.LdapDn (Type.LdapString dn))

deleteResult :: Request -> Response -> Either ResponseError ()
deleteResult req (Type.DeleteResponse (Type.LdapResult code (Type.LdapDn (Type.LdapString dn))
                                                            (Type.LdapString msg) _) :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (ResponseErrorCode req code (Dn dn) msg)
deleteResult req res = Left (ResponseInvalid req res)
