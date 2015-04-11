-- | <https://tools.ietf.org/html/rfc4511#section-4.2 Bind> operation.
--
-- This operation comes in four flavours:
--
--   * synchronous, exception throwing ('bind')
--
--   * synchronous, returning 'Either' 'ResponseError' @()@ ('bindEither')
--
--   * asynchronous, 'IO' based ('bindAsync')
--
--   * asynchronous, 'STM' based ('bindAsyncSTM')
--
-- Of those, the first one ('bind') is probably the most useful for the typical usecase.
module Ldap.Client.Bind
  ( Password(..)
  , bind
  , bindEither
  , bindAsync
  , bindAsyncSTM
  , Async
  , wait
  , waitSTM
  ) where

import           Control.Monad.STM (STM, atomically)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty((:|)))

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


-- | User's password.
newtype Password = Password ByteString
    deriving (Show, Eq)

-- | Perform the Bind operation synchronously. Raises 'ResponseError' on failures.
bind :: Ldap -> Dn -> Password -> IO ()
bind l username password =
  raise =<< bindEither l username password

-- | Perform the Bind operation synchronously. Returns @Left e@ where
-- @e@ is a 'ResponseError' on failures.
bindEither :: Ldap -> Dn -> Password -> IO (Either ResponseError ())
bindEither l username password =
  wait =<< bindAsync l username password

-- | Perform the Bind operation asynchronously. Call 'Ldap.Client.wait' to wait
-- for its completion.
bindAsync :: Ldap -> Dn -> Password -> IO (Async ())
bindAsync l username password =
  atomically (bindAsyncSTM l username password)

-- | Perform the Bind operation asynchronously.
--
-- Don't wait for its completion (with 'Ldap.Client.waitSTM') in the
-- same transaction you've performed it in.
bindAsyncSTM :: Ldap -> Dn -> Password -> STM (Async ())
bindAsyncSTM l username password =
  let req = bindRequest username password in sendRequest l (bindResult req) req

bindRequest :: Dn -> Password -> Request
bindRequest (Dn username) (Password password) =
  Type.BindRequest ldapVersion
                   (Type.LdapDn (Type.LdapString username))
                   (Type.Simple password)
 where
  ldapVersion = 3

bindResult :: Request -> Response -> Either ResponseError ()
bindResult req (Type.BindResponse (Type.LdapResult code (Type.LdapDn (Type.LdapString dn))
                                                        (Type.LdapString msg) _) _ :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (ResponseErrorCode req code (Dn dn) msg)
bindResult req res = Left (ResponseInvalid req res)
