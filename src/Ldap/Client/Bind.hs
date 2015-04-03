module Ldap.Client.Bind
  ( bind
  , bindEither
  , bindAsync
  , bindAsyncSTM
  , unbindAsync
  , unbindAsyncSTM
  ) where

import           Control.Monad (void)
import           Control.Monad.STM (STM, atomically)
import           Data.List.NonEmpty (NonEmpty((:|)))

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


-- | Throws 'BindError' on failure. Don't worry, the nearest 'with'
-- will catch it, so it won't destroy your program.
bind :: Ldap -> Dn -> Password -> IO ()
bind l username password =
  raise =<< bindEither l username password

bindEither :: Ldap -> Dn -> Password -> IO (Either ResponseError ())
bindEither l username password =
  wait =<< bindAsync l username password

bindAsync :: Ldap -> Dn -> Password -> IO (Async ())
bindAsync l username password =
  atomically (bindAsyncSTM l username password)

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


-- | Note that 'unbindAsync' does not return an 'Async',
-- because LDAP server never responds to @UnbindRequest@s, hence
-- a call to 'wait' on a hypothetical 'Async' would have resulted
-- in an exception anyway.
unbindAsync :: Ldap -> IO ()
unbindAsync =
  atomically . unbindAsyncSTM

-- | Note that 'unbindAsyncSTM' does not return an 'Async',
-- because LDAP server never responds to @UnbindRequest@s, hence
-- a call to 'wait' on a hypothetical 'Async' would have resulted
-- in an exception anyway.
unbindAsyncSTM :: Ldap -> STM ()
unbindAsyncSTM l =
  void (sendRequest l die Type.UnbindRequest)
 where
  die = error "Ldap.Client: do not wait for the response to UnbindRequest"
