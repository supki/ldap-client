module Ldap.Client.Bind
  ( BindError(..)
  , bind
  , bindEither
  , bindAsync
  , bindAsyncSTM
  , unbindAsync
  , unbindAsyncSTM
  ) where

import           Control.Exception (Exception)
import           Control.Monad (void)
import           Control.Monad.STM (STM, atomically)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Typeable (Typeable)

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


data BindError =
    BindInvalidResponse Response
  | BindErrorCode Type.ResultCode
    deriving (Show, Eq, Typeable)

instance Exception BindError

-- | Throws 'BindError' on failure. Don't worry, the nearest 'with'
-- will catch it, so it won't destroy your program.
bind :: Ldap -> Dn -> Password -> IO ()
bind l username password =
  raise =<< bindEither l username password

bindEither :: Ldap -> Dn -> Password -> IO (Either BindError ())
bindEither l username password =
  wait =<< bindAsync l username password

bindAsync :: Ldap -> Dn -> Password -> IO (Async BindError ())
bindAsync l username password =
  atomically (bindAsyncSTM l username password)

bindAsyncSTM :: Ldap -> Dn -> Password -> STM (Async BindError ())
bindAsyncSTM l username password =
  sendRequest l bindResult (bindRequest username password)

bindRequest :: Dn -> Password -> Request
bindRequest (Dn username) (Password password) =
  Type.BindRequest ldapVersion
                   (Type.LdapDn (Type.LdapString username))
                   (Type.Simple password)
 where
  ldapVersion = 3

bindResult :: Response -> Either BindError ()
bindResult (Type.BindResponse (Type.LdapResult code _ _ _) _ :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (BindErrorCode code)
bindResult res = Left (BindInvalidResponse res)


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
