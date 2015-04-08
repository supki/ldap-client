{-# LANGUAGE OverloadedStrings #-}
-- | <https://tools.ietf.org/html/rfc4511#section-4.12 Extended> operation.
--
-- This operation comes in four flavours:
--
--   * synchronous, exception throwing ('extended')
--
--   * synchronous, returning 'Either' 'ResponseError' @()@ ('extendedEither')
--
--   * asynchronous, 'IO' based ('extendedAsync')
--
--   * asynchronous, 'STM' based ('extendedAsyncSTM')
--
-- Of those, the first one ('extended') is probably the most useful for the typical usecase.
module Ldap.Client.Extended
  ( extended
  , extendedEither
  , extendedAsync
  , extendedAsyncSTM
  , startTls
  , startTlsEither
  , startTlsAsync
  , startTlsAsyncSTM
  ) where

import           Control.Monad ((<=<))
import           Control.Monad.STM (STM, atomically)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty((:|)))

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


-- | Perform the Extended operation synchronously. Raises 'ResponseError' on failures.
extended :: Ldap -> Oid -> Maybe ByteString -> IO ()
extended l oid mv =
  raise =<< extendedEither l oid mv

-- | Perform the Extended operation synchronously. Returns @Left e@ where
-- @e@ is a 'ResponseError' on failures.
extendedEither :: Ldap -> Oid -> Maybe ByteString -> IO (Either ResponseError ())
extendedEither l oid mv =
  wait =<< extendedAsync l oid mv

-- | Perform the Extended operation asynchronously. Call 'Ldap.Client.wait' to wait
-- for its completion.
extendedAsync :: Ldap -> Oid -> Maybe ByteString -> IO (Async ())
extendedAsync l oid mv =
  atomically (extendedAsyncSTM l oid mv)

-- | Perform the Extended operation asynchronously.
--
-- Don't wait for its completion (with 'Ldap.Client.waitSTM') in the
-- same transaction you've performed it in.
extendedAsyncSTM :: Ldap -> Oid -> Maybe ByteString -> STM (Async ())
extendedAsyncSTM l oid mv =
  let req = extendedRequest oid mv in sendRequest l (extendedResult req) req

extendedRequest :: Oid -> Maybe ByteString -> Request
extendedRequest (Oid oid) =
  Type.ExtendedRequest (Type.LdapOid oid)

extendedResult :: Request -> Response -> Either ResponseError ()
extendedResult req (Type.ExtendedResponse (Type.LdapResult code (Type.LdapDn (Type.LdapString dn))
                                                                (Type.LdapString msg) _) _ _ :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (ResponseErrorCode req code (Dn dn) msg)
extendedResult req res = Left (ResponseInvalid req res)


startTls :: Ldap -> IO ()
startTls =
  raise <=< startTlsEither

startTlsEither :: Ldap -> IO (Either ResponseError ())
startTlsEither =
  wait <=< startTlsAsync

startTlsAsync :: Ldap -> IO (Async ())
startTlsAsync =
  atomically . startTlsAsyncSTM

startTlsAsyncSTM :: Ldap -> STM (Async ())
startTlsAsyncSTM l =
  extendedAsyncSTM l (Oid "1.3.6.1.4.1.1466.20037") Nothing
