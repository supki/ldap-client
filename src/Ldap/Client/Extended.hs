{-# LANGUAGE OverloadedStrings #-}
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


extended :: Ldap -> Oid -> Maybe ByteString -> IO ()
extended l oid mv =
  raise =<< extendedEither l oid mv

extendedEither :: Ldap -> Oid -> Maybe ByteString -> IO (Either ResponseError ())
extendedEither l oid mv =
  wait =<< extendedAsync l oid mv

extendedAsync :: Ldap -> Oid -> Maybe ByteString -> IO (Async ())
extendedAsync l oid mv =
  atomically (extendedAsyncSTM l oid mv)

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
