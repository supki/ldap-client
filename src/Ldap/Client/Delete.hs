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


delete :: Ldap -> Dn -> IO ()
delete l dn =
  raise =<< deleteEither l dn

deleteEither :: Ldap -> Dn -> IO (Either ResponseError ())
deleteEither l dn =
  wait =<< deleteAsync l dn

deleteAsync :: Ldap -> Dn -> IO (Async ())
deleteAsync l dn =
  atomically (deleteAsyncSTM l dn)

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
