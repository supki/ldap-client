module Ldap.Client.Modify
  ( ModifyError(..)
  , Operation(..)
  , modify
  , modifyEither
  , modifyAsync
  , modifyAsyncSTM
  ) where

import           Control.Exception (Exception)
import           Control.Monad.STM (STM, atomically)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Text (Text)
import           Data.Typeable (Typeable)

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


data ModifyError =
    ModifyInvalidResponse Response
  | ModifyErrorCode Type.ResultCode Dn Text
    deriving (Show, Eq, Typeable)

data Operation =
    Delete Attr [ByteString]
  | Add Attr [ByteString]
  | Replace Attr [ByteString]
    deriving (Show, Eq)


instance Exception ModifyError

modify :: Ldap -> Dn -> [Operation] -> IO ()
modify l dn as =
  raise =<< modifyEither l dn as

modifyEither :: Ldap -> Dn -> [Operation] -> IO (Either ModifyError ())
modifyEither l dn as =
  wait =<< modifyAsync l dn as

modifyAsync :: Ldap -> Dn -> [Operation] -> IO (Async ModifyError ())
modifyAsync l dn as =
  atomically (modifyAsyncSTM l dn as)

modifyAsyncSTM :: Ldap -> Dn -> [Operation] -> STM (Async ModifyError ())
modifyAsyncSTM l (Dn dn) xs =
  sendRequest l modifyResult
                (Type.ModifyRequest (Type.LdapDn (Type.LdapString dn)) (map f xs))
 where
  f (Delete (Attr k) vs) =
    (Type.Delete, Type.PartialAttribute (Type.AttributeDescription (Type.LdapString k))
                                        (map Type.AttributeValue vs))
  f (Add (Attr k) vs) =
    (Type.Add, Type.PartialAttribute (Type.AttributeDescription (Type.LdapString k))
                                     (map Type.AttributeValue vs))
  f (Replace (Attr k) vs) =
    (Type.Replace, Type.PartialAttribute (Type.AttributeDescription (Type.LdapString k))
                                         (map Type.AttributeValue vs))

modifyResult :: Response -> Either ModifyError ()
modifyResult (Type.ModifyResponse (Type.LdapResult code (Type.LdapDn (Type.LdapString dn)) (Type.LdapString msg) _) :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (ModifyErrorCode code (Dn dn) msg)
modifyResult res = Left (ModifyInvalidResponse res)
