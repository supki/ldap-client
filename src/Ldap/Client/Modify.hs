-- | <https://tools.ietf.org/html/rfc4511#section-4.6 Modify> and
-- <https://tools.ietf.org/html/rfc4511#section-4.9 Modify DN> operations.
--
-- These operations come in four flavours:
--
--   * synchronous, exception throwing ('modify' / 'modifyDn')
--
--   * synchronous, returning 'Either' 'ResponseError' @()@
--     ('modifyEither' / 'modifyDnEither')
--
--   * asynchronous, 'IO' based ('modifyAsync' / 'modifyDnAsync')
--
--   * asynchronous, 'STM' based ('modifyAsyncSTM' / 'modifyDnAsyncSTM')
--
-- Of those, the first one ('modify' / 'modifyDn') is probably the most
-- useful for the typical usecase.
module Ldap.Client.Modify
  ( Operation(..)
  , modify
  , modifyEither
  , modifyAsync
  , modifyAsyncSTM
  , RelativeDn(..)
  , modifyDn
  , modifyDnEither
  , modifyDnAsync
  , modifyDnAsyncSTM
  , Async
  , wait
  , waitSTM
  ) where

import           Control.Monad.STM (STM, atomically)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Text (Text)

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


-- | Type of modification being performed.
data Operation =
    Delete !Attr ![AttrValue]  -- ^ Delete values from the attribute. Deletes the attribute if the list is empty or all current values are listed.
  | Add !Attr ![AttrValue]     -- ^ Add values to the attribute, creating it if necessary.
  | Replace !Attr ![AttrValue] -- ^ Replace all existing values of the attribute with the new list. Deletes the attribute if the list is empty.
    deriving (Show, Eq)

-- | Perform the Modify operation synchronously. Raises 'ResponseError' on failures.
modify :: Ldap -> Dn -> [Operation] -> IO ()
modify l dn as =
  eitherToIO =<< modifyEither l dn as

-- | Perform the Modify operation synchronously. Returns @Left e@ where
-- @e@ is a 'ResponseError' on failures.
modifyEither :: Ldap -> Dn -> [Operation] -> IO (Either ResponseError ())
modifyEither l dn as =
  wait =<< modifyAsync l dn as

-- | Perform the Modify operation asynchronously. Call 'Ldap.Client.wait' to wait
-- for its completion.
modifyAsync :: Ldap -> Dn -> [Operation] -> IO (Async ())
modifyAsync l dn as =
  atomically (modifyAsyncSTM l dn as)

-- | Perform the Modify operation asynchronously.
--
-- Don't wait for its completion (with 'Ldap.Client.waitSTM') in the
-- same transaction you've performed it in.
modifyAsyncSTM :: Ldap -> Dn -> [Operation] -> STM (Async ())
modifyAsyncSTM l dn xs =
  let req = modifyRequest dn xs in sendRequest l (modifyResult req) req

modifyRequest :: Dn -> [Operation] -> Request
modifyRequest (Dn dn) xs =
  Type.ModifyRequest (Type.LdapDn (Type.LdapString dn)) (map f xs)
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

modifyResult :: Request -> Response -> Either ResponseError ()
modifyResult req (Type.ModifyResponse (Type.LdapResult code (Type.LdapDn (Type.LdapString dn)) (Type.LdapString msg) _) :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (ResponseErrorCode req code (Dn dn) msg)
modifyResult req res = Left (ResponseInvalid req res)


-- | A component of 'Dn'.
newtype RelativeDn = RelativeDn Text
    deriving (Show, Eq)

-- | Perform the Modify DN operation synchronously. Raises 'ResponseError' on failures.
modifyDn :: Ldap -> Dn -> RelativeDn -> Bool -> Maybe Dn -> IO ()
modifyDn l dn rdn del new =
  eitherToIO =<< modifyDnEither l dn rdn del new

-- | Perform the Modify DN operation synchronously. Returns @Left e@ where
-- @e@ is a 'ResponseError' on failures.
modifyDnEither :: Ldap -> Dn -> RelativeDn -> Bool -> Maybe Dn -> IO (Either ResponseError ())
modifyDnEither l dn rdn del new =
  wait =<< modifyDnAsync l dn rdn del new

-- | Perform the Modify DN operation asynchronously. Call 'Ldap.Client.wait' to wait
-- for its completion.
modifyDnAsync :: Ldap -> Dn -> RelativeDn -> Bool -> Maybe Dn -> IO (Async ())
modifyDnAsync l dn rdn del new =
  atomically (modifyDnAsyncSTM l dn rdn del new)

-- | Perform the Modify DN operation asynchronously.
--
-- Don't wait for its completion (with 'Ldap.Client.waitSTM') in the
-- same transaction you've performed it in.
modifyDnAsyncSTM :: Ldap -> Dn -> RelativeDn -> Bool -> Maybe Dn -> STM (Async ())
modifyDnAsyncSTM l dn rdn del new =
  let req = modifyDnRequest dn rdn del new in sendRequest l (modifyDnResult req) req

modifyDnRequest :: Dn -> RelativeDn -> Bool -> Maybe Dn -> Request
modifyDnRequest (Dn dn) (RelativeDn rdn) del new =
  Type.ModifyDnRequest (Type.LdapDn (Type.LdapString dn))
                       (Type.RelativeLdapDn (Type.LdapString rdn))
                       del
                       (fmap (\(Dn dn') -> Type.LdapDn (Type.LdapString dn')) new)

modifyDnResult :: Request -> Response -> Either ResponseError ()
modifyDnResult req (Type.ModifyDnResponse (Type.LdapResult code (Type.LdapDn (Type.LdapString dn)) (Type.LdapString msg) _) :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (ResponseErrorCode req code (Dn dn) msg)
modifyDnResult req res = Left (ResponseInvalid req res)
