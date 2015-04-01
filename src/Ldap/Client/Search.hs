{-# LANGUAGE NamedFieldPuns #-}
module Ldap.Client.Search
  ( SearchError(..)
  , search
  , searchEither
  , searchAsync
  , searchAsyncSTM
  , Search
  , Type.Scope(..)
  , scope
  , size
  , time
  , typesOnly
  , derefAliases
  , Filter(..)
  , SearchEntry(..)
  ) where

import           Control.Exception (Exception)
import           Control.Monad.STM (STM, atomically)
import           Data.ByteString (ByteString)
import           Data.Int (Int32)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


data SearchError =
    SearchInvalidResponse Response
  | SearchErrorCode Type.ResultCode
    deriving (Show, Eq, Typeable)

instance Exception SearchError

search
  :: Ldap
  -> Dn
  -> Mod Search
  -> Filter
  -> [Attr]
  -> IO [SearchEntry]
search l base opts flt attributes =
  raise =<< searchEither l base opts flt attributes

searchEither
  :: Ldap
  -> Dn
  -> Mod Search
  -> Filter
  -> [Attr]
  -> IO (Either SearchError [SearchEntry])
searchEither l base opts flt attributes =
  wait =<< searchAsync l base opts flt attributes

searchAsync
  :: Ldap
  -> Dn
  -> Mod Search
  -> Filter
  -> [Attr]
  -> IO (Async SearchError [SearchEntry])
searchAsync l base opts flt attributes =
  atomically (searchAsyncSTM l base opts flt attributes)

searchAsyncSTM
  :: Ldap
  -> Dn
  -> Mod Search
  -> Filter
  -> [Attr]
  -> STM (Async SearchError [SearchEntry])
searchAsyncSTM l base opts flt attributes =
  sendRequest l searchResult (searchRequest base opts flt attributes)

searchResult :: Response -> Either SearchError [SearchEntry]
searchResult (Type.SearchResultDone (Type.LdapResult code _ _ _) :| xs)
  | Type.Success <- code = Right (mapMaybe g xs)
  | Type.AdminLimitExceeded <- code = Right (mapMaybe g xs)
  | Type.SizeLimitExceeded <- code = Right (mapMaybe g xs)
  | otherwise = Left (SearchErrorCode code)
 where
  g (Type.SearchResultEntry (Type.LdapDn (Type.LdapString dn))
                            (Type.PartialAttributeList ys)) =
    Just (SearchEntry (Dn dn) (map h ys))
  g _ = Nothing
  h (Type.PartialAttribute (Type.AttributeDescription (Type.LdapString x))
                           y) = (Attr x, Set.map j y)
  j (Type.AttributeValue x) = x
searchResult res = Left (SearchInvalidResponse res)

searchRequest :: Dn -> Mod Search -> Filter -> [Attr] -> Request
searchRequest (Dn base) (Mod m) flt attributes =
  Type.SearchRequest (Type.LdapDn (Type.LdapString base))
                     _scope
                     _derefAliases
                     _size
                     _time
                     _typesOnly
                     (fromFilter flt)
                     (Type.AttributeSelection (map (Type.LdapString . unAttr) attributes))
 where
  Search { _scope, _derefAliases, _size, _time, _typesOnly } =
    m defaultSearch
  fromFilter (Not x) = Type.Not (fromFilter x)
  fromFilter (And xs) = Type.And (fmap fromFilter xs)
  fromFilter (Or xs) = Type.Or (fmap fromFilter xs)
  fromFilter (Present (Attr x)) =
    Type.Present (Type.AttributeDescription (Type.LdapString x))
  fromFilter (Attr x := y) =
    Type.EqualityMatch
      (Type.AttributeValueAssertion (Type.AttributeDescription (Type.LdapString x))
                                    (Type.AssertionValue y))
  fromFilter (Attr x :>= y) =
    Type.GreaterOrEqual
      (Type.AttributeValueAssertion (Type.AttributeDescription (Type.LdapString x))
                                    (Type.AssertionValue y))
  fromFilter (Attr x :<= y) =
    Type.LessOrEqual
      (Type.AttributeValueAssertion (Type.AttributeDescription (Type.LdapString x))
                                    (Type.AssertionValue y))
  fromFilter (Attr x :~= y) =
    Type.ApproxMatch
      (Type.AttributeValueAssertion (Type.AttributeDescription (Type.LdapString x))
                                    (Type.AssertionValue y))
  fromFilter (Attr x :=* (mi, xs, mf)) =
    Type.Substrings
      (Type.SubstringFilter (Type.AttributeDescription (Type.LdapString x))
                            (NonEmpty.fromList (concat
                              [ maybe [] (\i -> [Type.Initial (Type.AssertionValue i)]) mi
                              , fmap (Type.Any . Type.AssertionValue) xs
                              , maybe [] (\f -> [Type.Final (Type.AssertionValue f)]) mf
                              ])))
  fromFilter ((mx, mr, b) ::= y) =
    Type.ExtensibleMatch
      (Type.MatchingRuleAssertion (fmap (\(Attr r) -> Type.MatchingRuleId (Type.LdapString r)) mr)
                                  (fmap (\(Attr x) -> Type.AttributeDescription (Type.LdapString x)) mx)
                                  (Type.AssertionValue y)
                                  b)

data Search = Search
  { _scope        :: Type.Scope
  , _derefAliases :: Type.DerefAliases
  , _size         :: Int32
  , _time         :: Int32
  , _typesOnly    :: Bool
  } deriving (Show, Eq)

defaultSearch :: Search
defaultSearch = Search
  { _scope        = Type.BaseObject
  , _size         = 0
  , _time         = 0
  , _typesOnly    = False
  , _derefAliases = Type.NeverDerefAliases
  }

scope :: Type.Scope -> Mod Search
scope x = Mod (\y -> y { _scope = x })

size :: Int32 -> Mod Search
size x = Mod (\y -> y { _size = x })

time :: Int32 -> Mod Search
time x = Mod (\y -> y { _time = x })

typesOnly :: Bool -> Mod Search
typesOnly x = Mod (\y -> y { _typesOnly = x })

derefAliases :: Type.DerefAliases -> Mod Search
derefAliases x = Mod (\y -> y { _derefAliases = x })

newtype Mod a = Mod (a -> a)

instance Monoid (Mod a) where
  mempty = Mod id
  Mod f `mappend` Mod g = Mod (g . f)

data Filter =
    Not Filter
  | And (NonEmpty Filter)
  | Or (NonEmpty Filter)
  | Present Attr
  | Attr := ByteString
  | Attr :>= ByteString
  | Attr :<= ByteString
  | Attr :~= ByteString
  | Attr :=* (Maybe ByteString, [ByteString], Maybe ByteString)
  | (Maybe Attr, Maybe Attr, Bool) ::= ByteString

data SearchEntry = SearchEntry Dn (AttrList Set)
    deriving (Show, Eq)
