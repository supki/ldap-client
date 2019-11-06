{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | <https://tools.ietf.org/html/rfc4511#section-4.5 Search> operation.
--
-- This operation comes in four flavours:
--
--   * synchronous, exception throwing ('search')
--
--   * synchronous, returning 'Either' 'ResponseError' @()@ ('searchEither')
--
--   * asynchronous, 'IO' based ('searchAsync')
--
--   * asynchronous, 'STM' based ('searchAsyncSTM')
--
-- Of those, the first one ('search') is probably the most useful for the typical usecase.
module Ldap.Client.Search
  ( search
  , searchEither
  , searchAsync
  , searchAsyncSTM
  , Search
  , Mod
  , Type.Scope(..)
  , scope
  , size
  , time
  , typesOnly
  , Type.DerefAliases(..)
  , derefAliases
  , Filter(..)
  , SearchEntry(..)
  , Async
  , wait
  , waitSTM
  ) where

import           Control.Monad.STM (STM, atomically)
import           Data.Int (Int32)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
#if __GLASGOW_HASKELL__ >= 710
import           Data.Semigroup (Semigroup(..))
#else
import           Data.Semigroup (Semigroup(..), Monoid(..))
#endif

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


-- | Perform the Search operation synchronously. Raises 'ResponseError' on failures.
search :: Ldap -> Dn -> Mod Search -> Filter -> [Attr] -> IO [SearchEntry]
search l base opts flt attributes =
  eitherToIO =<< searchEither l base opts flt attributes

-- | Perform the Search operation synchronously. Returns @Left e@ where
-- @e@ is a 'ResponseError' on failures.
searchEither
  :: Ldap
  -> Dn
  -> Mod Search
  -> Filter
  -> [Attr]
  -> IO (Either ResponseError [SearchEntry])
searchEither l base opts flt attributes =
  wait =<< searchAsync l base opts flt attributes

-- | Perform the Search operation asynchronously. Call 'Ldap.Client.wait' to wait
-- for its completion.
searchAsync :: Ldap -> Dn -> Mod Search -> Filter -> [Attr] -> IO (Async [SearchEntry])
searchAsync l base opts flt attributes =
  atomically (searchAsyncSTM l base opts flt attributes)

-- | Perform the Search operation asynchronously.
--
-- Don't wait for its completion (with 'Ldap.Client.waitSTM') in the
-- same transaction you've performed it in.
searchAsyncSTM
  :: Ldap
  -> Dn
  -> Mod Search
  -> Filter
  -> [Attr]
  -> STM (Async [SearchEntry])
searchAsyncSTM l base opts flt attributes =
  let req = searchRequest base opts flt attributes in sendRequest l (searchResult req) req

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

searchResult :: Request -> Response -> Either ResponseError [SearchEntry]
searchResult req (Type.SearchResultDone (Type.LdapResult code (Type.LdapDn (Type.LdapString dn'))
                                                              (Type.LdapString msg) _) :| xs)
  | Type.Success <- code = Right (mapMaybe g xs)
  | Type.AdminLimitExceeded <- code = Right (mapMaybe g xs)
  | Type.SizeLimitExceeded <- code = Right (mapMaybe g xs)
  | otherwise = Left (ResponseErrorCode req code (Dn dn') msg)
 where
  g (Type.SearchResultEntry (Type.LdapDn (Type.LdapString dn))
                            (Type.PartialAttributeList ys)) =
    Just (SearchEntry (Dn dn) (map h ys))
  g _ = Nothing
  h (Type.PartialAttribute (Type.AttributeDescription (Type.LdapString x))
                           y) = (Attr x, fmap j y)
  j (Type.AttributeValue x) = x
searchResult req res = Left (ResponseInvalid req res)

-- | Search options. Use 'Mod' to change some of those.
data Search = Search
  { _scope        :: !Type.Scope
  , _derefAliases :: !Type.DerefAliases
  , _size         :: !Int32
  , _time         :: !Int32
  , _typesOnly    :: !Bool
  } deriving (Show, Eq)

defaultSearch :: Search
defaultSearch = Search
  { _scope        = Type.WholeSubtree
  , _size         = 0
  , _time         = 0
  , _typesOnly    = False
  , _derefAliases = Type.NeverDerefAliases
  }

-- | Scope of the search (default: 'WholeSubtree').
scope :: Type.Scope -> Mod Search
scope x = Mod (\y -> y { _scope = x })

-- | Maximum number of entries to be returned as a result of the Search.
-- No limit if the value is @0@ (default: @0@).
size :: Int32 -> Mod Search
size x = Mod (\y -> y { _size = x })

-- | Maximum time (in seconds) allowed for the Search. No limit if the value
-- is @0@ (default: @0@).
time :: Int32 -> Mod Search
time x = Mod (\y -> y { _time = x })

-- | Whether Search results are to contain just attribute descriptions, or
-- both attribute descriptions and values (default: 'False').
typesOnly :: Bool -> Mod Search
typesOnly x = Mod (\y -> y { _typesOnly = x })

-- | Alias dereference policy (default: 'NeverDerefAliases').
derefAliases :: Type.DerefAliases -> Mod Search
derefAliases x = Mod (\y -> y { _derefAliases = x })

-- | Search modifier. Combine using 'Semigroup' and/or 'Monoid' instance.
newtype Mod a = Mod (a -> a)

instance Semigroup (Mod a) where
  Mod f <> Mod g = Mod (g . f)

instance Monoid (Mod a) where
  mempty = Mod id
  mappend = (<>)

-- | Conditions that must be fulfilled in order for the Search to match a given entry.
data Filter =
    Not !Filter             -- ^ Filter does not match the entry
  | And !(NonEmpty Filter)  -- ^ All filters match the entry
  | Or !(NonEmpty Filter)   -- ^ Any filter matches the entry
  | Present !Attr           -- ^ Attribute is present in the entry
  | !Attr := !AttrValue     -- ^ Attribute's value is equal to the assertion
  | !Attr :>= !AttrValue    -- ^ Attribute's value is equal to or greater than the assertion
  | !Attr :<= !AttrValue    -- ^ Attribute's value is equal to or less than the assertion
  | !Attr :~= !AttrValue    -- ^ Attribute's value approximately matches the assertion
  | !Attr :=* !(Maybe AttrValue, [AttrValue], Maybe AttrValue)
                            -- ^ Glob match
  | !(Maybe Attr, Maybe Attr, Bool) ::= !AttrValue
                            -- ^ Extensible match

-- | Entry found during the Search.
data SearchEntry = SearchEntry !Dn !(AttrList [])
    deriving (Show, Eq)
