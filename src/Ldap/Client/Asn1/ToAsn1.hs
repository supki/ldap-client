-- | This module contains convertions from LDAP types to ASN.1.
module Ldap.Client.Asn1.ToAsn1
  ( ToAsn1(toAsn1)
  ) where

import           Data.Bool (Bool(False))
import           Data.Foldable (foldMap)
import           Data.Eq (Eq((==)))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Monoid (Monoid(mempty), (<>))
import qualified Data.Text.Encoding as Text
import           Prelude (fromIntegral)

import           Ldap.Asn1.Type
import           Ldap.Asn1.ToAsn1


-- | Convert a LDAP type to ASN.1.
--
-- When it's relevant, instances include the part of the RFC describing the encoding.
class ToAsn1 a where
  toAsn1 :: Mod -> a -> Ber

{- |
@
LDAPMessage ::= SEQUENCE {
     messageID       MessageID,
     protocolOp      CHOICE {
          bindRequest           BindRequest,
          bindResponse          BindResponse,
          unbindRequest         UnbindRequest,
          searchRequest         SearchRequest,
          searchResEntry        SearchResultEntry,
          searchResDone         SearchResultDone,
          searchResRef          SearchResultReference,
          addRequest            AddRequest,
          addResponse           AddResponse,
          ... },
     controls       [0] Controls OPTIONAL }
@
-}
instance ToAsn1 op => ToAsn1 (LdapMessage op) where
  toAsn1 m (LdapMessage i op mc) =
    sequence m
      (toAsn1 mempty i <>
       toAsn1 mempty op <>
       foldMap (toAsn1 (context <> tag 0)) mc)

{- |
@
MessageID ::= INTEGER (0 ..  maxInt)
@
-}
instance ToAsn1 Id where
  toAsn1 m (Id i) = int32 m i

{- |
@
LDAPString ::= OCTET STRING -- UTF-8 encoded
@
-}
instance ToAsn1 LdapString where
  toAsn1 m (LdapString s) = octetstring m (Text.encodeUtf8 s)

{- |
@
LDAPOID ::= OCTET STRING -- Constrained to \<numericoid\>
@
-}
instance ToAsn1 LdapOid where
  toAsn1 m (LdapOid s) = octetstring m (Text.encodeUtf8 s)

{- |
@
LDAPDN ::= LDAPString -- Constrained to \<distinguishedName\>
@
-}
instance ToAsn1 LdapDn where
  toAsn1 m (LdapDn s) = toAsn1 m s

{- |
@
RelativeLDAPDN ::= LDAPString -- Constrained to \<name-component\>
@
-}
instance ToAsn1 RelativeLdapDn where
  toAsn1 m (RelativeLdapDn s) = toAsn1 m s

{- |
@
AttributeDescription ::= LDAPString
@
-}
instance ToAsn1 AttributeDescription where
  toAsn1 m (AttributeDescription s) = toAsn1 m s

{- |
@
AttributeValue ::= OCTET STRING
@
-}
instance ToAsn1 AttributeValue where
  toAsn1 m (AttributeValue s) = octetstring m s

{- |
@
AttributeValueAssertion ::= SEQUENCE {
     attributeDesc   AttributeDescription,
     assertionValue  AssertionValue }
@
-}
instance ToAsn1 AttributeValueAssertion where
  toAsn1 m (AttributeValueAssertion d v) =
    sequence m (toAsn1 mempty d <> toAsn1 mempty v)

{- |
@
AssertionValue ::= OCTET STRING
@
-}
instance ToAsn1 AssertionValue where
  toAsn1 m (AssertionValue s) = octetstring m s


{- |
@
PartialAttribute ::= SEQUENCE {
     type       AttributeDescription,
     vals       SET OF value AttributeValue }
@
-}
instance ToAsn1 PartialAttribute where
  toAsn1 m (PartialAttribute d xs) =
    sequence m (toAsn1 mempty d <> set mempty (toAsn1 mempty xs))

{- |
@
Attribute ::= PartialAttribute(WITH COMPONENTS {
     ...,
     vals (SIZE(1..MAX))})
@
-}
instance ToAsn1 Attribute where
  toAsn1 m (Attribute d xs) =
    sequence m (toAsn1 mempty d <> set mempty (toAsn1 mempty xs))

{- |
@
MatchingRuleId ::= LDAPString
@
-}
instance ToAsn1 MatchingRuleId where
  toAsn1 m (MatchingRuleId s) = toAsn1 m s

{- |
@
Controls ::= SEQUENCE OF control Control
@
-}
instance ToAsn1 Controls where
  toAsn1 m (Controls cs) = sequence m (toAsn1 mempty cs)

{- |
@
Control ::= SEQUENCE {
     controlType             LDAPOID,
     criticality             BOOLEAN DEFAULT FALSE,
     controlValue            OCTET STRING OPTIONAL }
@
-}
instance ToAsn1 Control where
  toAsn1 m (Control t c v) =
    sequence m
      (toAsn1 mempty t <>
       default_ False c (bool mempty c) <>
       foldMap (octetstring mempty) v)

{- |
@
BindRequest ::= [APPLICATION 0] SEQUENCE {
     version                 INTEGER (1 ..  127),
     name                    LDAPDN,
     authentication          AuthenticationChoice }
@

@
UnbindRequest ::= [APPLICATION 2] NULL
@

@
SearchRequest ::= [APPLICATION 3] SEQUENCE {
     baseObject      LDAPDN,
     scope           ENUMERATED {
          baseObject              (0),
          singleLevel             (1),
          wholeSubtree            (2),
          ...  },
     derefAliases    ENUMERATED {
          neverDerefAliases       (0),
          derefInSearching        (1),
          derefFindingBaseObj     (2),
          derefAlways             (3) },
     sizeLimit       INTEGER (0 ..  maxInt),
     timeLimit       INTEGER (0 ..  maxInt),
     typesOnly       BOOLEAN,
     filter          Filter,
     attributes      AttributeSelection }
@

@
ModifyRequest ::= [APPLICATION 6] SEQUENCE {
     object          LDAPDN,
     changes         SEQUENCE OF change SEQUENCE {
          operation       ENUMERATED {
               add     (0),
               delete  (1),
               replace (2),
               ...  },
          modification    PartialAttribute } }
@

@
AddRequest ::= [APPLICATION 8] SEQUENCE {
     entry           LDAPDN,
     attributes      AttributeList }
@

@
DelRequest ::= [APPLICATION 10] LDAPDN
@

@
ModifyDNRequest ::= [APPLICATION 12] SEQUENCE {
     entry           LDAPDN,
     newrdn          RelativeLDAPDN,
     deleteoldrdn    BOOLEAN,
     newSuperior     [0] LDAPDN OPTIONAL }
@

@
CompareRequest ::= [APPLICATION 14] SEQUENCE {
     entry           LDAPDN,
     ava             AttributeValueAssertion }
@

@
AbandonRequest ::= [APPLICATION 16] MessageID
@

@
ExtendedRequest ::= [APPLICATION 23] SEQUENCE {
     requestName      [0] LDAPOID,
     requestValue     [1] OCTET STRING OPTIONAL }
@
-}
instance ToAsn1 ProtocolClientOp where
  toAsn1 _ (BindRequest v n a) =
    sequence (application <> tag 0)
      (int32 mempty (fromIntegral v) <>
       toAsn1 mempty n <>
       toAsn1 mempty a)
  toAsn1 _ UnbindRequest =
    null (application <> tag 2)
  toAsn1 _ (SearchRequest bo s da sl tl to f a) =
    sequence (application <> tag 3)
      (toAsn1 mempty bo <>
       enum mempty s' <>
       enum mempty da' <>
       int32 mempty sl <>
       int32 mempty tl <>
       bool mempty to <>
       toAsn1 mempty f <>
       toAsn1 mempty a)
   where
    s' = case s of
      BaseObject   -> 0
      SingleLevel  -> 1
      WholeSubtree -> 2
    da' = case da of
      NeverDerefAliases      -> 0
      DerefInSearching       -> 1
      DerefFindingBaseObject -> 2
      DerefAlways            -> 3
  toAsn1 _ (ModifyRequest dn xs) =
    sequence (application <> tag 6)
      (toAsn1 mempty dn <>
       sequence mempty (foldMap (\(op, pa) -> sequence mempty (enum mempty (case op of
         Add     -> 0
         Delete  -> 1
         Replace -> 2) <> toAsn1 mempty pa)) xs))
  toAsn1 _ (AddRequest dn as) =
    sequence (application <> tag 8) (toAsn1 mempty dn <> toAsn1 mempty as)
  toAsn1 _ (DeleteRequest dn) =
    toAsn1 (application <> tag 10) dn
  toAsn1 _ (ModifyDnRequest dn rdn del new) =
    sequence (application <> tag 12)
      (toAsn1 mempty dn <>
       toAsn1 mempty rdn <>
       bool mempty del <>
       foldMap (toAsn1 (context <> tag 0)) new)
  toAsn1 _ (CompareRequest dn av) =
    sequence (application <> tag 14) (toAsn1 mempty dn <> toAsn1 mempty av)
  toAsn1 _ (AbandonRequest i) =
    toAsn1 (application <> tag 16) i
  toAsn1 _ (ExtendedRequest oid mv) =
    sequence (application <> tag 23)
     (toAsn1 (context <> tag 0) oid <>
      foldMap (octetstring (context <> tag 1)) mv)

{- |
@
AuthenticationChoice ::= CHOICE {
     simple                  [0] OCTET STRING,
     ...  }
@
-}
instance ToAsn1 AuthenticationChoice where
  toAsn1 _ (Simple s) = octetstring (context <> tag 0) s

{- |
@
AttributeSelection ::= SEQUENCE OF selector LDAPString
@
-}
instance ToAsn1 AttributeSelection where
  toAsn1 m (AttributeSelection as) = sequence m (toAsn1 mempty as)

{- |
@
Filter ::= CHOICE {
     and             [0] SET SIZE (1..MAX) OF filter Filter,
     or              [1] SET SIZE (1..MAX) OF filter Filter,
     not             [2] Filter,
     equalityMatch   [3] AttributeValueAssertion,
     substrings      [4] SubstringFilter,
     greaterOrEqual  [5] AttributeValueAssertion,
     lessOrEqual     [6] AttributeValueAssertion,
     present         [7] AttributeDescription,
     approxMatch     [8] AttributeValueAssertion,
     extensibleMatch [9] MatchingRuleAssertion,
     ...  }
@
-}
instance ToAsn1 Filter where
  toAsn1 _ f = case f of
    And xs            -> set (context <> tag 0) (toAsn1 mempty xs)
    Or xs             -> set (context <> tag 1) (toAsn1 mempty xs)
    Not x             -> tagged (context <> tag 2) (toAsn1 mempty x)
    EqualityMatch x   -> toAsn1 (context <> tag 3) x
    Substrings x      -> toAsn1 (context <> tag 4) x
    GreaterOrEqual x  -> toAsn1 (context <> tag 5) x
    LessOrEqual x     -> toAsn1 (context <> tag 6) x
    Present x         -> toAsn1 (context <> tag 7) x
    ApproxMatch x     -> toAsn1 (context <> tag 8) x
    ExtensibleMatch x -> toAsn1 (context <> tag 9) x

{- |
@
SubstringFilter ::= SEQUENCE {
     type           AttributeDescription,
     substrings     SEQUENCE SIZE (1..MAX) OF substring CHOICE {
          initial [0] AssertionValue,  -- can occur at most once
          any     [1] AssertionValue,
          final   [2] AssertionValue } -- can occur at most once
     }
@
-}
instance ToAsn1 SubstringFilter where
  toAsn1 m (SubstringFilter ad ss) =
    sequence m
      (toAsn1 mempty ad <>
       sequence mempty (foldMap (\s -> case s of
         Initial v -> toAsn1 (context <> tag 0) v
         Any v     -> toAsn1 (context <> tag 1) v
         Final v   -> toAsn1 (context <> tag 2) v) ss))

{- |
@
MatchingRuleAssertion ::= SEQUENCE {
     matchingRule    [1] MatchingRuleId OPTIONAL,
     type            [2] AttributeDescription OPTIONAL,
     matchValue      [3] AssertionValue,
     dnAttributes    [4] BOOLEAN DEFAULT FALSE }
@
-}
instance ToAsn1 MatchingRuleAssertion where
  toAsn1 m (MatchingRuleAssertion mmr mad av b) = sequence m
    (foldMap (toAsn1 (context <> tag 1)) mmr <>
     foldMap (toAsn1 (context <> tag 2)) mad <>
     toAsn1 (context <> tag 3) av <>
     default_ False b (bool (context <> tag 4) b))

{- |
@
AttributeList ::= SEQUENCE OF attribute Attribute
@
-}
instance ToAsn1 AttributeList where
  toAsn1 m (AttributeList xs) = sequence m (toAsn1 mempty xs)

instance ToAsn1 a => ToAsn1 [a] where
  toAsn1 _ = foldMap (toAsn1 mempty)

instance ToAsn1 a => ToAsn1 (NonEmpty a) where
  toAsn1 _ = foldMap (toAsn1 mempty)

default_ :: (Eq a, Monoid m) => a -> a -> m -> m
default_ a b c = if a == b then mempty else c
