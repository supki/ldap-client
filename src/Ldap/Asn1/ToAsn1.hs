{-# LANGUAGE OverloadedStrings #-}
-- | This module contains convertions from LDAP types to ASN.1.
--
-- Various hacks are employed because "asn1-encoding" only encodes to DER, but
-- LDAP demands BER-encoding.  So, when a definition looks suspiciously different
-- from the spec in the comment, that's why.  I hope all that will be fixed
-- eventually.
module Ldap.Asn1.ToAsn1
  ( ToAsn1(toAsn1)
  ) where

import           Data.ASN1.Types (ASN1, ASN1Class, ASN1Tag, ASN1ConstructionType)
import qualified Data.ASN1.Types as Asn1
import           Data.ByteString (ByteString)
import           Data.Foldable (fold, foldMap)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (maybe)
import           Data.Monoid (Endo(Endo), (<>), mempty)
import qualified Data.Text.Encoding as Text
import           Prelude (Integer, (.), fromIntegral)

import           Ldap.Asn1.Type


-- | Convert a LDAP type to ASN.1.
--
-- When it's relevant, instances include the part of RFC describing the encoding.
class ToAsn1 a where
  toAsn1 :: a -> Endo [ASN1]

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
  toAsn1 (LdapMessage i op mc) =
    sequence (toAsn1 i <> toAsn1 op <> maybe mempty (context 0 . toAsn1) mc)

{- |
@
MessageID ::= INTEGER (0 ..  maxInt)
@
-}
instance ToAsn1 Id where
  toAsn1 (Id i) = single (Asn1.IntVal (fromIntegral i))

{- |
@
LDAPString ::= OCTET STRING -- UTF-8 encoded
@
-}
instance ToAsn1 LdapString where
  toAsn1 (LdapString s) = single (Asn1.OctetString (Text.encodeUtf8 s))

{- |
@
LDAPOID ::= OCTET STRING -- Constrained to \<numericoid\>
@
-}
instance ToAsn1 LdapOid where
  toAsn1 (LdapOid s) = single (Asn1.OctetString (Text.encodeUtf8 s))

{- |
@
LDAPDN ::= LDAPString -- Constrained to \<distinguishedName\>
@
-}
instance ToAsn1 LdapDn where
  toAsn1 (LdapDn s) = toAsn1 s

{- |
@
RelativeLDAPDN ::= LDAPString -- Constrained to \<name-component\>
@
-}
instance ToAsn1 RelativeLdapDn where
  toAsn1 (RelativeLdapDn s) = toAsn1 s

{- |
@
AttributeDescription ::= LDAPString
@
-}
instance ToAsn1 AttributeDescription where
  toAsn1 (AttributeDescription s) = toAsn1 s

{- |
@
AttributeValue ::= OCTET STRING
@
-}
instance ToAsn1 AttributeValue where
  toAsn1 (AttributeValue s) = single (Asn1.OctetString s)

{- |
@
AttributeValueAssertion ::= SEQUENCE {
     attributeDesc   AttributeDescription,
     assertionValue  AssertionValue }
@
-}
instance ToAsn1 AttributeValueAssertion where
  toAsn1 (AttributeValueAssertion d v) = toAsn1 d <> toAsn1 v

{- |
@
AssertionValue ::= OCTET STRING
@
-}
instance ToAsn1 AssertionValue where
  toAsn1 (AssertionValue s) = single (Asn1.OctetString s)


{- |
@
PartialAttribute ::= SEQUENCE {
     type       AttributeDescription,
     vals       SET OF value AttributeValue }
@
-}
instance ToAsn1 PartialAttribute where
  toAsn1 (PartialAttribute d xs) = sequence (toAsn1 d <> set (toAsn1 xs))

{- |
@
Attribute ::= PartialAttribute(WITH COMPONENTS {
     ...,
     vals (SIZE(1..MAX))})
@
-}
instance ToAsn1 Attribute where
  toAsn1 (Attribute d xs) = sequence (toAsn1 d <> set (toAsn1 xs))

{- |
@
MatchingRuleId ::= LDAPString
@
-}
instance ToAsn1 MatchingRuleId where
  toAsn1 (MatchingRuleId s) = toAsn1 s

{- |
@
Controls ::= SEQUENCE OF control Control
@
-}
instance ToAsn1 Controls where
  toAsn1 (Controls cs) = sequence (toAsn1 cs)

{- |
@
Control ::= SEQUENCE {
     controlType             LDAPOID,
     criticality             BOOLEAN DEFAULT FALSE,
     controlValue            OCTET STRING OPTIONAL }
@
-}
instance ToAsn1 Control where
  toAsn1 (Control t c v) =
    sequence (fold
      [ toAsn1 t
      , single (Asn1.Boolean c)
      , maybe mempty (single . Asn1.OctetString) v
      ])

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
ExtendedRequest ::= [APPLICATION 23] SEQUENCE {
     requestName      [0] LDAPOID,
     requestValue     [1] OCTET STRING OPTIONAL }
@
-}
instance ToAsn1 ProtocolClientOp where
  toAsn1 (BindRequest v n a) =
    application 0 (single (Asn1.IntVal (fromIntegral v)) <> toAsn1 n <> toAsn1 a)
  toAsn1 UnbindRequest =
    other Asn1.Application 2 mempty
  toAsn1 (SearchRequest bo s da sl tl to f a) =
    application 3 (fold
      [ toAsn1 bo
      , enum s'
      , enum da'
      , single (Asn1.IntVal (fromIntegral sl))
      , single (Asn1.IntVal (fromIntegral tl))
      , single (Asn1.Boolean to)
      , toAsn1 f
      , toAsn1 a
      ])
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
  toAsn1 (ModifyRequest dn xs) =
    application 6 (fold
      [ toAsn1 dn
      , sequence (foldMap (\(op, pa) -> sequence (enum (case op of
          Add     -> 0
          Delete  -> 1
          Replace -> 2) <> toAsn1 pa)) xs)
      ])
  toAsn1 (AddRequest dn as) =
    application 8 (toAsn1 dn <> toAsn1 as)
  toAsn1 (DeleteRequest (LdapDn (LdapString dn))) =
    other Asn1.Application 10 (Text.encodeUtf8 dn)
  toAsn1 (ModifyDnRequest dn rdn del new) =
    application 12 (fold
      [ toAsn1 dn
      , toAsn1 rdn
      , single (Asn1.Boolean del)
      , maybe mempty
              (\(LdapDn (LdapString dn')) -> other Asn1.Context 0 (Text.encodeUtf8 dn'))
              new
      ])
  toAsn1 (CompareRequest dn av) =
    application 14 (toAsn1 dn <> sequence (toAsn1 av))
  toAsn1 (ExtendedRequest (LdapOid oid) mv) =
    application 23 (fold
     [ other Asn1.Context 0 (Text.encodeUtf8 oid)
     , maybe mempty (other Asn1.Context 1) mv
     ])

{- |
@
AuthenticationChoice ::= CHOICE {
     simple                  [0] OCTET STRING,
     sasl                    [3] SaslCredentials,
     ...  }


SaslCredentials ::= SEQUENCE {
     mechanism               LDAPString,
     credentials             OCTET STRING OPTIONAL }
@
-}
instance ToAsn1 AuthenticationChoice where
  toAsn1 (Simple s) = other Asn1.Context 0 s
  toAsn1 (Sasl External c) =
    context 3 (fold
      [ toAsn1 (LdapString "EXTERNAL")
      , maybe mempty (toAsn1 . LdapString) c
      ])
{- |
@
AttributeSelection ::= SEQUENCE OF selector LDAPString
@
-}
instance ToAsn1 AttributeSelection where
  toAsn1 (AttributeSelection as) = sequence (toAsn1 as)

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
  toAsn1 f = case f of
    And xs            -> context 0 (toAsn1 xs)
    Or xs             -> context 1 (toAsn1 xs)
    Not x             -> context 2 (toAsn1 x)
    EqualityMatch x   -> context 3 (toAsn1 x)
    Substrings x      -> context 4 (toAsn1 x)
    GreaterOrEqual x  -> context 5 (toAsn1 x)
    LessOrEqual x     -> context 6 (toAsn1 x)
    Present (AttributeDescription (LdapString x))
                      -> other Asn1.Context 7 (Text.encodeUtf8 x)
    ApproxMatch x     -> context 8 (toAsn1 x)
    ExtensibleMatch x -> context 9 (toAsn1 x)

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
  toAsn1 (SubstringFilter ad ss) =
    toAsn1 ad <> sequence (foldMap (\s -> case s of
      Initial (AssertionValue v) -> other Asn1.Context 0 v
      Any (AssertionValue v)     -> other Asn1.Context 1 v
      Final (AssertionValue v)   -> other Asn1.Context 2 v) ss)

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
  toAsn1 (MatchingRuleAssertion mmr mad (AssertionValue av) _) = fold
    [ maybe mempty f mmr
    , maybe mempty g mad
    , other Asn1.Context 3 av
    ]
   where
    f (MatchingRuleId (LdapString x)) = other Asn1.Context 1 (Text.encodeUtf8 x)
    g (AttributeDescription (LdapString x)) = other Asn1.Context 2 (Text.encodeUtf8 x)

{- |
@
AttributeList ::= SEQUENCE OF attribute Attribute
@
-}
instance ToAsn1 AttributeList where
  toAsn1 (AttributeList xs) = sequence (toAsn1 xs)

instance ToAsn1 a => ToAsn1 [a] where
  toAsn1 = foldMap toAsn1

instance ToAsn1 a => ToAsn1 (NonEmpty a) where
  toAsn1 = foldMap toAsn1

sequence :: Endo [ASN1] -> Endo [ASN1]
sequence = construction Asn1.Sequence

set :: Endo [ASN1] -> Endo [ASN1]
set = construction Asn1.Set

application :: ASN1Tag -> Endo [ASN1] -> Endo [ASN1]
application = construction . Asn1.Container Asn1.Application

context :: ASN1Tag -> Endo [ASN1] -> Endo [ASN1]
context = construction . Asn1.Container Asn1.Context

construction :: ASN1ConstructionType -> Endo [ASN1] -> Endo [ASN1]
construction t x = single (Asn1.Start t) <> x <> single (Asn1.End t)

other :: ASN1Class -> ASN1Tag -> ByteString -> Endo [ASN1]
other c t = single . Asn1.Other c t

enum :: Integer -> Endo [ASN1]
enum = single . Asn1.Enumerated

single :: a -> Endo [a]
single x = Endo (x :)
