module Ldap.Asn1.Type where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)


-- | Message envelope. (Section 4.1.1.)
data LdapMessage op = LdapMessage
  { ldapMessageId       :: !Id
  , ldapMessageOp       :: !op
  , ldapMessageControls :: !(Maybe Controls)
  } deriving (Show, Eq)

-- | Every message being processed has a unique non-zero integer ID. (Section 4.1.1.1.)
newtype Id = Id { unId :: Int32 }
    deriving (Show, Eq, Ord)

-- | Client requests.  The RFC doesn't make a difference between 'ProtocolClientOp'
-- and 'ProtocolServerOp' but it's useful to distinguish between them in Haskell.
data ProtocolClientOp =
    BindRequest !Int8 !LdapDn !AuthenticationChoice
  | UnbindRequest
  | SearchRequest !LdapDn !Scope !DerefAliases !Int32 !Int32 !Bool !Filter !AttributeSelection
  | ModifyRequest !LdapDn ![(Operation, PartialAttribute)]
  | AddRequest !LdapDn !AttributeList
  | DeleteRequest !LdapDn
  | ModifyDnRequest !LdapDn !RelativeLdapDn !Bool !(Maybe LdapDn)
  | CompareRequest !LdapDn !AttributeValueAssertion
  | AbandonRequest !Id
  | ExtendedRequest !LdapOid !(Maybe ByteString)
    deriving (Show, Eq)

-- | Server responses.  The RFC doesn't make a difference between 'ProtocolClientOp'
-- and 'ProtocolServerOp' but it's useful to distinguish between them in Haskell.
data ProtocolServerOp =
    BindResponse !LdapResult !(Maybe ByteString)
  | SearchResultEntry !LdapDn !PartialAttributeList
  | SearchResultReference !(NonEmpty Uri)
  | SearchResultDone !(LdapResult)
  | ModifyResponse !LdapResult
  | AddResponse !LdapResult
  | DeleteResponse !LdapResult
  | ModifyDnResponse !LdapResult
  | CompareResponse !LdapResult
  | ExtendedResponse !LdapResult !(Maybe LdapOid) !(Maybe ByteString)
  | IntermediateResponse !(Maybe LdapOid) !(Maybe ByteString)
    deriving (Show, Eq)

-- | Not really a choice until SASL is supported.
newtype AuthenticationChoice = Simple ByteString
    deriving (Show, Eq)

-- | Scope of the search to be performed.
data Scope =
    BaseObject   -- ^ Constrained to the entry named by baseObject.
  | SingleLevel  -- ^ Constrained to the immediate subordinates of the entry named by baseObject.
  | WholeSubtree -- ^ Constrained to the entry named by baseObject and to all its subordinates.
    deriving (Show, Eq)

-- | An indicator as to whether or not alias entries (as defined in
-- [RFC4512]) are to be dereferenced during stages of the Search
-- operation.
data DerefAliases =
    NeverDerefAliases      -- ^ Do not dereference aliases in searching or in locating the base object of the Search.
  | DerefInSearching       -- ^ While searching subordinates of the base object, dereference any alias within the search scope.
  | DerefFindingBaseObject -- ^ Dereference aliases in locating the base object of the Search.
  | DerefAlways            -- ^ Dereference aliases both in searching and in locating the base object of the Search.
    deriving (Show, Eq)

-- | Conditions that must be fulfilled in order for the Search to match a given entry.
data Filter =
    And !(NonEmpty Filter)                 -- ^ All filters evaluate to @TRUE@
  | Or !(NonEmpty Filter)                  -- ^ Any filter evaluates to @TRUE@
  | Not Filter                             -- ^ Filter evaluates to @FALSE@
  | EqualityMatch AttributeValueAssertion  -- ^ @EQUALITY@ rule returns @TRUE@
  | Substrings SubstringFilter             -- ^ @SUBSTR@ rule returns @TRUE@
  | GreaterOrEqual AttributeValueAssertion -- ^ @ORDERING@ rule returns @FALSE@
  | LessOrEqual AttributeValueAssertion    -- ^ @ORDERING@ or @EQUALITY@ rule returns @TRUE@
  | Present AttributeDescription           -- ^ Attribute is present in the entry
  | ApproxMatch AttributeValueAssertion    -- ^ Same as 'EqualityMatch' for most servers
  | ExtensibleMatch MatchingRuleAssertion
    deriving (Show, Eq)

data SubstringFilter = SubstringFilter !AttributeDescription !(NonEmpty Substring)
    deriving (Show, Eq)

data Substring =
    Initial !AssertionValue
  | Any !AssertionValue
  | Final !AssertionValue
    deriving (Show, Eq)

data MatchingRuleAssertion = MatchingRuleAssertion !(Maybe MatchingRuleId) !(Maybe AttributeDescription) !AssertionValue !Bool
    deriving (Show, Eq)

-- | Matching rules are defined in Section 4.1.3 of [RFC4512].  A matching
-- rule is identified in the protocol by the printable representation of
-- either its <numericoid> or one of its short name descriptors
-- [RFC4512], e.g., 'caseIgnoreMatch' or '2.5.13.2'. (Section 4.1.8.)
newtype MatchingRuleId = MatchingRuleId LdapString
    deriving (Show, Eq)

newtype AttributeSelection = AttributeSelection [LdapString]
    deriving (Show, Eq)

newtype AttributeList = AttributeList [Attribute]
    deriving (Show, Eq)

newtype PartialAttributeList = PartialAttributeList [PartialAttribute]
    deriving (Show, Eq)

newtype Controls = Controls [Control]
    deriving (Show, Eq)

data Control = Control !LdapOid !Bool !(Maybe ByteString)
    deriving (Show, Eq)

data LdapResult = LdapResult !ResultCode !LdapDn !LdapString !(Maybe ReferralUris)
    deriving (Show, Eq)

-- | LDAP operation's result.
data ResultCode =
    Success
  | OperationError
  | ProtocolError
  | TimeLimitExceeded
  | SizeLimitExceeded
  | CompareFalse
  | CompareTrue
  | AuthMethodNotSupported
  | StrongerAuthRequired
  | Referral
  | AdminLimitExceeded
  | UnavailableCriticalExtension
  | ConfidentialityRequired
  | SaslBindInProgress
  | NoSuchAttribute
  | UndefinedAttributeType
  | InappropriateMatching
  | ConstraintViolation
  | AttributeOrValueExists
  | InvalidAttributeSyntax
  | NoSuchObject
  | AliasProblem
  | InvalidDNSyntax
  | AliasDereferencingProblem
  | InappropriateAuthentication
  | InvalidCredentials
  | InsufficientAccessRights
  | Busy
  | Unavailable
  | UnwillingToPerform
  | LoopDetect
  | NamingViolation
  | ObjectClassViolation
  | NotAllowedOnNonLeaf
  | NotAllowedOnRDN
  | EntryAlreadyExists
  | ObjectClassModsProhibited
  | AffectsMultipleDSAs
  | Other
    deriving (Show, Eq)

newtype AttributeDescription = AttributeDescription LdapString
    deriving (Show, Eq)

newtype AttributeValue = AttributeValue ByteString
    deriving (Show, Eq)

data AttributeValueAssertion = AttributeValueAssertion !AttributeDescription !AssertionValue
    deriving (Show, Eq)

newtype AssertionValue = AssertionValue ByteString
    deriving (Show, Eq)

data Attribute = Attribute !AttributeDescription !(NonEmpty AttributeValue)
    deriving (Show, Eq)

data PartialAttribute = PartialAttribute !AttributeDescription ![AttributeValue]
    deriving (Show, Eq)



-- | An LDAPDN is defined to be the representation of a Distinguished Name
-- (DN) after encoding according to the specification in [RFC4514].
newtype LdapDn = LdapDn LdapString
    deriving (Show, Eq)

-- | A RelativeLDAPDN is defined to be the representation of a Relative
-- Distinguished Name (RDN) after encoding according to the
-- specification in [RFC4514].
newtype RelativeLdapDn = RelativeLdapDn LdapString
    deriving (Show, Eq)

newtype ReferralUris = ReferralUris (NonEmpty Uri)
    deriving (Show, Eq)

newtype Uri = Uri LdapString
    deriving (Show, Eq)

data Operation =
    Add
  | Delete
  | Replace
    deriving (Show, Eq)

-- | The LDAPString is a notational convenience to indicate that, although
-- strings of LDAPString type encode as ASN.1 OCTET STRING types, the
-- [ISO10646] character set (a superset of [Unicode]) is used, encoded
-- following the UTF-8 [RFC3629] algorithm. (Section 4.1.2.)
newtype LdapString = LdapString Text
    deriving (Show, Eq)

-- | The LDAPOID is a notational convenience to indicate that the
-- permitted value of this string is a (UTF-8 encoded) dotted-decimal
-- representation of an OBJECT IDENTIFIER.  Although an LDAPOID is
-- encoded as an OCTET STRING, values are limited to the definition of
-- \<numericoid\> given in Section 1.4 of [RFC4512].
newtype LdapOid = LdapOid Text
    deriving (Show, Eq)
