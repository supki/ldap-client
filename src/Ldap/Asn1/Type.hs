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

newtype AuthenticationChoice = Simple ByteString
    deriving (Show, Eq)

data Scope =
    BaseObject
  | SingleLevel
  | WholeSubtree
    deriving (Show, Eq)

data DerefAliases =
    NeverDerefAliases
  | DerefInSearching
  | DerefFindingBaseObject
  | DerefAlways
    deriving (Show, Eq)

data Filter =
    And (NonEmpty Filter)
  | Or (NonEmpty Filter)
  | Not Filter
  | EqualityMatch AttributeValueAssertion
  | Substrings SubstringFilter
  | GreaterOrEqual AttributeValueAssertion
  | LessOrEqual AttributeValueAssertion
  | Present AttributeDescription
  | ApproxMatch AttributeValueAssertion
  | ExtensibleMatch MatchingRuleAssertion
    deriving (Show, Eq)

data SubstringFilter = SubstringFilter AttributeDescription (NonEmpty Substring)
    deriving (Show, Eq)

data Substring =
    Initial AssertionValue
  | Any AssertionValue
  | Final AssertionValue
    deriving (Show, Eq)

data MatchingRuleAssertion = MatchingRuleAssertion (Maybe MatchingRuleId) (Maybe AttributeDescription) AssertionValue Bool
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

data Control = Control LdapOid Bool (Maybe ByteString)
    deriving (Show, Eq)

data LdapResult = LdapResult ResultCode LdapDn LdapString (Maybe ReferralUris)
    deriving (Show, Eq)

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

data AttributeValueAssertion = AttributeValueAssertion AttributeDescription AssertionValue
    deriving (Show, Eq)

newtype AssertionValue = AssertionValue ByteString
    deriving (Show, Eq)

data Attribute = Attribute AttributeDescription (NonEmpty AttributeValue)
    deriving (Show, Eq)

data PartialAttribute = PartialAttribute AttributeDescription [AttributeValue]
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
