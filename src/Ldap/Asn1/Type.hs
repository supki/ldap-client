module Ldap.Asn1.Type where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)


data LdapMessage op = LdapMessage
  { ldapMessageId       :: !Id
  , ldapMessageOp       :: !op
  , ldapMessageControls :: !(Maybe Controls)
  } deriving (Show, Eq, Ord)

newtype Id = Id { unId :: Int32 }
    deriving (Show, Eq, Ord)

data ProtocolClientOp =
    BindRequest Int8 LdapDn AuthenticationChoice
  | UnbindRequest
  | SearchRequest LdapDn Scope DerefAliases Int32 Int32 Bool Filter AttributeSelection
  | ModifyRequest LdapDn [(Operation, PartialAttribute)]
  | AddRequest LdapDn AttributeList
  | DeleteRequest LdapDn
  | ModifyDnRequest LdapDn RelativeLdapDn Bool (Maybe LdapDn)
  | CompareRequest LdapDn AttributeValueAssertion
  | ExtendedRequest LdapOid (Maybe ByteString)
    deriving (Show, Eq, Ord)

data ProtocolServerOp =
    BindResponse LdapResult (Maybe ByteString)
  | SearchResultEntry LdapDn PartialAttributeList
  | SearchResultReference (NonEmpty Uri)
  | SearchResultDone (LdapResult)
  | ModifyResponse LdapResult
  | AddResponse LdapResult
  | DeleteResponse LdapResult
  | ModifyDnResponse LdapResult
  | CompareResponse LdapResult
  | ExtendedResponse LdapResult (Maybe LdapOid) (Maybe ByteString)
  | IntermediateResponse (Maybe LdapOid) (Maybe ByteString)
    deriving (Show, Eq, Ord)

data AuthenticationChoice = Simple ByteString
    deriving (Show, Eq, Ord)

data Scope =
    BaseObject
  | SingleLevel
  | WholeSubtree
    deriving (Show, Eq, Ord)

data DerefAliases =
    NeverDerefAliases
  | DerefInSearching
  | DerefFindingBaseObject
  | DerefAlways
    deriving (Show, Eq, Ord)

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
    deriving (Show, Eq, Ord)

data SubstringFilter = SubstringFilter AttributeDescription (NonEmpty Substring)
    deriving (Show, Eq, Ord)

data Substring =
    Initial AssertionValue
  | Any AssertionValue
  | Final AssertionValue
    deriving (Show, Eq, Ord)

data MatchingRuleAssertion = MatchingRuleAssertion (Maybe MatchingRuleId) (Maybe AttributeDescription) AssertionValue Bool
    deriving (Show, Eq, Ord)

newtype MatchingRuleId = MatchingRuleId LdapString
    deriving (Show, Eq, Ord)

newtype AttributeSelection = AttributeSelection [LdapString]
    deriving (Show, Eq, Ord)

newtype AttributeList = AttributeList [Attribute]
    deriving (Show, Eq, Ord)

newtype PartialAttributeList = PartialAttributeList [PartialAttribute]
    deriving (Show, Eq, Ord)

newtype Controls = Controls [Control]
    deriving (Show, Eq, Ord)

data Control = Control LdapOid Bool (Maybe ByteString)
    deriving (Show, Eq, Ord)

data LdapResult = LdapResult ResultCode LdapDn LdapString (Maybe ReferralUris)
    deriving (Show, Eq, Ord)

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
    deriving (Show, Eq, Ord)

newtype AttributeDescription = AttributeDescription LdapString
    deriving (Show, Eq, Ord)

newtype AttributeValue = AttributeValue ByteString
    deriving (Show, Eq, Ord)

data AttributeValueAssertion = AttributeValueAssertion AttributeDescription AssertionValue
    deriving (Show, Eq, Ord)

newtype AssertionValue = AssertionValue ByteString
    deriving (Show, Eq, Ord)

data Attribute = Attribute AttributeDescription (NonEmpty AttributeValue)
    deriving (Show, Eq, Ord)

data PartialAttribute = PartialAttribute AttributeDescription [AttributeValue]
    deriving (Show, Eq, Ord)

newtype LdapDn = LdapDn LdapString
    deriving (Show, Eq, Ord)

newtype RelativeLdapDn = RelativeLdapDn LdapString
    deriving (Show, Eq, Ord)

newtype ReferralUris = ReferralUris (NonEmpty Uri)
    deriving (Show, Eq, Ord)

newtype Uri = Uri LdapString
    deriving (Show, Eq, Ord)

data Operation =
    Add
  | Delete
  | Replace
    deriving (Show, Eq, Ord)

-- | The LDAPString is a notational convenience to indicate that, although
-- strings of LDAPString type encode as ASN.1 OCTET STRING types, the
-- [ISO10646] character set (a superset of [Unicode]) is used, encoded
-- following the UTF-8 [RFC3629] algorithm.
newtype LdapString = LdapString Text
    deriving (Show, Eq, Ord)

newtype LdapOid = LdapOid ByteString
    deriving (Show, Eq, Ord)
