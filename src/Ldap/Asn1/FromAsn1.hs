{-# LANGUAGE CPP #-}
-- | This module contains convertions from ASN.1 to LDAP types.
module Ldap.Asn1.FromAsn1
  ( parseAsn1
  , FromAsn1
  ) where

#if __GLASGOW_HASKELL__ >= 710
import           Control.Applicative (Alternative(..), liftA2, optional)
#else
import           Control.Applicative (Applicative(..), Alternative(..), liftA2, optional)
#endif
import           Control.Monad (MonadPlus(..), (>=>), guard)
import           Data.ASN1.Types (ASN1)
import qualified Data.ASN1.Types as Asn1
import           Data.Foldable (asum)
import           Data.List.NonEmpty (some1)
import qualified Data.Text.Encoding as Text

import           Ldap.Asn1.Type

{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}


-- | Convert a part of ASN.1 stream to a LDAP type returning the remainder of the stream.
parseAsn1 :: FromAsn1 a => [ASN1] -> Maybe ([ASN1], a)
parseAsn1 = parse fromAsn1

-- | ASN.1 stream parsers.
--
-- When it's relevant, instances include the part of RFC describing the encoding.
class FromAsn1 a where
  fromAsn1 :: Parser [ASN1] a

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
instance FromAsn1 op =>  FromAsn1 (LdapMessage op) where
  fromAsn1 = do
    Asn1.Start Asn1.Sequence <- next
    i  <- fromAsn1
    op <- fromAsn1
    Asn1.End Asn1.Sequence <- next
    return (LdapMessage i op Nothing)

{- |
@
MessageID ::= INTEGER (0 ..  maxInt)
@
-}
instance FromAsn1 Id where
  fromAsn1 = do
    Asn1.IntVal i <- next
    return (Id (fromIntegral i))

{- |
@
LDAPString ::= OCTET STRING -- UTF-8 encoded,
@
-}
instance FromAsn1 LdapString where
  fromAsn1 = do
    Asn1.OctetString s <- next
    case Text.decodeUtf8' s of
      Right t -> return (LdapString t)
      Left  _ -> empty

{- |
@
LDAPOID ::= OCTET STRING -- Constrained to \<numericoid\>
@
-}
instance FromAsn1 LdapOid where
  fromAsn1 = do
    Asn1.OctetString s <- next
    case Text.decodeUtf8' s of
      Right t -> return (LdapOid t)
      Left  _ -> empty

{- |
@
LDAPDN ::= LDAPString
@
-}
instance FromAsn1 LdapDn where
  fromAsn1 = fmap LdapDn fromAsn1

{- |
@
AttributeDescription ::= LDAPString
@
-}
instance FromAsn1 AttributeDescription where
  fromAsn1 = fmap AttributeDescription fromAsn1

{- |
@
AttributeValue ::= OCTET STRING
@
-}
instance FromAsn1 AttributeValue where
  fromAsn1 = do
    Asn1.OctetString s <- next
    return (AttributeValue s)

{- |
@
PartialAttribute ::= SEQUENCE {
     type       AttributeDescription,
     vals       SET OF value AttributeValue }
@
-}
instance FromAsn1 PartialAttribute where
  fromAsn1 = do
    Asn1.Start Asn1.Sequence <- next
    d  <- fromAsn1
    Asn1.Start Asn1.Set <- next
    vs <- many fromAsn1
    Asn1.End Asn1.Set <- next
    Asn1.End Asn1.Sequence <- next
    return (PartialAttribute d vs)

{- |
@
LDAPResult ::= SEQUENCE {
     resultCode         ENUMERATED {
          success                      (0),
          operationsError              (1),
          protocolError                (2),
          timeLimitExceeded            (3),
          sizeLimitExceeded            (4),
          compareFalse                 (5),
          compareTrue                  (6),
          authMethodNotSupported       (7),
          strongerAuthRequired         (8),
          -- 9 reserved --
          referral                     (10),
          adminLimitExceeded           (11),
          unavailableCriticalExtension (12),
          confidentialityRequired      (13),
          saslBindInProgress           (14),
          noSuchAttribute              (16),
          undefinedAttributeType       (17),
          inappropriateMatching        (18),
          constraintViolation          (19),
          attributeOrValueExists       (20),
          invalidAttributeSyntax       (21),
          -- 22-31 unused --
          noSuchObject                 (32),
          aliasProblem                 (33),
          invalidDNSyntax              (34),
          -- 35 reserved for undefined isLeaf --
          aliasDereferencingProblem    (36),
          -- 37-47 unused --
          inappropriateAuthentication  (48),
          invalidCredentials           (49),
          insufficientAccessRights     (50),
          busy                         (51),
          unavailable                  (52),
          unwillingToPerform           (53),
          loopDetect                   (54),
          -- 55-63 unused --
          namingViolation              (64),
          objectClassViolation         (65),
          notAllowedOnNonLeaf          (66),
          notAllowedOnRDN              (67),
          entryAlreadyExists           (68),
          objectClassModsProhibited    (69),
          -- 70 reserved for CLDAP --
          affectsMultipleDSAs          (71),
          -- 72-79 unused --
          other                        (80),
          ...  },
     matchedDN          LDAPDN,
     diagnosticMessage  LDAPString,
     referral           [3] Referral OPTIONAL }
@
-}
instance FromAsn1 LdapResult where
  fromAsn1 = do
    resultCode <- do
      Asn1.Enumerated x <- next
      case x of
        0  -> pure Success
        1  -> pure OperationError
        2  -> pure ProtocolError
        3  -> pure TimeLimitExceeded
        4  -> pure SizeLimitExceeded
        5  -> pure CompareFalse
        6  -> pure CompareTrue
        7  -> pure AuthMethodNotSupported
        8  -> pure StrongerAuthRequired
        10 -> pure Referral
        11 -> pure AdminLimitExceeded
        12 -> pure UnavailableCriticalExtension
        13 -> pure ConfidentialityRequired
        14 -> pure SaslBindInProgress
        16 -> pure NoSuchAttribute
        17 -> pure UndefinedAttributeType
        18 -> pure InappropriateMatching
        19 -> pure ConstraintViolation
        20 -> pure AttributeOrValueExists
        21 -> pure InvalidAttributeSyntax
        32 -> pure NoSuchObject
        33 -> pure AliasProblem
        34 -> pure InvalidDNSyntax
        36 -> pure AliasDereferencingProblem
        48 -> pure InappropriateAuthentication
        49 -> pure InvalidCredentials
        50 -> pure InsufficientAccessRights
        51 -> pure Busy
        52 -> pure Unavailable
        53 -> pure UnwillingToPerform
        54 -> pure LoopDetect
        64 -> pure NamingViolation
        65 -> pure ObjectClassViolation
        66 -> pure NotAllowedOnNonLeaf
        67 -> pure NotAllowedOnRDN
        68 -> pure EntryAlreadyExists
        69 -> pure ObjectClassModsProhibited
        71 -> pure AffectsMultipleDSAs
        80 -> pure Other
        _  -> empty
    matchedDn  <- fromAsn1
    diagnosticMessage
               <- fromAsn1
    referral   <- optional $ do
      Asn1.Start (Asn1.Container Asn1.Context 0) <- next
      x <- fromAsn1
      Asn1.End (Asn1.Container Asn1.Context 0) <- next
      return x
    return (LdapResult resultCode matchedDn diagnosticMessage referral)

{- |
@
Referral ::= SEQUENCE SIZE (1..MAX) OF uri URI
@
-}
instance FromAsn1 ReferralUris where
  fromAsn1 = do
    Asn1.Start Asn1.Sequence <- next
    xs <- some1 fromAsn1
    Asn1.End Asn1.Sequence <- next
    return (ReferralUris xs)

{- |
@
URI ::= LDAPString
@
-}
instance FromAsn1 Uri where
  fromAsn1 = fmap Uri fromAsn1

{- |
@
BindResponse ::= [APPLICATION 1] SEQUENCE {
     COMPONENTS OF LDAPResult,
     serverSaslCreds    [7] OCTET STRING OPTIONAL }
@

@
SearchResultEntry ::= [APPLICATION 4] SEQUENCE {
     objectName      LDAPDN,
     attributes      PartialAttributeList }
@

@
SearchResultReference ::= [APPLICATION 19] SEQUENCE
                          SIZE (1..MAX) OF uri URI
@

@
SearchResultDone ::= [APPLICATION 5] LDAPResult
@

@
ModifyResponse ::= [APPLICATION 7] LDAPResult
@

@
AddResponse ::= [APPLICATION 9] LDAPResult
@

@
DelResponse ::= [APPLICATION 11] LDAPResult
@

@
ModifyDNResponse ::= [APPLICATION 13] LDAPResult
@

@
CompareResponse ::= [APPLICATION 15] LDAPResult
@

@
ExtendedResponse ::= [APPLICATION 24] SEQUENCE {
     COMPONENTS OF LDAPResult,
     responseName     [10] LDAPOID OPTIONAL,
     responseValue    [11] OCTET STRING OPTIONAL }
@

@
IntermediateResponse ::= [APPLICATION 25] SEQUENCE {
     responseName     [0] LDAPOID OPTIONAL,
     responseValue    [1] OCTET STRING OPTIONAL }
@
-}
instance FromAsn1 ProtocolServerOp where
  fromAsn1 = asum
    [ fmap (\res -> BindResponse res Nothing) (app 1)
    , fmap (uncurry SearchResultEntry) (app 4)
    , fmap SearchResultDone (app 5)
    , fmap ModifyResponse (app 7)
    , fmap AddResponse (app 9)
    , fmap DeleteResponse (app 11)
    , fmap ModifyDnResponse (app 13)
    , fmap CompareResponse (app 15)

    , do
      Asn1.Start (Asn1.Container Asn1.Application 19) <- next
      uris <- some1 fromAsn1
      Asn1.End (Asn1.Container Asn1.Application 19) <- next
      return (SearchResultReference uris)

    , do
      Asn1.Start (Asn1.Container Asn1.Application 24) <- next
      res <- fromAsn1
      utf8Name <- optional $ do
        Asn1.Other Asn1.Context 10 s <- next
        return s
      name <- maybe (return Nothing) (\n -> case Text.decodeUtf8' n of
        Left  _    -> empty
        Right name -> return (Just name)) utf8Name
      value <- optional $ do
        Asn1.Other Asn1.Context 11 s <- next
        return s
      Asn1.End (Asn1.Container Asn1.Application 24) <- next
      return (ExtendedResponse res (fmap LdapOid name) value)

    , do
      Asn1.Start (Asn1.Container Asn1.Application 25) <- next
      name  <- optional fromAsn1
      value <- optional $ do
        Asn1.OctetString s <- next
        return s
      Asn1.End (Asn1.Container Asn1.Application 25) <- next
      return (IntermediateResponse name value)
    ]
   where
    app l = do
      Asn1.Start (Asn1.Container Asn1.Application x) <- next
      guard (x == l)
      res <- fromAsn1
      Asn1.End (Asn1.Container Asn1.Application y) <- next
      guard (y == l)
      return res

{- |
@
PartialAttributeList ::= SEQUENCE OF partialAttribute PartialAttribute
@
-}
instance FromAsn1 PartialAttributeList where
  fromAsn1 = do
    Asn1.Start Asn1.Sequence <- next
    xs <- many fromAsn1
    Asn1.End Asn1.Sequence <- next
    return (PartialAttributeList xs)

instance (FromAsn1 a, FromAsn1 b) => FromAsn1 (a, b) where
  fromAsn1 = liftA2 (,) fromAsn1 fromAsn1


newtype Parser s a = Parser { unParser :: s -> Maybe (s, a) }

instance Functor (Parser s) where
  fmap f (Parser g) = Parser (fmap (fmap f) . g)

instance Applicative (Parser s) where
  pure x = Parser (\s -> pure (s, x))
  Parser mf <*> Parser mx = Parser $ \s -> do
    (s', f)  <- mf s
    (s'', x) <- mx s'
    pure (s'', f x)

instance Alternative (Parser s) where
  empty = Parser (\_ -> empty)
  Parser ma <|> Parser mb =
    Parser (\s -> ma s <|> mb s)

instance Monad (Parser s) where
  return x = Parser (\s -> return (s, x))
  Parser mx >>= k =
    Parser (mx >=> \(s', x) -> unParser (k x) s')
  fail _ = empty

instance MonadPlus (Parser s) where
  mzero = Parser (\_ -> mzero)
  Parser ma `mplus` Parser mb =
    Parser (\s -> ma s `mplus` mb s)

parse :: Parser s a -> s -> Maybe (s, a)
parse = unParser

next :: Parser [s] s
next = Parser (\s -> case s of [] -> Nothing; x : xs -> Just (xs, x))
