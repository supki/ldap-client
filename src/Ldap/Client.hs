{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
module Ldap.Client
  ( Host(..)
  , PortNumber
  , Ldap
  , LdapError(..)
  , Async
  , with
    -- * Bind Request
  , Dn(..)
  , Password(..)
  , bind
  , bindEither
  , bindAsync
  , bindAsyncSTM
    -- * Search Request
  , Type.Scope(..)
  , Attr(..)
  , SearchEntry(..)
  , search
  , searchEither
  , searchAsync
  , searchAsyncSTM
  , Search
  , defaultSearch
  , scope
  , size
  , time
  , typesOnly
  , derefAliases
  , Filter(..)
    -- * Unbind Request
  , unbindAsync
  , unbindAsyncSTM
    -- * Waiting for Request Completion
  , wait
  , waitSTM
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, readTMVar)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import           Control.Exception (Exception, Handler(..), bracket, throwIO, catches)
import           Control.Monad (forever, void)
import qualified Data.ASN1.BinaryEncoding as Asn1
import qualified Data.ASN1.Encoding as Asn1
import qualified Data.ASN1.Error as Asn1
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           Data.Foldable (traverse_, asum)
import           Data.Function (fix)
import           Data.Int (Int32)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Monoid (Endo(appEndo))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Network.Connection (Connection)
import qualified Network.Connection as Conn
import           Network (PortNumber)
import qualified System.IO.Error as IO

import           Ldap.Asn1.ToAsn1 (ToAsn1(toAsn1))
import           Ldap.Asn1.FromAsn1 (FromAsn1, parseAsn1)
import qualified Ldap.Asn1.Type as Type


data Host =
    Plain String
  | Secure String
    deriving (Show, Eq, Ord)

data Ldap = Ldap
  { client  :: TQueue ClientMessage
  } deriving (Eq)

data ClientMessage = New Request (TMVar (NonEmpty Type.ProtocolServerOp))
type Request = Type.ProtocolClientOp
type InMessage = Type.ProtocolServerOp
type Response = NonEmpty InMessage

newLdap :: IO Ldap
newLdap = Ldap
  <$> newTQueueIO

data LdapError =
    IOError IOError
  | ParseError Asn1.ASN1Error
  | BindError BindError
  | SearchError SearchError
    deriving (Show, Eq)

-- | The entrypoint into LDAP.
with :: Host -> PortNumber -> (Ldap -> IO a) -> IO (Either LdapError a)
with host port f = do
  context <- Conn.initConnectionContext
  bracket (Conn.connectTo context params) Conn.connectionClose (\conn ->
    bracket newLdap unbindAsync (\l -> do
      inq  <- newTQueueIO
      outq <- newTQueueIO
      Async.withAsync (input inq conn) $ \i ->
        Async.withAsync (output outq conn) $ \o ->
          Async.withAsync (dispatch l inq outq) $ \d ->
            Async.withAsync (f l) $ \u ->
              fmap (Right . snd) (Async.waitAnyCancel [i, o, d, u])))
 `catches`
  [ Handler (return . Left . IOError)
  , Handler (return . Left . ParseError)
  , Handler (return . Left . BindError)
  , Handler (return . Left . SearchError)
  ]
 where
  params = Conn.ConnectionParams
    { Conn.connectionHostname =
        case host of
          Plain  h -> h
          Secure h -> h
    , Conn.connectionPort = port
    , Conn.connectionUseSecure =
        case host of
          Plain  _ -> Nothing
          Secure _ -> Just Conn.TLSSettingsSimple
            { Conn.settingDisableCertificateValidation = False
            , Conn.settingDisableSession = False
            , Conn.settingUseServerName = False
            }
    , Conn.connectionUseSocks = Nothing
    }

input :: FromAsn1 a => TQueue a -> Connection -> IO b
input inq conn = flip fix [] $ \loop chunks -> do
  chunk <- Conn.connectionGet conn 8192
  case ByteString.length chunk of
    0 -> throwIO (IO.mkIOError IO.eofErrorType "Ldap.Client.input" Nothing Nothing)
    _ -> do
      let chunks' = chunk : chunks
      case Asn1.decodeASN1 Asn1.DER (ByteString.Lazy.fromChunks (reverse chunks')) of
        Left  Asn1.ParsingPartial
                   -> loop chunks'
        Left  e    -> throwIO e
        Right asn1 -> do
          flip fix asn1 $ \loop' asn1' ->
            case parseAsn1 asn1' of
              Nothing -> return ()
              Just (asn1'', a) -> do
                atomically (writeTQueue inq a)
                loop' asn1''
          loop []

output :: ToAsn1 a => TQueue a -> Connection -> IO b
output out conn = forever $
  Conn.connectionPut conn . encode . toAsn1 =<< atomically (readTQueue out)
 where
  encode x = Asn1.encodeASN1' Asn1.DER (appEndo x [])

dispatch :: Ldap -> TQueue (Type.LdapMessage InMessage) -> TQueue (Type.LdapMessage Request) -> IO a
dispatch Ldap { client } inq outq =
  flip fix (Map.empty, Map.empty, 1) $ \loop (!got, !results, !counter) -> do
    loop =<< atomically (asum
      [ do New new var <- readTQueue client
           writeTQueue outq (Type.LdapMessage (Type.Id counter) new Nothing)
           return (got, Map.insert (Type.Id counter) var results, counter + 1)
      , do Type.LdapMessage mid op _ <- readTQueue inq
           case op of
             Type.BindResponse {} -> do
               traverse_ (\var -> putTMVar var (op :| [])) (Map.lookup mid results)
               return (Map.delete mid got, Map.delete mid results, counter)
             Type.SearchResultEntry {} -> do
               return (Map.insertWith (++) mid [op] got, results, counter)
             Type.SearchResultReference {} -> do
               return (got, results, counter)
             Type.SearchResultDone {} -> do
               let stack = Map.findWithDefault [] mid got
               traverse_ (\var -> putTMVar var (op :| stack)) (Map.lookup mid results)
               return (Map.delete mid got, Map.delete mid results, counter)
      ])


data Async e a = Async (STM (Either e a))

instance Functor (Async e) where
  fmap f (Async stm) = Async (fmap (fmap f) stm)


newtype Dn = Dn Text
    deriving (Show, Eq)
newtype Password = Password ByteString
    deriving (Show, Eq)

data BindError =
    BindInvalidResponse Response
  | BindErrorCode Type.ResultCode
    deriving (Show, Eq, Typeable)

instance Exception BindError

-- | Throws 'BindError' on failure. Don't worry, the nearest 'with'
-- will catch it, so it won't destroy your program.
bind :: Ldap -> Dn -> Password -> IO ()
bind l username password =
  raise =<< bindEither l username password

bindEither :: Ldap -> Dn -> Password -> IO (Either BindError ())
bindEither l username password =
  wait =<< bindAsync l username password

bindAsync :: Ldap -> Dn -> Password -> IO (Async BindError ())
bindAsync l username password =
  atomically (bindAsyncSTM l username password)

bindAsyncSTM :: Ldap -> Dn -> Password -> STM (Async BindError ())
bindAsyncSTM l username password =
  sendRequest l bindResult (bindRequest username password)

bindRequest :: Dn -> Password -> Request
bindRequest (Dn username) (Password password) =
  Type.BindRequest ldapVersion
                   (Type.LdapDn (Type.LdapString username))
                   (Type.Simple password)
 where
  ldapVersion = 3

bindResult :: Response -> Either BindError ()
bindResult (Type.BindResponse (Type.LdapResult code _ _ _) _ :| [])
  | Type.Success <- code = Right ()
  | otherwise = Left (BindErrorCode code)
bindResult res = Left (BindInvalidResponse res)


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

newtype Attr = Attr Text
    deriving (Show, Eq)

-- 'Attr' unwrapper. This is a separate function not to turn 'Attr''s
-- 'Show' instance into complete and utter shit.
unAttr :: Attr -> Text
unAttr (Attr a) = a

data SearchEntry = SearchEntry Dn [(Attr, Set ByteString)]
    deriving (Show, Eq)


-- | Note that 'unbindAsync' does not return an 'Async',
-- because LDAP server never responds to @UnbindRequest@s, hence
-- a call to 'wait' on a hypothetical 'Async' would have resulted
-- in an exception anyway.
unbindAsync :: Ldap -> IO ()
unbindAsync =
  atomically . unbindAsyncSTM

-- | Note that 'unbindAsyncSTM' does not return an 'Async',
-- because LDAP server never responds to @UnbindRequest@s, hence
-- a call to 'wait' on a hypothetical 'Async' would have resulted
-- in an exception anyway.
unbindAsyncSTM :: Ldap -> STM ()
unbindAsyncSTM l =
  void (sendRequest l die Type.UnbindRequest)
 where
  die = error "Ldap.Client: do not wait for the response to UnbindRequest"


wait :: Async e a -> IO (Either e a)
wait = atomically . waitSTM

waitSTM :: Async e a -> STM (Either e a)
waitSTM (Async stm) = stm


sendRequest :: Ldap -> (Response -> Either e a) -> Request -> STM (Async e a)
sendRequest l p msg =
  do var <- newEmptyTMVar
     writeRequest l var msg
     return (Async (fmap p (readTMVar var)))

writeRequest :: Ldap -> TMVar Response -> Request -> STM ()
writeRequest Ldap { client } var msg = writeTQueue client (New msg var)

raise :: Exception e => Either e a -> IO a
raise = either throwIO return
