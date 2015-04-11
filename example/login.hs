{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- | An example of how to do LDAP logins with ldap-client.
--
-- First, the assumptions this example makes. It defaults to LDAP over TLS,
-- so if you only have a plaintext server, please replace `Secure` with `Plain`.
-- It also assumes the accounts you may want to log in as all have
-- `objectClass` "Person".
--
-- To run the example you have to provide a bunch of environment variables:
--
--   - `HOST` is the LDAP host to connect to (without "ldap://", "ldaps://", etc).
--   - `POST` is the port LDAP server listens on.
--   - `MANAGER_DN` is the DN of the account the first bind is made with.
--   - `MANAGER_PASSWORD` is its password.
--   - `BASE_OBJECT` is the search root
module Main (main) where

import           Control.Exception (bracket_) -- base
import           Control.Monad (when)         -- base
import           Data.Function (fix)          -- base
import           Data.Text (Text)             -- text
import qualified Data.Text.Encoding as Text   -- text
import qualified Data.Text.IO as Text         -- text
import           Env                          -- envparse
import           Ldap.Client as Ldap          -- ldap-client
import qualified Ldap.Client.Bind as Ldap     -- ldap-client
import           System.Exit (die)            -- base
import qualified System.IO as IO              -- base


data Conf = Conf
  { host     :: String
  , port     :: PortNumber
  , dn       :: Dn
  , password :: Password
  , base     :: Dn
  } deriving (Show, Eq)

getConf :: IO Conf
getConf = Env.parse (header "LDAP login example") $ Conf
  <$> var str                        "HOST"             (help "LDAP hostname")
  <*> var (fmap fromIntegral . auto) "PORT"             (help "LDAP port")
  <*> var (fmap Ldap.Dn . str)       "MANAGER_DN"       (help "Manager login DN")
  <*> var (fmap Ldap.Password . str) "MANAGER_PASSWORD" (help "Manager password")
  <*> var (fmap Ldap.Dn . str)       "BASE_OBJECT"      (help "Search root")

main :: IO ()
main = do
  conf <- getConf
  res  <- login conf
  case res of
    Left  e -> die (show e)
    Right _ -> return ()

login :: Conf -> IO (Either LdapError ())
login conf =
  Ldap.with (Ldap.Secure (host conf)) (port conf) $ \l -> do
    Ldap.bind l (dn conf) (password conf)
    fix $ \loop -> do
      uid <- prompt "Username: "
      us  <- Ldap.search l (base conf)
                           (typesOnly True)
                           (And [ Attr "objectClass" := "Person"
                                , Attr "uid" := Text.encodeUtf8 uid
                                ])
                           []
      case us of
        SearchEntry udn _ : _ ->
          fix $ \loop' -> do
            pwd <- bracket_ hideOutput
                            showOutput
                            (do pwd <- prompt ("Password for ‘" <> uid <> "’: ")
                                Text.putStr "\n"
                                return pwd)
            res <- Ldap.bindEither l udn (Password (Text.encodeUtf8 pwd))
            case res of
              Left  _ -> do again <- question "Invalid password. Try again? [y/n] "
                            when again loop'
              Right _ -> Text.putStrLn "OK"
        [] -> do again <- question "Invalid username. Try again? [y/n] "
                 when again loop

prompt :: Text -> IO Text
prompt msg = do Text.putStr msg; IO.hFlush IO.stdout; Text.getLine

question :: Text -> IO Bool
question msg = fix $ \loop -> do
  res <- prompt msg
  case res of
    "y" -> return True
    "n" -> return False
    _   -> do Text.putStrLn "Please, answer either ‘y’ or ‘n’."; loop

hideOutput, showOutput :: IO ()
hideOutput = IO.hSetEcho IO.stdout False
showOutput = IO.hSetEcho IO.stdout True
