{-# LANGUAGE OverloadedStrings #-}
module SpecHelper
  ( locally
  , port
  , dns
  -- * Users
  , bulbasaur
  , ivysaur
  , venusaur
  , charmander
  , charmeleon
  , charizard
  , squirtle
  , wartortle
  , blastoise
  , caterpie
  , metapod
  , butterfree
  , pikachu
  , raichu
  , vulpix
  , oddish
  ) where

import Control.Exception (bracket)
import System.Environment (getEnvironment)
import System.IO (hGetLine)
import System.Process (runInteractiveProcess, terminateProcess, waitForProcess)

import Ldap.Client as Ldap


locally :: (Ldap -> IO a) -> IO (Either LdapError a)
locally f =
  bracket (do env <- getEnvironment
              (_, out, _, h) <- runInteractiveProcess "./test/ldap.js" [] Nothing
                                  (Just (("PORT", show port) :
                                         ("SSL_CERT", "./ssl/cert.pem") :
                                         ("SSL_KEY", "./ssl/key.pem") :
                                         env))
              hGetLine out
              return h)
          (\h -> do terminateProcess h
                    waitForProcess h)
          (\_ -> Ldap.with localhost port f)

localhost :: Host
localhost = Insecure "localhost"

port :: Num a => a
port = 24620

dns :: [SearchEntry] -> [Dn]
dns (SearchEntry dn _ : es) = dn : dns es
dns [] = []

bulbasaur :: Dn
bulbasaur = Dn "cn=bulbasaur,o=localhost"

ivysaur :: Dn
ivysaur = Dn "cn=ivysaur,o=localhost"

venusaur :: Dn
venusaur = Dn "cn=venusaur,o=localhost"

charmander :: Dn
charmander = Dn "cn=charmander,o=localhost"

charmeleon :: Dn
charmeleon = Dn "cn=charmeleon,o=localhost"

charizard :: Dn
charizard = Dn "cn=charizard,o=localhost"

squirtle :: Dn
squirtle = Dn "cn=squirtle,o=localhost"

wartortle :: Dn
wartortle = Dn "cn=wartortle,o=localhost"

blastoise :: Dn
blastoise = Dn "cn=blastoise,o=localhost"

caterpie :: Dn
caterpie = Dn "cn=caterpie,o=localhost"

metapod :: Dn
metapod = Dn "cn=metapod,o=localhost"

butterfree :: Dn
butterfree = Dn "cn=butterfree,o=localhost"

pikachu :: Dn
pikachu = Dn "cn=pikachu,o=localhost"

raichu :: Dn
raichu = Dn "cn=raichu,o=localhost"

vulpix :: Dn
vulpix = Dn "cn=vulpix,o=localhost"

oddish :: Dn
oddish = Dn "cn=oddish,o=localhost"
