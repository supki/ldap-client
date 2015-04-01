{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ldap.ClientSpec (spec) where

import           Data.Monoid ((<>))
import           Test.Hspec

import           Ldap.Client (Dn(..), Password(..), Filter(..), Scope(..), Attr(..))
import qualified Ldap.Client as Ldap

import           SpecHelper (port)


spec :: Spec
spec = do

  let locally = Ldap.with localhost port
      search l f = Ldap.search l (Dn "o=localhost")
                                 (Ldap.scope WholeSubtree <> Ldap.typesOnly True)
                                 f
                                 []

  context "bind" $ do

    it "binds as admin" $ do
      res <- locally $ \l -> do
        Ldap.bind l (Dn "cn=admin") (Password "secret")
      res `shouldBe` Right ()

    it "tries to bind as admin with the wrong password, unsuccessfully" $ do
      res <- locally $ \l -> do
        Ldap.bind l (Dn "cn=admin") (Password "public")
      res `shouldBe` Left (Ldap.BindError (Ldap.BindErrorCode Ldap.InvalidCredentials))

    it "binds as pikachu" $ do
      res <- locally $ \l -> do
        Ldap.bind l (Dn "cn=admin") (Password "secret")
        Ldap.SearchEntry udn _ : []
          <- search l (Attr "cn" := "pikachu")
        Ldap.bind l udn (Password "i-choose-you")
      res `shouldBe` Right ()

  context "search" $ do

    it "cannot search as ‘pikachu’" $ do
      res <- locally $ \l -> do
        Ldap.bind l pikachu (Password "i-choose-you")
        search l (Present (Attr "password"))
      res `shouldBe` Left (Ldap.SearchError (Ldap.SearchErrorCode Ldap.InsufficientAccessRights))

    it "‘present’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Present (Attr "password"))
        dns res `shouldBe` [pikachu]
      res `shouldBe` Right ()

    it "‘equality match’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Attr "type" := "flying")
        dns res `shouldMatchList`
          [ butterfree
          , charizard
          ]
      res `shouldBe` Right ()

    it "‘and’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (And [ Attr "type" := "fire"
                             , Attr "evolution" := "1"
                             ])
        dns res `shouldBe` [charmeleon]
      res `shouldBe` Right ()

    it "‘or’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Or [ Attr "type" := "fire"
                            , Attr "evolution" := "1"
                            ])
        dns res `shouldMatchList`
          [ ivysaur
          , charizard
          , charmeleon
          , charmander
          , wartortle
          , metapod
          ]
      res `shouldBe` Right ()

    it "‘ge’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Attr "evolution" :>= "2")
        dns res `shouldMatchList`
          [ venusaur
          , charizard
          , blastoise
          , butterfree
          ]
      res `shouldBe` Right ()

    it "‘le’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Attr "evolution" :<= "0")
        dns res `shouldMatchList`
          [ bulbasaur
          , charmander
          , squirtle
          , caterpie
          , pikachu
          ]
      res `shouldBe` Right ()

    it "‘not’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Not (Or [ Attr "type" := "fire"
                                 , Attr "evolution" :>= "1"
                                 ]))
        dns res `shouldMatchList`
          [ bulbasaur
          , squirtle
          , caterpie
          , pikachu
          ]
      res `shouldBe` Right ()

    it "‘substrings’ filter" $ do
      res <- locally $ \l -> do
        x <- search l (Attr "cn" :=* (Just "char", [], Nothing))
        dns x `shouldMatchList`
          [ charmander
          , charmeleon
          , charizard
          ]
        y <- search l (Attr "cn" :=* (Nothing, [], Just "saur"))
        dns y `shouldMatchList`
          [ bulbasaur
          , ivysaur
          , venusaur
          ]
        z <- search l (Attr "cn" :=* (Nothing, ["a", "o"], Just "e"))
        dns z `shouldMatchList`
          [ blastoise
          , wartortle
          ]
      res `shouldBe` Right ()

  context "add" $ do

    it "adds an entry" $ do
      res <- locally $ \l -> do
        Ldap.add l vulpix
                   [ (Attr "cn",        ["vulpix"])
                   , (Attr "evolution", ["0"])
                   , (Attr "type",      ["fire"])
                   ]
        res <- search l (Attr "cn" := "vulpix")
        dns res `shouldBe` [vulpix]
      res `shouldBe` Right ()

 where
  bulbasaur = Dn "cn=bulbasaur,o=localhost"
  ivysaur = Dn "cn=ivysaur,o=localhost"
  venusaur = Dn "cn=venusaur,o=localhost"
  charmander = Dn "cn=charmander,o=localhost"
  charmeleon = Dn "cn=charmeleon,o=localhost"
  charizard = Dn "cn=charizard,o=localhost"
  squirtle = Dn "cn=squirtle,o=localhost"
  wartortle = Dn "cn=wartortle,o=localhost"
  blastoise = Dn "cn=blastoise,o=localhost"
  caterpie = Dn "cn=caterpie,o=localhost"
  metapod = Dn "cn=metapod,o=localhost"
  butterfree = Dn "cn=butterfree,o=localhost"
  pikachu = Dn "cn=pikachu,o=localhost"
  vulpix = Dn "cn=vulpix,o=localhost"

localhost :: Ldap.Host
localhost = Ldap.Plain "localhost"

dns :: [Ldap.SearchEntry] -> [Dn]
dns (Ldap.SearchEntry dn _ : es) = dn : dns es
dns [] = []
dns _  = error "?"
