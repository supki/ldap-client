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
      search l f = Ldap.search l (Dn "o=localhost") (Ldap.scope WholeSubtree <> Ldap.typesOnly True) f []

  context "bind" $ do
    it "can bind" $ do
      res <- locally $ \l -> do
        Ldap.bind l (Dn "cn=admin") (Password "secret")
      res `shouldBe` Right ()

    it "can try to bind with a wrong password" $ do
      res <- locally $ \l -> do
        Ldap.bind l (Dn "cn=admin") (Password "public")
      res `shouldBe` Left (Ldap.BindError (Ldap.BindErrorCode Ldap.InvalidCredentials))

    it "can login as another user" $ do
      res <- locally $ \l -> do
        Ldap.bind l (Dn "cn=admin") (Password "secret")
        Ldap.SearchEntry udn _ : []
          <- search l (Attr "cn" := "pikachu")
        Ldap.bind l udn (Password "i-choose-you")
      res `shouldBe` Right ()

  context "search" $ do
    it "cannot search as ‘pikachu’" $ do
      res <- locally $ \l -> do
        Ldap.bind l (Dn "cn=pikachu,o=localhost") (Password "i-choose-you")
        search l (Present (Attr "password"))
      res `shouldBe` Left (Ldap.SearchError (Ldap.SearchErrorCode Ldap.InsufficientAccessRights))

    it "can use ‘present’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Present (Attr "password"))
        dns res `shouldBe` [Dn "cn=pikachu,o=localhost"]
      res `shouldBe` Right ()

    it "can use ‘equality match’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Attr "type" := "flying")
        dns res `shouldMatchList` [Dn "cn=butterfree,o=localhost", Dn "cn=charizard,o=localhost"]
      res `shouldBe` Right ()

    it "can use ‘and’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (And [ Attr "type" := "fire"
                             , Attr "evolution" := "1"
                             ])
        dns res `shouldBe` [Dn "cn=charmeleon,o=localhost"]
      res `shouldBe` Right ()

    it "can use ‘or’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Or [ Attr "type" := "fire"
                            , Attr "evolution" := "1"
                            ])
        dns res `shouldMatchList`
          [ Dn "cn=charizard,o=localhost"
          , Dn "cn=charmeleon,o=localhost"
          , Dn "cn=charmander,o=localhost"
          , Dn "cn=metapod,o=localhost"
          , Dn "cn=wartortle,o=localhost"
          , Dn "cn=ivysaur,o=localhost"
          ]
      res `shouldBe` Right ()

    it "can use ‘or’ filter" $ do
      res <- locally $ \l -> do
        res <- search l (Not (Or [ Attr "type" := "fire"
                                 , Attr "evolution" :>= "1"
                                 ]))
        dns res `shouldMatchList`
          [ Dn "cn=bulbasaur,o=localhost"
          , Dn "cn=squirtle,o=localhost"
          , Dn "cn=caterpie,o=localhost"
          , Dn "cn=pikachu,o=localhost"
          ]
      res `shouldBe` Right ()

localhost :: Ldap.Host
localhost = Ldap.Plain "localhost"

dns :: [Ldap.SearchEntry] -> [Dn]
dns (Ldap.SearchEntry dn _ : es) = dn : dns es
dns [] = []
dns _  = error "?"
