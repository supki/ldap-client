{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.SearchSpec (spec) where

import qualified Data.List.NonEmpty as NonEmpty
import           Test.Hspec
import           Ldap.Client as Ldap
import qualified Ldap.Asn1.Type as Ldap.Type

import           SpecHelper
  ( locally
  , dns
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
  )


spec :: Spec
spec = do
  let go l f = Ldap.search l (Dn "o=localhost") (Ldap.typesOnly True) f []

  it "cannot search as ‘pikachu’" $ do
    res <- locally $ \l -> do
      Ldap.bind l pikachu (Password "i-choose-you")
      go l (Present (Attr "password"))
    let req = Ldap.Type.SearchRequest
          (Ldap.Type.LdapDn (Ldap.Type.LdapString "o=localhost"))
          Ldap.Type.WholeSubtree
          Ldap.Type.NeverDerefAliases
          0
          0
          True
          (Ldap.Type.Present (Ldap.Type.AttributeDescription (Ldap.Type.LdapString "password")))
          (Ldap.Type.AttributeSelection [])
    res `shouldBe` Left
      (Ldap.ResponseError
        (Ldap.ResponseErrorCode req
                                Ldap.InsufficientAccessRights
                                (Dn "o=localhost")
                                "InsufficientAccessRightsError"))

  it "‘present’ filter" $ do
    res <- locally $ \l -> do
      res <- go l (Present (Attr "password"))
      dns res `shouldBe` [pikachu]
    res `shouldBe` Right ()

  it "‘equality’ filter" $ do
    res <- locally $ \l -> do
      res <- go l (Attr "type" := "flying")
      dns res `shouldMatchList`
        [ butterfree
        , charizard
        ]
    res `shouldBe` Right ()

  it "‘and’ filter" $ do
    res <- locally $ \l -> do
      res <- go l (And (NonEmpty.fromList [ Attr "type" := "fire"
                                          , Attr "evolution" := "1"
                                          ]))
      dns res `shouldBe` [charmeleon]
    res `shouldBe` Right ()

  it "‘or’ filter" $ do
    res <- locally $ \l -> do
      res <- go l (Or (NonEmpty.fromList [ Attr "type" := "fire"
                                         , Attr "evolution" := "1"
                                         ]))
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
      res <- go l (Attr "evolution" :>= "2")
      dns res `shouldMatchList`
        [ venusaur
        , charizard
        , blastoise
        , butterfree
        ]
    res `shouldBe` Right ()

  it "‘le’ filter" $ do
    res <- locally $ \l -> do
      res <- go l (Attr "evolution" :<= "0")
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
      res <- go l (Not (Or (NonEmpty.fromList [ Attr "type" := "fire"
                                              , Attr "evolution" :>= "1"
                                              ])))
      dns res `shouldMatchList`
        [ bulbasaur
        , squirtle
        , caterpie
        , pikachu
        ]
    res `shouldBe` Right ()

  it "‘substrings’ filter" $ do
    res <- locally $ \l -> do
      x <- go l (Attr "cn" :=* (Just "char", [], Nothing))
      dns x `shouldMatchList`
        [ charmander
        , charmeleon
        , charizard
        ]
      y <- go l (Attr "cn" :=* (Nothing, [], Just "saur"))
      dns y `shouldMatchList`
        [ bulbasaur
        , ivysaur
        , venusaur
        ]
      z <- go l (Attr "cn" :=* (Nothing, ["a", "o"], Just "e"))
      dns z `shouldMatchList`
        [ blastoise
        , wartortle
        ]
    res `shouldBe` Right ()

  it "‘approximate’ filter (actually, another ‘equality’ filter)" $ do
    res <- locally $ \l -> do
      res <- go l (Attr "type" :~= "flying")
      dns res `shouldMatchList`
        [ butterfree
        , charizard
        ]
    res `shouldBe` Right ()

  it "‘extensible’ filter" $ do
    res <- locally $ \l -> do
      res <- go l ((Just (Attr "type"), Nothing, True) ::= "flying")
      dns res `shouldMatchList`
        [ butterfree
        , charizard
        ]
    res `shouldBe` Right ()
