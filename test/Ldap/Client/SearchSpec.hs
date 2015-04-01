{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.SearchSpec (spec) where

import Data.Monoid ((<>))
import Test.Hspec
import Ldap.Client as Ldap

import SpecHelper
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
  let go l f = Ldap.search l (Dn "o=localhost")
                             (Ldap.scope WholeSubtree <> Ldap.typesOnly True)
                             f
                             []

  it "cannot search as ‘pikachu’" $ do
    res <- locally $ \l -> do
      Ldap.bind l pikachu (Password "i-choose-you")
      go l (Present (Attr "password"))
    res `shouldBe` Left (Ldap.SearchError (Ldap.SearchErrorCode Ldap.InsufficientAccessRights))

  it "‘present’ filter" $ do
    res <- locally $ \l -> do
      res <- go l (Present (Attr "password"))
      dns res `shouldBe` [pikachu]
    res `shouldBe` Right ()

  it "‘equality match’ filter" $ do
    res <- locally $ \l -> do
      res <- go l (Attr "type" := "flying")
      dns res `shouldMatchList`
        [ butterfree
        , charizard
        ]
    res `shouldBe` Right ()

  it "‘and’ filter" $ do
    res <- locally $ \l -> do
      res <- go l (And [ Attr "type" := "fire"
                       , Attr "evolution" := "1"
                       ])
      dns res `shouldBe` [charmeleon]
    res `shouldBe` Right ()

  it "‘or’ filter" $ do
    res <- locally $ \l -> do
      res <- go l (Or [ Attr "type" := "fire"
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
      res <- go l (Not (Or [ Attr "type" := "fire"
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
