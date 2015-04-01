{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.CompareSpec (spec) where

import Test.Hspec
import Ldap.Client as Ldap

import SpecHelper (locally, charmander, charizard)


spec :: Spec
spec = do
  it "compares and wins" $ do
    res <- locally $ \l -> do
      res <- Ldap.compare l charizard (Attr "type") "fire"
      res `shouldBe` True
    res `shouldBe` Right ()

  it "compares and looses" $ do
    res <- locally $ \l -> do
      res <- Ldap.compare l charmander (Attr "type") "flying"
      res `shouldBe` False
    res `shouldBe` Right ()

  it "tries to compare non-existing object, unsuccessfully" $ do
    res <- locally $ \l -> do
      res <- Ldap.compare l (Dn "cn=nope") (Attr "type") "flying"
      res `shouldBe` False
    res `shouldBe` Left (CompareError (CompareErrorCode NoSuchObject))
