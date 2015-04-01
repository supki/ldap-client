{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.BindSpec (spec) where

import Test.Hspec
import Ldap.Client as Ldap

import SpecHelper (locally)


spec :: Spec
spec = do
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
        <- Ldap.search l (Dn "o=localhost")
                         (scope WholeSubtree)
                         (Attr "cn" := "pikachu")
                         []
      Ldap.bind l udn (Password "i-choose-you")
    res `shouldBe` Right ()
