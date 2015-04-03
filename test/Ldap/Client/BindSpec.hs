{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.BindSpec (spec) where

import           Test.Hspec
import qualified Ldap.Asn1.Type as Ldap.Type
import           Ldap.Client as Ldap

import           SpecHelper (locally)


spec :: Spec
spec = do
  it "binds as ‘admin’" $ do
    res <- locally $ \l -> do
      Ldap.bind l (Dn "cn=admin") (Password "secret")
    res `shouldBe` Right ()

  it "tries to bind as ‘admin’ with the wrong password, unsuccessfully" $ do
    res <- locally $ \l -> do
      Ldap.bind l (Dn "cn=admin") (Password "public")
    res `shouldBe` Left
      (Ldap.ResponseError
        (Ldap.ResponseErrorCode
          (Ldap.Type.BindRequest 3
                                 (Ldap.Type.LdapDn (Ldap.Type.LdapString "cn=admin"))
                                 (Ldap.Type.Simple "public"))
          Ldap.InvalidCredentials
          (Dn "cn=admin")
          "Invalid Credentials"))

  it "binds as ‘pikachu’" $ do
    res <- locally $ \l -> do
      Ldap.bind l (Dn "cn=admin") (Password "secret")
      Ldap.SearchEntry udn _ : []
        <- Ldap.search l (Dn "o=localhost")
                         (scope WholeSubtree)
                         (Attr "cn" := "pikachu")
                         []
      Ldap.bind l udn (Password "i-choose-you")
    res `shouldBe` Right ()
