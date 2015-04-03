{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.CompareSpec (spec) where

import           Test.Hspec
import qualified Ldap.Asn1.Type as Ldap.Type
import           Ldap.Client as Ldap

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
    res `shouldBe` Left
      (Ldap.ResponseError
        (Ldap.ResponseErrorCode
          (Ldap.Type.CompareRequest
                                 (Ldap.Type.LdapDn (Ldap.Type.LdapString "cn=nope"))
                                 (Ldap.Type.AttributeValueAssertion
                                    (Ldap.Type.AttributeDescription (Ldap.Type.LdapString "type"))
                                    (Ldap.Type.AssertionValue "flying")))
          Ldap.NoSuchObject
          (Dn "")
          "No tree found for: cn=nope"))
