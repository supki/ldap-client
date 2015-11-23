{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.DeleteSpec (spec) where

import           Test.Hspec

import           Ldap.Client (Dn(..), Filter(..), Attr(..))
import qualified Ldap.Client as Ldap
import qualified Ldap.Asn1.Type as Ldap.Type

import           SpecHelper (locally, dns, pikachu, oddish)


spec :: Spec
spec = do
  let go l f = Ldap.search l (Dn "o=localhost") (Ldap.typesOnly True) f []

  it "deletes an entry" $ do
    res <- locally $ \l -> do
      Ldap.delete l pikachu
      res <- go l (Attr "cn" := "pikachu")
      dns res `shouldBe` []
    res `shouldBe` Right ()

  it "tries to delete an non-existing entry, unsuccessfully" $ do
    res <- locally $ \l ->
      Ldap.delete l oddish
    res `shouldBe` Left
      (Ldap.ResponseError
        (Ldap.ResponseErrorCode (Ldap.Type.DeleteRequest
                                  (Ldap.Type.LdapDn (Ldap.Type.LdapString "cn=oddish,o=localhost")))
                                Ldap.NoSuchObject
                                (Dn "o=localhost")
                                "cn=oddish,o=localhost"))
