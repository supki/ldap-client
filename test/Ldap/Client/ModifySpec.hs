{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.ModifySpec (spec) where

import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import           Test.Hspec
import qualified Ldap.Asn1.Type as Ldap.Type
import           Ldap.Client as Ldap

import           SpecHelper (locally, charizard, pikachu)


spec :: Spec
spec = do
  let go l f = Ldap.search l (Dn "o=localhost")
                             (Ldap.scope WholeSubtree <> Ldap.typesOnly True)
                             f
                             []

  context "delete" $ do
    it "can land ‘charizard’" $ do
      res <- locally $ \l -> do
        [x] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") x `shouldBe` Just ["fire", "flying"]

        Ldap.modify l charizard [Attr "type" `Delete` ["flying"]]

        [y] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") y `shouldBe` Just ["fire"]
      res `shouldBe` Right ()

    it "tries to remove ‘pikachu’'s password, unsuccessfully" $ do
      res <- locally $ \l -> do
        Ldap.modify l pikachu [Attr "password" `Delete` []]
      res `shouldBe` Left
        (ResponseError
          (ResponseErrorCode
            (Ldap.Type.ModifyRequest (Ldap.Type.LdapDn (Ldap.Type.LdapString "cn=pikachu,o=localhost"))
                                     [( Ldap.Type.Delete
                                     , Ldap.Type.PartialAttribute
                                         (Ldap.Type.AttributeDescription (Ldap.Type.LdapString "password"))
                                         []
                                     )])
                                     UnwillingToPerform
                                     (Dn "o=localhost")
                                     "cannot delete password"))

  context "add" $ do
    it "can feed ‘charizard’" $ do
      res <- locally $ \l -> do
        [x] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") x `shouldBe` Just ["fire", "flying"]

        Ldap.modify l charizard [Attr "type" `Add` ["fed"]]

        [y] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") y `shouldBe` Just ["fire", "flying", "fed"]
      res `shouldBe` Right ()

  context "replace" $ do
    it "can put ‘charizard’ to sleep" $ do
      res <- locally $ \l -> do
        [x] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") x `shouldBe` Just ["fire", "flying"]

        Ldap.modify l charizard [Attr "type" `Replace` ["sleeping"]]

        [y] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") y `shouldBe` Just ["sleeping"]
      res `shouldBe` Right ()

lookupAttr :: Attr -> SearchEntry -> Maybe [ByteString]
lookupAttr a (SearchEntry _ as) = lookup a as
