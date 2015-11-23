{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.ModifySpec (spec) where

import           Test.Hspec
import qualified Ldap.Asn1.Type as Ldap.Type
import           Ldap.Client as Ldap

import           SpecHelper (locally, charizard, pikachu, raichu)


spec :: Spec
spec = do
  let go l f = Ldap.search l (Dn "o=localhost") (Ldap.typesOnly True) f []

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
      res <- locally $ \l ->
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

  context "add" $
    it "can feed ‘charizard’" $ do
      res <- locally $ \l -> do
        [x] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") x `shouldBe` Just ["fire", "flying"]

        Ldap.modify l charizard [Attr "type" `Add` ["fed"]]

        [y] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") y `shouldBe` Just ["fire", "flying", "fed"]
      res `shouldBe` Right ()

  context "replace" $
    it "can put ‘charizard’ to sleep" $ do
      res <- locally $ \l -> do
        [x] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") x `shouldBe` Just ["fire", "flying"]

        Ldap.modify l charizard [Attr "type" `Replace` ["sleeping"]]

        [y] <- go l (Attr "cn" := "charizard")
        lookupAttr (Attr "type") y `shouldBe` Just ["sleeping"]
      res `shouldBe` Right ()

  context "modify dn" $
    it "evolves ‘pikachu’ into ‘raichu’" $ do
      res <- locally $ \l -> do
        [] <- go l (Attr "cn" := "raichu")

        Ldap.modifyDn l pikachu (RelativeDn "cn=raichu") False Nothing
        Ldap.modify l raichu [Attr "evolution" `Replace` ["1"]]

        [res] <- go l (Attr "cn" := "raichu")
        res `shouldBe`
          SearchEntry raichu
                      [ (Attr "cn",        ["raichu"])
                      , (Attr "evolution", ["1"])
                      , (Attr "type",      ["electric"])
                      , (Attr "password",  ["i-choose-you"])
                      ]
      res `shouldBe` Right ()

lookupAttr :: Attr -> SearchEntry -> Maybe [AttrValue]
lookupAttr a (SearchEntry _ as) = lookup a as
