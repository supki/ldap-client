{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.ExtendedSpec (spec) where

import           Test.Hspec

import           Ldap.Client as Ldap
import           Ldap.Client.Extended as Ldap
import qualified Ldap.Asn1.Type as Ldap.Type

import           SpecHelper (locally )


spec :: Spec
spec = do
  it "sends an extended request" $ do
    res <- locally $ \l ->
      Ldap.extended l (Oid "0") Nothing
    res `shouldBe` Left
      (ResponseError (ResponseErrorCode (Ldap.Type.ExtendedRequest (Ldap.Type.LdapOid "0") Nothing)
                                        ProtocolError
                                        (Dn "")
                                        "0 not supported"))

  it "sends a startTLS  request" $ do
    res <- locally $ \l ->
      Ldap.startTls l
    res `shouldBe` Left
      (ResponseError (ResponseErrorCode (Ldap.Type.ExtendedRequest (Ldap.Type.LdapOid "1.3.6.1.4.1.1466.20037")
                                                                   Nothing)
                                        ProtocolError
                                        (Dn "")
                                        "1.3.6.1.4.1.1466.20037 not supported"))
