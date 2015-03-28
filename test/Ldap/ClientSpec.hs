{-# LANGUAGE OverloadedStrings #-}
module Ldap.ClientSpec (spec) where

import           Data.Monoid ((<>))
import           Test.Hspec

import           Ldap.Client
import qualified Ldap.Client as Ldap


spec :: Spec
spec =
  context "Example stolen from the LDAP package tests" $
    it "searches the public LDAP server at MIT" $ do
      res <- Ldap.with (Plain "scripts.mit.edu") 389 $ \l -> do
        res <- Ldap.search l (Dn "ou=People,dc=scripts,dc=mit,dc=edu")
                             (scope WholeSubtree <> typesOnly True)
                             (Present (Attr "uid"))
                             []
        res `shouldSatisfy` (not . null)
      res `shouldBe` Right ()
