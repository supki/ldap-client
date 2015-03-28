{-# LANGUAGE OverloadedStrings #-}
module Ldap.ClientSpec (spec) where

import           Data.Monoid ((<>))
import           Test.Hspec

import           Ldap.Client
import qualified Ldap.Client as Ldap


spec :: Spec
spec =
  context "Examples stolen from the LDAP package tests" $

    context "public LDAP server at MIT" $ do

      it "searches the whole tree for the entries that have ‘uid’ attribute" $ do
        res <- Ldap.with (Plain "scripts.mit.edu") 389 $ \l -> do
          res <- Ldap.search l (Dn "ou=People,dc=scripts,dc=mit,dc=edu")
                              (scope WholeSubtree <> typesOnly True)
                              (Present (Attr "uid"))
                              []
          res `shouldSatisfy` (not . null)
        res `shouldBe` Right ()

      it "searches the single level for the first 10 entries that have ‘uid’ attribute" $ do
        res <- Ldap.with (Plain "scripts.mit.edu") 389 $ \l -> do
          res <- Ldap.search l (Dn "ou=People,dc=scripts,dc=mit,dc=edu")
                              (scope WholeSubtree <> typesOnly True <> size 10)
                              (Present (Attr "uid"))
                              []
          length res `shouldBe` 10
        res `shouldBe` Right ()
