{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ldap.ClientSpec (spec) where

import           Data.Monoid ((<>))
import           Test.Hspec

import           Ldap.Client (Dn(..), Filter(..), Scope(..), Attr(..))
import qualified Ldap.Client as Ldap

import           SpecHelper
  ( locally
  , dns
  , pikachu
  , vulpix
  , oddish
  )


spec :: Spec
spec = do
  let go l f = Ldap.search l (Dn "o=localhost")
                             (Ldap.scope WholeSubtree <> Ldap.typesOnly True)
                             f
                             []

  context "add" $ do

    it "adds an entry" $ do
      res <- locally $ \l -> do
        Ldap.add l vulpix
                   [ (Attr "cn",        ["vulpix"])
                   , (Attr "evolution", ["0"])
                   , (Attr "type",      ["fire"])
                   ]
        res <- go l (Attr "cn" := "vulpix")
        dns res `shouldBe` [vulpix]
      res `shouldBe` Right ()

  context "delete" $ do

    it "deletes an entry" $ do
      res <- locally $ \l -> do
        Ldap.delete l pikachu
        res <- go l (Attr "cn" := "pikachu")
        dns res `shouldBe` []
      res `shouldBe` Right ()

    it "tries to delete an unexisting entry, unsuccessfully" $ do
      res <- locally $ \l -> do
        Ldap.delete l oddish
      res `shouldBe` Left (Ldap.DeleteError (Ldap.DeleteErrorCode Ldap.NoSuchObject))
