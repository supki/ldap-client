{-# LANGUAGE OverloadedStrings #-}
module Ldap.Client.AddSpec (spec) where

import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid ((<>))
import           Test.Hspec

import           Ldap.Client (Dn(..), Filter(..), Scope(..), Attr(..))
import qualified Ldap.Client as Ldap

import           SpecHelper (locally , dns , vulpix)


spec :: Spec
spec = do
  let go l f = Ldap.search l (Dn "o=localhost")
                             (Ldap.scope WholeSubtree <> Ldap.typesOnly True)
                             f
                             []

  it "adds an entry" $ do
    res <- locally $ \l -> do
      Ldap.add l vulpix
                 [ (Attr "cn",        NonEmpty.fromList ["vulpix"])
                 , (Attr "evolution", NonEmpty.fromList ["0"])
                 , (Attr "type",      NonEmpty.fromList ["fire"])
                 ]
      res <- go l (Attr "cn" := "vulpix")
      dns res `shouldBe` [vulpix]
    res `shouldBe` Right ()
