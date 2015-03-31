{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ldap.ClientSpec (spec) where

import           Data.Monoid ((<>))
import           Test.Hspec

import           Ldap.Client (Host(..), Dn(..), Password(..), Filter(..), Scope(..), scope, size, Attr(..), BindError(..), ResultCode(..))
import qualified Ldap.Client as Ldap


spec :: Spec
spec = do

  context "public LDAP server at MIT\
    \<https://github.com/ezyang/ldap-haskell/blob/371a200f14317f8943d2aebdcc56a09dac46c0ed/testsrc/Tests.hs>" $ do

    it "searches the whole tree for the entries that have ‘uid’ attribute" $ do
      Right () <- Ldap.with mit 389 $ \l -> do
        res <- Ldap.search l (Dn "ou=People,dc=scripts,dc=mit,dc=edu")
                             (scope WholeSubtree)
                             (Present (Attr "uid"))
                             []
        res `shouldSatisfy` (not . null)
      return ()

    it "searches the single level for the first 10 entries that have ‘uid’ attribute" $ do
      Right () <- Ldap.with mit 389 $ \l -> do
        res <- Ldap.search l (Dn "ou=People,dc=scripts,dc=mit,dc=edu")
                             (scope SingleLevel <> size 10)
                             (Present (Attr "uid"))
                             []
        length res `shouldBe` 10
      return ()

    it "searches the single level for the first 10 entries that do not have ‘uid’ attribute" $ do
      Right () <- Ldap.with mit 389 $ \l -> do
        res <- Ldap.search l (Dn "ou=People,dc=scripts,dc=mit,dc=edu")
                             (scope SingleLevel <> size 10)
                             (Not (Present (Attr "uid")))
                             []
        res `shouldBe` []
      return ()

  context "online LDAP test server \
    \<http://www.forumsys.com/tutorials/integration-how-to/ldap/online-ldap-test-server>" $ do

    context "bind" $ do
      it "can bind" $ do
        Right () <- Ldap.with forumsys 389 $ \l -> do
          Ldap.bind l (Dn "cn=read-only-admin,dc=example,dc=com")
                      (Password "password")
        return ()

      it "can try to bind with a wrong password" $ do
        Right () <- Ldap.with forumsys 389 $ \l -> do
          res <- Ldap.bindEither l (Dn "cn=read-only-admin,dc=example,dc=com")
                                   (Password "drowssap")
          res `shouldBe` Left (BindErrorCode InvalidCredentials)
        return ()

      it "can login as another user" $ do
        Right () <- Ldap.with forumsys 389 $ \l -> do
          Ldap.bind l (Dn "cn=read-only-admin,dc=example,dc=com")
                      (Password "password")
          Ldap.SearchEntry udn _ : _
              <- Ldap.search l (Dn "dc=example,dc=com")
                               (Ldap.scope WholeSubtree <> Ldap.typesOnly True)
                               (Attr "uid" := "euler")
                               []
          Ldap.bind l udn (Password "password")
        return ()

    context "search" $ do
      let search l f = Ldap.search l (Dn "dc=example,dc=com") (Ldap.scope WholeSubtree <> Ldap.typesOnly True) f []

      it "can use ‘present’ filter" $ do
        Right () <- Ldap.with forumsys 389 $ \l -> do
          res <- search l (Present (Attr "initials"))
          dns res `shouldMatchList` [Dn "uid=test,dc=example,dc=com"]
        return ()

      it "can use ‘equality match’ filter" $ do
        Right () <- Ldap.with forumsys 389 $ \l -> do
          res <- search l (Attr "sn" := "Tesla")
          dns res `shouldMatchList` [Dn "uid=tesla,dc=example,dc=com"]
        return ()

      it "can use ‘or’ filter" $ do
        Right () <- Ldap.with forumsys 389 $ \l -> do
          res <- search l (Or [ Attr "sn" := "Tesla"
                              , Attr "cn" := "Issac Newton" -- why the fuck "Issac"?
                              ])
          dns res `shouldMatchList` [Dn "uid=tesla,dc=example,dc=com", Dn "uid=newton,dc=example,dc=com"]
        return ()

      it "can use ‘and’ and ‘not’ filters" $ do
        Right () <- Ldap.with forumsys 389 $ \l -> do
          res <- search l (And [ Attr "uniqueMember" := "uid=tesla,dc=example,dc=com"
                               , Not (Attr "uniqueMember" := "uid=einstein,dc=example,dc=com")
                               ])
          dns res `shouldMatchList` [Dn "ou=italians,ou=scientists,dc=example,dc=com"]
        return ()

mit :: Host
mit = Plain "scripts.mit.edu"

forumsys :: Host
forumsys = Plain "ldap.forumsys.com"

dns :: [Ldap.SearchEntry] -> [Dn]
dns (Ldap.SearchEntry dn _ : es) = dn : dns es
dns [] = []
dns _  = error "?"
