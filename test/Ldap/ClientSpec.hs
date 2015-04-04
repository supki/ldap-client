module Ldap.ClientSpec (spec) where

import Control.Exception (IOException, throwIO)
import Test.Hspec

import SpecHelper (locally)


spec :: Spec
spec =
  context "exceptions" $
    it "propagates unrelated ‘IOException’s through" $
      locally (\_ -> throwIO unrelated)
     `shouldThrow`
      (== unrelated)

unrelated :: IOException
unrelated = userError "unrelated"
