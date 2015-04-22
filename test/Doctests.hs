module Main (main) where

import Test.DocTest (doctest)


main :: IO ()
main = doctest ["src//Ldap/Asn1/ToAsn1.hs"]
