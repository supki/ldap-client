module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket)
import           System.IO (hGetLine)
import           System.Process (runInteractiveProcess, terminateProcess, waitForProcess)
import           Test.Hspec

import qualified Spec
import           SpecHelper (port)


main :: IO ()
main =
  bracket (do (_, out, _, h) <- runInteractiveProcess "./test/ldap.js" [] Nothing
                                  (Just [ ("PORT", show port)
                                        , ("SSL_CERT", "./ssl/cert.pem")
                                        , ("SSL_KEY", "./ssl/key.pem")
                                        ])
              hGetLine out
              return h)
          (\h -> do terminateProcess h
                    waitForProcess h)
          (\_ -> hspec Spec.spec)
