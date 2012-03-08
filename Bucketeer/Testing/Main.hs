module Main (main) where

import Bucketeer.Util
import qualified Bucketeer.Testing.Persistence as P (specs)

import Control.Applicative
import Database.Redis (connect,
                       defaultConnectInfo)
import Test.Hspec (hspecX)

main :: IO ()
main = do conn <- connect defaultConnectInfo
          hspecX $ P.specs conn
