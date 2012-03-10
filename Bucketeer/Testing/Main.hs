module Main (main) where

import Bucketeer.Util
import qualified Bucketeer.Testing.Persistence    as P (specs)
import qualified Bucketeer.Testing.Util           as U (specs)
import qualified Bucketeer.Testing.Manager        as M (specs)
import qualified Bucketeer.Testing.WebServer.Util as WU (specs)

import Control.Applicative
import Database.Redis (connect,
                       defaultConnectInfo)
import Test.Hspec (hspecX)

main :: IO ()
main = do conn <- connect defaultConnectInfo
          mspecs <- M.specs
          hspecX $ P.specs conn ++ U.specs ++ WU.specs ++ mspecs
