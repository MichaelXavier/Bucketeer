module Main (main) where

import Bucketeer.Manager (startBucketManager)
import Bucketeer.Types
import Bucketeer.WebServer (BucketeerWeb(..))
import Bucketeer.Util
import qualified Bucketeer.Testing.Persistence    as P (specs)
import qualified Bucketeer.Testing.Util           as U (specs)
import qualified Bucketeer.Testing.Manager        as M (specs)
import qualified Bucketeer.Testing.Types          as T (specs)
import qualified Bucketeer.Testing.WebServer      as WS (specs)
import qualified Bucketeer.Testing.WebServer.Util as WU (specs)

import Control.Applicative
import Data.IORef (newIORef)
import Database.Redis (connect,
                       defaultConnectInfo)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Test.Hspec as HS
import qualified Test.Hspec.Monadic as HSM
import System.Exit (ExitCode(..),
                    exitWith)
import Yesod (toWaiApp)


main :: IO ()
main = do conn <- connect defaultConnectInfo
          bmRef <- newIORef =<< startBucketManager [] conn
          let foundation = BucketeerWeb conn bmRef
          app <- toWaiApp foundation
          let loggedApp = logStdoutDev app
          mspecs <- M.specs
          runSpecs [--HS.hspecB $ P.specs conn ++ U.specs ++ WU.specs ++ T.specs ++ mspecs,
                    HSM.hspecB $ WS.specs app]

---- Helpers
runSpecs :: [IO Bool]
            -> IO ()
runSpecs specs = exitWith . toExitCode . all id =<< sequence specs

toExitCode :: Bool
              -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1
