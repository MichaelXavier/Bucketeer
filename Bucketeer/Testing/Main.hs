module Main (main) where

import Bucketeer.Manager (startBucketManager)
import Bucketeer.Types
import Bucketeer.WebServer (BucketeerWeb(..))
import Bucketeer.Util
import qualified Bucketeer.Testing.Persistence    as P (specs)
import qualified Bucketeer.Testing.Util           as U (specs)
import qualified Bucketeer.Testing.Manager        as M (specs)
import qualified Bucketeer.Testing.Types          as T (specs)
import qualified Bucketeer.Testing.WebServer      as WS (runSpecs)
import qualified Bucketeer.Testing.WebServer.Util as WU (specs)

import Control.Monad (when)
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
main = do conn   <- connect defaultConnectInfo
          mspecs <- M.specs
          runSpecs [HS.hspecB $ P.specs conn ++ U.specs ++ WU.specs ++ T.specs ++ mspecs]
          WS.runSpecs conn

---- Helpers
runSpecs :: [IO Bool]
            -> IO ()
runSpecs specs = do failed <- passed
                    when failed $ exitWith $ ExitFailure 1
  where passed :: IO Bool
        passed = return . all id . map not =<< sequence specs

toExitCode :: Bool
              -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1
