module Main (main) where

import Test.Hspec (hspecX)

import qualified Bucketeer.Testing.Persistence         as P  (specs)

main :: IO ()
main = hspecX $ P.specs
