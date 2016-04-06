module Main where

import AS.Prelude
import Prelude()

import AS.Config.Settings
import AS.Kernels.R.Server (runServer)

import Control.Exception
import qualified Language.R.Instance as R

num_initial_workers = 50

main :: IO ()
main = alphaMain $ R.withEmbeddedR R.defaultConfig $ do
  addr <- getSetting rkernelAddress_server
  runServer addr num_initial_workers