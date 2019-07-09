module Main where

import Control.Monad.Logger

import Network.Wai.Handler.Warp (run)

import Myriad.Core
import Myriad.Docker
import Myriad.Server

main :: IO ()
main = do
    env <- initEnv "./config.dhall"
    runMyriadT env do
        buildAllImages
        startCleanup
    runStdoutLoggingT do
        logInfoN "Finished Docker-related setup"
        logInfoN "Starting server"
    run (fromIntegral . port . config $ env) $ app env
