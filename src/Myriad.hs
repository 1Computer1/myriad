module Myriad
    ( runMyriadServer
    ) where

import Control.Monad.Logger (runStdoutLoggingT)

import Data.String.Conversions

import Network.Wai.Handler.Warp

import Optics

import Myriad.Core
import Myriad.Docker
import Myriad.Server

runMyriadServer :: FilePath -> FilePath -> IO ()
runMyriadServer configPath languagesDir = do
    env <- initEnv configPath languagesDir
    runMyriadT env $ do
        buildAllImages
        startCleanup
        logInfo ["Finished Docker-related setup"]
    let myriadPort = fromIntegral $ env ^. #config % #port
        onReady = runStdoutLoggingT $ logInfo ["Server started on port ", cs $ show myriadPort, "!"]
        settings = setPort myriadPort . setBeforeMainLoop onReady $ defaultSettings
    runSettings settings $ app env
