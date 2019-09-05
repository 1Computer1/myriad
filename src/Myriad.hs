module Myriad
    ( runMyriadServer
    ) where

import Control.Monad.Logger (runStdoutLoggingT)

import Data.String.Conversions
import qualified Data.Text as T

import Network.Wai.Handler.Warp

import Myriad.Core
import Myriad.Docker
import Myriad.Server

runMyriadServer :: T.Text -> T.Text -> IO ()
runMyriadServer configInput languagesDir = do
    env <- initEnv configInput languagesDir
    runMyriadT env do
        buildAllImages
        startCleanup
        logInfo ["Finished Docker-related setup"]
    let myriadPort = fromIntegral . port $ config env
        onReady = runStdoutLoggingT $ logInfo ["Server started on port ", cs $ show myriadPort, "!"]
        settings = setPort myriadPort . setBeforeMainLoop onReady $ defaultSettings
    runSettings settings $ app env
