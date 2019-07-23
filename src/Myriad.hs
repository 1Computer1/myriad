module Myriad
    ( runMyriadServer
    ) where

import qualified Data.Text as T

import Network.Wai.Handler.Warp (run)

import Myriad.Core
import Myriad.Docker
import Myriad.Server

runMyriadServer :: T.Text -> IO ()
runMyriadServer configInput = do
    env <- initEnv configInput
    runMyriadT env do
        buildAllImages
        startCleanup
        logInfo ["Finished Docker-related setup"]
        logInfo ["Starting server"]
    run (fromIntegral . port . config $ env) $ app env
