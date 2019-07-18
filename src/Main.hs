module Main where

import qualified Data.Text as T
import Options.Applicative

import Network.Wai.Handler.Warp (run)

import Myriad.Core
import Myriad.Docker
import Myriad.Server

data Args = Args
    { configInput :: T.Text
    }

parseArgs :: IO Args
parseArgs = execParser $ info (helper <*> args) (fullDesc <> progDesc "Run the Myriad server")
    where
        args = Args <$> option str (mconcat
            [ long "config"
            , short 'c'
            , help "Sets the Dhall configuration"
            , metavar "DHALL"
            ])

main :: IO ()
main = do
    Args { configInput } <- parseArgs
    env <- initEnv configInput
    runMyriadT env do
        buildAllImages
        startCleanup
        logInfo ["Finished Docker-related setup"]
        logInfo ["Starting server"]
    run (fromIntegral . port . config $ env) $ app env
