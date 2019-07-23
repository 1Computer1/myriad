module Main where

import qualified Data.Text as T

import Options.Applicative

import Myriad

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
    runMyriadServer configInput
