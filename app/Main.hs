module Main where

import qualified Data.Text as T

import Options.Applicative

import Myriad

data Args = Args
    { configInput :: T.Text
    , languagesDir :: T.Text
    }

parseArgs :: IO Args
parseArgs = execParser $ info (helper <*> args) (fullDesc <> progDesc "Run the Myriad server")
    where
        args = Args
            <$> option str (mconcat
                [ long "config"
                , short 'c'
                , help "Set the Dhall configuration"
                , metavar "DHALL"
                ])
            <*> option str (mconcat
                [ long "languages"
                , short 'l'
                , help "Set the languages directory"
                , metavar "DIR"
                ])

main :: IO ()
main = do
    Args { configInput, languagesDir } <- parseArgs
    runMyriadServer configInput languagesDir
