module Main where

import Options.Applicative
import Myriad

data Args = Args
    { configPath :: FilePath
    , languagesDir :: FilePath
    }

parseArgs :: IO Args
parseArgs = execParser $ info (helper <*> args) (fullDesc <> progDesc "Run the Myriad server")
    where
        args = Args
            <$> option str (mconcat
                [ long "config"
                , short 'c'
                , help "Set the myriad configuration"
                , metavar "PATH"
                , value "./config.yaml"
                , showDefault
                ])
            <*> option str (mconcat
                [ long "languages"
                , short 'l'
                , help "Set the languages directory"
                , metavar "DIR"
                , value "./languages/"
                , showDefault
                ])

main :: IO ()
main = do
    Args { configPath, languagesDir } <- parseArgs
    runMyriadServer configPath languagesDir
