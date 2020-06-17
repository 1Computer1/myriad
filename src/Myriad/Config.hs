module Myriad.Config
    ( LanguageName
    , Config(..)
    , Language(..)
    , readConfig
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import qualified Data.Text as T
import Data.YAML

type LanguageName = T.Text

data Config = Config
    { languages :: [Language]
    , buildConcurrently :: Bool
    , prepareContainers :: Bool
    , cleanupInterval :: Int
    , port :: Int
    } deriving (Show)

data Language = Language
    { name :: LanguageName
    , memory :: T.Text
    , cpus :: Double
    , timeout :: Int
    , concurrent :: Int
    , retries :: Int
    } deriving (Show)

fromRawConfig :: RawConfig -> Config
fromRawConfig (r@RawConfig { rawLanguages, rawDefaultLanguage }) =
    Config
        { languages = map f rawLanguages
        , buildConcurrently = rawBuildConcurrently r
        , prepareContainers = rawPrepareContainers r
        , cleanupInterval = rawCleanupInterval r
        , port = rawPort r
        }
    where
        f :: RawLanguage -> Language
        f l =
            Language
                { name = rawName l
                , memory = fromMaybe (defMemory rawDefaultLanguage) (rawMemory l)
                , cpus = fromMaybe (defCpus rawDefaultLanguage) (rawCpus l)
                , timeout = fromMaybe (defTimeout rawDefaultLanguage) (rawTimeout l)
                , concurrent = fromMaybe (defConcurrent rawDefaultLanguage) (rawConcurrent l)
                , retries = fromMaybe (defRetries rawDefaultLanguage) (rawRetries l)
                }

data RawConfig = RawConfig
    { rawLanguages :: [RawLanguage]
    , rawDefaultLanguage :: DefaultLanguage
    , rawBuildConcurrently :: Bool
    , rawPrepareContainers :: Bool
    , rawCleanupInterval :: Int
    , rawPort :: Int
    } deriving (Show)

instance FromYAML RawConfig where
    parseYAML = withMap "config" $ \m -> RawConfig
        <$> m .: "languages"
        <*> m .: "defaultLanguage"
        <*> m .: "buildConcurrently"
        <*> m .: "prepareContainers"
        <*> m .: "cleanupInterval"
        <*> m .: "port"

data DefaultLanguage = DefaultLanguage
    { defMemory :: T.Text
    , defCpus :: Double
    , defTimeout :: Int
    , defConcurrent :: Int
    , defRetries :: Int
    } deriving (Show)

instance FromYAML DefaultLanguage where
    parseYAML = withMap "default language" $ \m -> DefaultLanguage
        <$> m .: "memory"
        <*> m .: "cpus"
        <*> m .: "timeout"
        <*> m .: "concurrent"
        <*> m .: "retries"

data RawLanguage = RawLanguage
    { rawName :: LanguageName
    , rawMemory :: Maybe T.Text
    , rawCpus :: Maybe Double
    , rawTimeout :: Maybe Int
    , rawConcurrent :: Maybe Int
    , rawRetries :: Maybe Int
    } deriving (Show)

instance FromYAML RawLanguage where
    parseYAML = withMap "language" $ \m -> RawLanguage
        <$> m .: "name"
        <*> m .:? "memory"
        <*> m .:? "cpus"
        <*> m .:? "timeout"
        <*> m .:? "concurrent"
        <*> m .:? "retries"

readConfig :: FilePath -> IO Config
readConfig = fmap fromRawConfig . readRawConfig

readRawConfig :: FilePath -> IO RawConfig
readRawConfig f = do
    x <- BL.readFile f
    case decode1 x of
        Left (pos, e) -> error $ prettyPosWithSource pos x e
        Right y -> pure y
