module Myriad.Config
    ( LanguageName
    , Config(..)
    , Language(..)
    , readConfig
    ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import qualified Data.Text as T
import           Data.YAML

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

readConfig :: FilePath -> IO Config
readConfig = fmap fromRawConfig . readRawConfig

readRawConfig :: FilePath -> IO RawConfig
readRawConfig f = do
    x <- BL.readFile f
    case decode1 x of
        Left (pos, e) -> error $ prettyPosWithSource pos x e
        Right y -> pure y

fromRawConfig :: RawConfig -> Config
fromRawConfig r =
    Config
        { languages = map (fromRawLanguage $ rawDefaultLanguage r) $ rawLanguages r
        , buildConcurrently = rawBuildConcurrently r
        , prepareContainers = rawPrepareContainers r
        , cleanupInterval = rawCleanupInterval r
        , port = rawPort r
        }

fromRawLanguage :: DefaultLanguage -> RawLanguage -> Language
fromRawLanguage d l =
    Language
        { name = rawName l
        , memory = fromMaybe (defMemory d) (rawMemory l)
        , cpus = fromMaybe (defCpus d) (rawCpus l)
        , timeout = fromMaybe (defTimeout d) (rawTimeout l)
        , concurrent = fromMaybe (defConcurrent d) (rawConcurrent l)
        , retries = fromMaybe (defRetries d) (rawRetries l)
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
