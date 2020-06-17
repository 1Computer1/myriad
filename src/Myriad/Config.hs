{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

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

import Optics

type LanguageName = T.Text

data Language = Language
    { _name :: LanguageName
    , _memory :: T.Text
    , _cpus :: Double
    , _timeout :: Int
    , _concurrent :: Int
    , _retries :: Int
    } deriving (Show)

makeFieldLabelsWith classUnderscoreNoPrefixFields ''Language

data Config = Config
    { _languages :: [Language]
    , _buildConcurrently :: Bool
    , _prepareContainers :: Bool
    , _cleanupInterval :: Int
    , _port :: Int
    } deriving (Show)

makeFieldLabelsWith classUnderscoreNoPrefixFields ''Config

data DefaultLanguage = DefaultLanguage
    { _memory :: T.Text
    , _cpus :: Double
    , _timeout :: Int
    , _concurrent :: Int
    , _retries :: Int
    } deriving (Show)

makeFieldLabelsWith classUnderscoreNoPrefixFields ''DefaultLanguage

instance FromYAML DefaultLanguage where
    parseYAML = withMap "default language" $ \m -> DefaultLanguage
        <$> m .: "memory"
        <*> m .: "cpus"
        <*> m .: "timeout"
        <*> m .: "concurrent"
        <*> m .: "retries"

data RawLanguage = RawLanguage
    { _name :: LanguageName
    , _memory :: Maybe T.Text
    , _cpus :: Maybe Double
    , _timeout :: Maybe Int
    , _concurrent :: Maybe Int
    , _retries :: Maybe Int
    } deriving (Show)

makeFieldLabelsWith classUnderscoreNoPrefixFields ''RawLanguage

instance FromYAML RawLanguage where
    parseYAML = withMap "language" $ \m -> RawLanguage
        <$> m .: "name"
        <*> m .:? "memory"
        <*> m .:? "cpus"
        <*> m .:? "timeout"
        <*> m .:? "concurrent"
        <*> m .:? "retries"

data RawConfig = RawConfig
    { _languages :: [RawLanguage]
    , _defaultLanguage :: DefaultLanguage
    , _buildConcurrently :: Bool
    , _prepareContainers :: Bool
    , _cleanupInterval :: Int
    , _port :: Int
    } deriving (Show)

makeFieldLabelsWith classUnderscoreNoPrefixFields ''RawConfig

instance FromYAML RawConfig where
    parseYAML = withMap "config" $ \m -> RawConfig
        <$> m .: "languages"
        <*> m .: "defaultLanguage"
        <*> m .: "buildConcurrently"
        <*> m .: "prepareContainers"
        <*> m .: "cleanupInterval"
        <*> m .: "port"

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
        { _languages = map (fromRawLanguage (r ^. #defaultLanguage)) $ r ^. #languages
        , _buildConcurrently = r ^. #buildConcurrently
        , _prepareContainers = r ^. #prepareContainers
        , _cleanupInterval = r ^. #cleanupInterval
        , _port = r ^. #port
        }

fromRawLanguage :: DefaultLanguage -> RawLanguage -> Language
fromRawLanguage d r =
    Language
        { _name = r ^. #name
        , _memory = fromMaybe (d ^. #memory) (r ^. #memory)
        , _cpus = fromMaybe (d ^. #cpus) (r ^. #cpus)
        , _timeout = fromMaybe (d ^. #timeout) (r ^. #timeout)
        , _concurrent = fromMaybe (d ^. #concurrent) (r ^. #concurrent)
        , _retries = fromMaybe (d ^. #retries) (r ^. #retries)
        }
