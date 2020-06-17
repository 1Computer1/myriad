{-# LANGUAGE UndecidableInstances #-}

module Myriad.Core
    ( Language
    , ContainerName
    , ImageName
    , EvalResult(..)
    , Env(..)
    , MyriadConfig(..)
    , DefaultLanguageConfig(..)
    , LanguageConfig(..)
    , MyriadT
    , runMyriadT
    , initEnv
    , exec
    , exec_
    , logInfo
    , logError
    , mapMVar
    , writeMVar
    ) where

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger hiding (logError, logInfo)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Snowflake
import qualified Data.Text as T
import Data.YAML

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.QSem.Lifted
import System.Process.Typed

type Language = T.Text
type ContainerName = String
type ImageName = String

data EvalResult = EvalOk BL.ByteString | EvalTimedOut | EvalErrored

data Env = Env
    { config :: MyriadConfig
    , languagesDir :: FilePath
    , containers :: MVar (M.Map Language ContainerName)
    , containerSems :: MVar (M.Map Language QSem)
    , evalSems :: MVar (M.Map Language QSem)
    , snowflakeGen :: SnowflakeGen
    }

data MyriadConfig = MyriadConfig
    { languages :: [LanguageConfig]
    , defaultLanguage :: DefaultLanguageConfig
    , buildConcurrently :: Bool
    , prepareContainers :: Bool
    , cleanupInterval :: Int
    , port :: Int
    } deriving (Show)

instance FromYAML MyriadConfig where
    parseYAML = withMap "config" $ \m -> MyriadConfig
        <$> m .: "languages"
        <*> m .: "defaultLanguage"
        <*> m .: "buildConcurrently"
        <*> m .: "prepareContainers"
        <*> m .: "cleanupInterval"
        <*> m .: "port"

data DefaultLanguageConfig = DefaultLanguageConfig
    { defMemory :: T.Text
    , defCpus :: Double
    , defTimeout :: Int
    , defConcurrent :: Int
    , defRetries :: Int
    } deriving (Show)

instance FromYAML DefaultLanguageConfig where
    parseYAML = withMap "default language" $ \m -> DefaultLanguageConfig
        <$> m .: "memory"
        <*> m .: "cpus"
        <*> m .: "timeout"
        <*> m .: "concurrent"
        <*> m .: "retries"

data LanguageConfig = LanguageConfig
    { name :: Language
    , memory :: Maybe T.Text
    , cpus :: Maybe Double
    , timeout :: Maybe Int
    , concurrent :: Maybe Int
    , retries :: Maybe Int
    } deriving (Show)

instance FromYAML LanguageConfig where
    parseYAML = withMap "language" $ \m -> LanguageConfig
        <$> m .: "name"
        <*> m .:? "memory"
        <*> m .:? "cpus"
        <*> m .:? "timeout"
        <*> m .:? "concurrent"
        <*> m .:? "retries"

newtype MyriadT m a = MyriadT { unMyriadT :: ReaderT Env (LoggingT m) a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader Env
        , MonadLogger
        , MonadLoggerIO
        , MonadIO
        , MonadError e
        , MonadState s
        , MonadWriter w
        , MonadBase b
        )

instance MonadTrans MyriadT where
    lift = MyriadT . lift . lift 

instance MonadTransControl MyriadT where
    type StT MyriadT a = a
    liftWith = defaultLiftWith2 MyriadT unMyriadT
    restoreT = defaultRestoreT2 MyriadT

instance MonadBaseControl b m => MonadBaseControl b (MyriadT m) where
    type StM (MyriadT m) a = ComposeSt MyriadT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

readConfig :: FilePath -> IO MyriadConfig
readConfig f = do
    x <- BL.readFile f
    case decode1 x of
        Left (pos, e) -> error $ prettyPosWithSource pos x e
        Right y -> pure y

initEnv :: FilePath -> FilePath -> IO Env
initEnv configPath languagesDir =
        Env
    <$> readConfig configPath
    <*> pure languagesDir
    <*> newMVar M.empty
    <*> newMVar M.empty
    <*> newMVar M.empty
    <*> newSnowflakeGen defaultConfig 0

runMyriadT :: MonadIO m => Env -> MyriadT m a -> m a
runMyriadT env = runStdoutLoggingT . flip runReaderT env . unMyriadT

exec :: MonadIO m => [String] -> m BL.ByteString
exec = readProcessInterleaved_ . shell . mconcat

exec_ :: MonadIO m => [String] -> m ()
exec_ = (() <$) . exec

logInfo :: MonadLogger m => [T.Text] -> m ()
logInfo = logInfoN . mconcat

logError :: MonadLogger m => [T.Text] -> m ()
logError = logErrorN . mconcat

mapMVar :: (MonadBase IO m, MonadBaseControl IO m) => MVar a -> (a -> a) -> m ()
mapMVar var f = modifyMVar_ var (pure . f)

writeMVar :: (MonadBase IO m, MonadBaseControl IO m) => MVar a -> a -> m ()
writeMVar var x = mapMVar var $ const x
