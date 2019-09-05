{-# LANGUAGE UndecidableInstances #-}

module Myriad.Core
    ( Language
    , ContainerName
    , ImageName
    , EvalResult(..)
    , Env(..)
    , MyriadConfig(..)
    , LanguageConfig(..)
    , MyriadT
    , Myriad
    , Myriadic
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
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Data.Snowflake
import qualified Data.Text as T
import Dhall
import GHC.Generics (Generic)

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.QSem.Lifted
import System.Process.Typed

type Language = T.Text
type ContainerName = String
type ImageName = String

data EvalResult = EvalOk BL.ByteString | EvalTimedOut | EvalErrored

data Env = Env
    { config :: MyriadConfig
    , languagesDir :: T.Text
    , containers :: MVar (M.Map Language ContainerName)
    , containerSems :: MVar (M.Map Language QSem)
    , evalSems :: MVar (M.Map Language QSem)
    , snowflakeGen :: SnowflakeGen
    }

data MyriadConfig = MyriadConfig
    { languages :: [LanguageConfig]
    , buildConcurrently :: Bool
    , prepareContainers :: Bool
    , cleanupInterval :: Natural
    , port :: Natural
    } deriving (Show, Generic)

instance Interpret MyriadConfig

data LanguageConfig = LanguageConfig
    { name :: Language
    , memory :: T.Text
    , cpus :: T.Text
    , timeout :: Natural
    , concurrent :: Natural
    , retries :: Natural
    } deriving (Show, Generic)

instance Interpret LanguageConfig

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

type Myriad = MyriadT Identity

type Myriadic m = (MonadReader Env m, MonadLogger m, MonadLoggerIO m, MonadIO m, MonadBase IO m, MonadBaseControl IO m)

readConfig :: T.Text -> IO MyriadConfig
readConfig = input auto

initEnv :: T.Text -> T.Text -> IO Env
initEnv configInput languagesDir =
        Env
    <$> readConfig configInput
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
exec_ = void . exec

logInfo :: MonadLogger m => [T.Text] -> m ()
logInfo = logInfoN . mconcat

logError :: MonadLogger m => [T.Text] -> m ()
logError = logErrorN . mconcat

mapMVar :: (MonadBase IO m, MonadBaseControl IO m) => MVar a -> (a -> a) -> m ()
mapMVar var f = modifyMVar_ var (pure . f)

writeMVar :: (MonadBase IO m, MonadBaseControl IO m) => MVar a -> a -> m ()
writeMVar var x = mapMVar var $ const x
