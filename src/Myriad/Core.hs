{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Myriad.Core
    ( Language
    , ContainerName
    , ImageName
    , Env(..)
    , MyriadT
    , runMyriadT
    , initEnv
    , exec
    , exec_
    , logInfo
    , logDebug
    , logWarn
    , logError
    , mapMVar
    , writeMVar
    ) where

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger hiding (logError, logDebug, logWarn, logInfo)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.Snowflake
import           Data.String.Conversions
import qualified Data.Text as T

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.QSem.Lifted
import System.Process.Typed

import Optics

import Myriad.Config

type ContainerName = String

type ImageName = String

data Env = Env
    { _config :: Config
    , _languagesDir :: FilePath
    , _containers :: MVar (M.Map LanguageName ContainerName)
    , _containerSems :: MVar (M.Map LanguageName QSem)
    , _evalSems :: MVar (M.Map LanguageName QSem)
    , _snowflakeGen :: SnowflakeGen
    }

makeFieldLabelsWith classUnderscoreNoPrefixFields ''Env

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

exec :: (MonadIO m, MonadLogger m) => [String] -> m BL.ByteString
exec args = do
    logDebug ["Executing `", cs $ mconcat args, "`"]
    readProcessInterleaved_ . shell $ mconcat args

exec_ :: (MonadIO m, MonadLogger m) => [String] -> m ()
exec_ = void . exec

logInfo :: MonadLogger m => [T.Text] -> m ()
logInfo = logInfoN . mconcat

logDebug :: MonadLogger m => [T.Text] -> m ()
logDebug = logDebugN . mconcat

logWarn :: MonadLogger m => [T.Text] -> m ()
logWarn = logWarnN . mconcat

logError :: MonadLogger m => [T.Text] -> m ()
logError = logErrorN . mconcat

mapMVar :: (MonadBase IO m, MonadBaseControl IO m) => MVar a -> (a -> a) -> m ()
mapMVar var f = modifyMVar_ var (pure . f)

writeMVar :: (MonadBase IO m, MonadBaseControl IO m) => MVar a -> a -> m ()
writeMVar var x = mapMVar var $ const x
