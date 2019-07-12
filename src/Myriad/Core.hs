module Myriad.Core
    ( Language
    , ContainerName
    , ImageName
    , EvalResult(..)
    , Env(..)
    , MyriadConfig(..)
    , LanguageConfig(..)
    , MyriadT
    , MonadWithIO
    , MyriadIO
    , runMyriadT
    , initEnv
    ) where

import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Dhall
import GHC.Generics (Generic)

import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Data.Snowflake

type Language = T.Text
type ContainerName = String
type ImageName = String

data EvalResult = EvalOk BL.ByteString | EvalTimedOut | EvalErrored

data Env = Env
    { config :: MyriadConfig
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

type MyriadT m = ReaderT Env (LoggingT m)

type MonadWithIO m = (MonadIO m, MonadBase IO m, MonadBaseControl IO m)

type MyriadIO a = forall m. MonadWithIO m => MyriadT m a

readConfig :: T.Text -> IO MyriadConfig
readConfig = input auto

initEnv :: T.Text -> IO Env
initEnv configInput =
        Env
    <$> readConfig configInput
    <*> newMVar M.empty
    <*> newMVar M.empty
    <*> newMVar M.empty
    <*> newSnowflakeGen defaultConfig 0

runMyriadT :: MonadIO m => Env -> MyriadT m a -> m a
runMyriadT env f = runStdoutLoggingT $ runReaderT f env
