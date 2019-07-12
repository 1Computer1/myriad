module Myriad.Util
    ( newContainerName
    , imageName
    , cvs
    , when_
    , unless_
    , exec
    , exec_
    , logInfo
    , logError
    , mapMVar
    , writeMVar
    ) where

import qualified Control.Monad.Logger as L
import Control.Monad.Reader

import qualified Data.ByteString.Lazy as BL
import Data.Snowflake
import Data.String.Conversions
import qualified Data.Text as T

import Control.Concurrent.MVar.Lifted
import System.Process.Typed

import Myriad.Core

newContainerName :: MonadIO m => LanguageConfig -> MyriadT m ContainerName
newContainerName LanguageConfig { name } = do
    snowflakeGen <- asks snowflakeGen
    snowflake <- liftIO $ nextSnowflake snowflakeGen
    pure $ "comp_iler-" <> convertString name <> "-" <> show snowflake

imageName :: LanguageConfig -> ImageName
imageName LanguageConfig { name } = "1computer1/comp_iler:" <> convertString name

-- Shorthand because laziness
cvs :: ConvertibleStrings a b => a -> b
cvs = convertString

when_ :: Applicative f => Bool -> f a -> f ()
when_ p = when p . void 

unless_ :: Applicative f => Bool -> f a -> f ()
unless_ p = unless p . void 

exec :: [String] -> MyriadIO BL.ByteString
exec = readProcessInterleaved_ . shell . mconcat

exec_ :: [String] -> MyriadIO ()
exec_ = void . exec

logInfo :: [T.Text] -> MyriadIO ()
logInfo = L.logInfoN . mconcat

logError :: [T.Text] -> MyriadIO ()
logError = L.logErrorN . mconcat

mapMVar :: MVar a -> (a -> a) -> MyriadIO ()
mapMVar var f = modifyMVar_ var (pure . f)

writeMVar :: MVar a -> a -> MyriadIO ()
writeMVar var x = mapMVar var $ const x
