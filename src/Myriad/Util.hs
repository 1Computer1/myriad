module Myriad.Util
    ( newContainerName
    , imageName
    , cvs
    , when_
    , unless_
    ) where

import Control.Monad.Reader

import Data.Snowflake
import Data.String.Conversions

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
