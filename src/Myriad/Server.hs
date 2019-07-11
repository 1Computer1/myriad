{-# LANGUAGE DeriveAnyClass #-}

module Myriad.Server
    ( app
    ) where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader

import Data.Aeson
import Data.List (find)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics

import Control.Concurrent.Async.Lifted
import Data.IORef.Lifted
import Servant

import Myriad.Core
import Myriad.Docker
import Myriad.Util

data EvalRequest = EvalRequest { language :: T.Text, code :: String } deriving (Generic, FromJSON)
data EvalResponse = EvalResponse { result :: T.Text } deriving (Generic, ToJSON)

type API
    =    "languages" :> Get '[JSON] [T.Text]
    :<|> "eval" :> ReqBody '[JSON] EvalRequest :> Post '[JSON] EvalResponse
    :<|> "containers" :> Get '[JSON] [T.Text]
    :<|> "cleanup" :> Post '[JSON] [T.Text]

app :: Env -> Application
app = serve (Proxy @API) . server

server :: Env -> Server API
server env = hoistServer (Proxy @API) (runMyriadT env) serverT

serverT :: forall m. (MonadWithIO m, MonadError ServantErr m) => ServerT API (MyriadT m)
serverT = handleLanguages :<|> handleEval :<|> handleContainers :<|> handleCleanup
    where
        handleLanguages :: MyriadT m [T.Text]
        handleLanguages = do
            logInfoN $ mconcat ["GET /languages"]
            MyriadConfig { languages } <- asks config
            pure . map name $ languages

        handleEval :: EvalRequest -> MyriadT m EvalResponse
        handleEval EvalRequest { language, code } = do
            logInfoN $ mconcat ["POST /eval"]
            MyriadConfig { languages } <- asks config
            case find (\x -> name x == language) languages of
                Nothing  -> throwError $ err404 { errBody = "Language " <> cvs language <> " was not found" }
                Just cfg -> do
                    res <- withAsync (evalCode cfg 0 $ cvs code) wait
                    case res of
                        EvalErrored  -> throwError $ err500 { errBody = "Evaluation failed" }
                        EvalTimedOut -> throwError $ err504 { errBody = "Evaluation timed out" }
                        EvalOk xs    -> pure . EvalResponse $ cvs xs

        handleContainers :: MyriadT m [T.Text]
        handleContainers = do
            logInfoN $ mconcat ["GET /containers"]
            containers <- asks containers >>= readIORef
            pure . map cvs $ M.elems containers

        handleCleanup :: MyriadT m [T.Text]
        handleCleanup = do
            logInfoN $ mconcat ["POST /cleanup"]
            map cvs <$> killAllContainersMaybe
