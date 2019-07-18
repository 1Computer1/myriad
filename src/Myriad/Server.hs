{-# LANGUAGE DeriveAnyClass #-}

module Myriad.Server
    ( app
    ) where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Aeson
import Data.List (find)
import qualified Data.Map as M
import Data.String.Conversions
import qualified Data.Text as T
import GHC.Generics

import Control.Concurrent.Async.Lifted
import Control.Concurrent.MVar.Lifted
import Servant

import Myriad.Core
import Myriad.Docker

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

serverT :: ServerT API (MyriadT Handler)
serverT = handleLanguages :<|> handleEval :<|> handleContainers :<|> handleCleanup
    where
        handleLanguages :: MyriadT Handler [T.Text]
        handleLanguages = do
            logInfo ["GET /languages"]
            MyriadConfig { languages } <- asks config
            pure . map name $ languages

        handleEval :: EvalRequest -> MyriadT Handler EvalResponse
        handleEval EvalRequest { language, code } = do
            logInfo ["POST /eval"]
            MyriadConfig { languages } <- asks config
            case find (\x -> name x == language) languages of
                Nothing  -> throwError $ err404 { errBody = "Language " <> cs language <> " was not found" }
                Just cfg -> do
                    res <- withAsync (evalCode cfg 0 $ cs code) wait
                    case res of
                        EvalErrored  -> throwError $ err500 { errBody = "Evaluation failed" }
                        EvalTimedOut -> throwError $ err504 { errBody = "Evaluation timed out" }
                        EvalOk xs    -> pure . EvalResponse $ cs xs

        handleContainers :: MyriadT Handler [T.Text]
        handleContainers = do
            logInfo ["GET /containers"]
            containers <- asks containers >>= readMVar
            pure . map cs $ M.elems containers

        handleCleanup :: MyriadT Handler [T.Text]
        handleCleanup = do
            logInfo ["POST /cleanup"]
            map cs <$> killContainers
