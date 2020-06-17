module Myriad.Server
    ( app
    ) where

import Control.Monad.Except
import Control.Monad.Reader

import           Data.Aeson
import           Data.List (find)
import qualified Data.Map as M
import           Data.String.Conversions
import qualified Data.Text as T
import           GHC.Generics

import Control.Concurrent.Async.Lifted
import Control.Concurrent.MVar.Lifted
import Servant

import Optics

import Myriad.Core
import Myriad.Docker

type Myriad = MyriadT Handler

data EvalRequest = EvalRequest
    { language :: T.Text
    , code :: String
    } deriving (Generic, FromJSON)

data EvalResponse = EvalResponse
    { result :: T.Text
    } deriving (Generic, ToJSON)

type API
    =    "languages" :> Get '[JSON] [T.Text]
    :<|> "eval" :> ReqBody '[JSON] EvalRequest :> Post '[JSON] EvalResponse
    :<|> "containers" :> Get '[JSON] [T.Text]
    :<|> "cleanup" :> Post '[JSON] [T.Text]

app :: Env -> Application
app = serve (Proxy @API) . server

server :: Env -> Server API
server env = hoistServer (Proxy @API) (runMyriadT env) serverT

serverT :: ServerT API Myriad
serverT = handleLanguages :<|> handleEval :<|> handleContainers :<|> handleCleanup
    where
        handleLanguages :: Myriad [T.Text]
        handleLanguages = do
            logInfo ["GET /languages"]
            languages <- gview $ #config % #languages
            pure $ map (^. #name) languages

        handleEval :: EvalRequest -> Myriad EvalResponse
        handleEval EvalRequest { language, code } = do
            logInfo ["POST /eval"]
            languages <- gview $ #config % #languages
            case find (\x -> x ^. #name == language) languages of
                Nothing  -> throwError $ err404 { errBody = "Language " <> cs language <> " was not found" }
                Just cfg -> do
                    env <- ask
                    res <- withAsync (liftIO . runMyriadT env . evalCode cfg 0 $ cs code) wait
                    case res of
                        EvalErrored  -> throwError $ err500 { errBody = "Evaluation failed" }
                        EvalTimedOut -> throwError $ err504 { errBody = "Evaluation timed out" }
                        EvalOk xs    -> pure . EvalResponse $ cs xs

        handleContainers :: Myriad [T.Text]
        handleContainers = do
            logInfo ["GET /containers"]
            containers <- gview #containers >>= readMVar
            pure . map cs $ M.elems containers

        handleCleanup :: Myriad [T.Text]
        handleCleanup = do
            logInfo ["POST /cleanup"]
            env <- ask
            liftIO $ map cs <$> runMyriadT env killContainers
