module Myriad.Docker
    ( EvalResult(..)
    , buildImage
    , buildAllImages
    , startCleanup
    , setupContainer
    , killContainer
    , killContainers
    , evalCode
    ) where

import Control.Monad.Reader

import qualified Data.Map.Strict as M
import Data.Snowflake

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.QSem.Lifted
import Control.Exception.Lifted
import System.Process.Typed

import Myriad.Core
import Myriad.Util

buildImage :: LanguageConfig -> MyriadIO ()
buildImage lang@LanguageConfig { name, concurrent } = do
    logInfo ["Building image ", cvs $ imageName lang]
    exec_ ["docker build -t ", imageName lang, " ./languages/", cvs name]
    setupQSems
    logInfo ["Built image ", cvs $ imageName lang]
    MyriadConfig { prepareContainers } <- asks config
    when_ prepareContainers $ setupContainer lang
    where
        setupQSems :: MyriadIO ()
        setupQSems = do
            Env { containerSems, evalSems } <- ask
            csem <- newQSem 1 -- We only want one container to be set up at a time
            esem <- newQSem $ fromIntegral concurrent
            mapMVar containerSems $ M.insert name csem
            mapMVar evalSems $ M.insert name esem

buildAllImages :: MyriadIO ()
buildAllImages = do
    MyriadConfig { languages, buildConcurrently } <- asks config
    if buildConcurrently
        then forConcurrently_ languages buildImage
        else forM_ languages buildImage

startCleanup :: MyriadIO ()
startCleanup = do
    MyriadConfig { cleanupInterval } <- asks config
    when_ (cleanupInterval > 0) do
        let t = fromIntegral cleanupInterval * 60000000
        fork $ timer t
    where
        timer :: Int -> MyriadIO ()
        timer t = forever do
            threadDelay t
            n <- killContainers
            logInfo ["Cleaned up ", cvs $ show n, " containers"]
            timer t

setupContainer :: LanguageConfig -> MyriadIO ContainerName
setupContainer lang@LanguageConfig { name, memory, cpus } = do
    cnts <- asks containers >>= readMVar
    case cnts M.!? name of
        Nothing  -> setup
        Just cnt -> pure cnt
    where
        setup :: MyriadIO ContainerName
        setup = do
            ref <- asks containers
            cnt <- newContainerName lang
            exec_
                [ "docker run --rm --name="
                , cvs cnt
                -- User 1000 will be for setting up the environment
                , " -u1000:1000 -w/tmp/ -dt --net=none --cpus="
                , cvs cpus
                , " -m="
                , cvs memory
                , " --memory-swap="
                , cvs memory
                , " "
                , imageName lang
                , " /bin/sh"
                ]
            -- The `eval` directory is where all the eval work is done
            -- 711 so that users can't traverse into other people's code
            exec_ ["docker exec ", cnt, " mkdir eval"]
            exec_ ["docker exec ", cnt, " chmod 711 eval"]
            mapMVar ref $ M.insert name cnt
            logInfo ["Started container ", cvs cnt]
            pure cnt

killContainer :: Language -> MyriadIO Bool
killContainer lang = do
    containers <- asks containers >>= readMVar
    case containers M.!? lang of
        Nothing  -> pure False
        Just cnt -> do
            res <- kill cnt
            case res of
                Nothing  -> pure True
                Just err -> do
                    logError ["An exception occured when killing ", cvs cnt, ":\n", cvs $ show err]
                    pure False
    where
        kill :: ContainerName -> MyriadIO (Maybe SomeException)
        kill cnt = do
            ref <- asks containers
            mapMVar ref $ M.delete lang
            res <- try $ exec_ ["docker kill ", cnt]
            case res of
                Left err -> pure $ Just err
                Right _  -> do
                    logInfo ["Killed container ", cvs cnt]
                    pure Nothing

killContainers :: MyriadIO [ContainerName]
killContainers = do
    containers <- asks containers >>= readMVar
    xs <- forConcurrently (M.toList containers) \(k, v) -> (v,) <$> killContainer k
    pure . map fst $ filter snd xs

evalCode :: LanguageConfig -> Int -> String -> MyriadIO EvalResult
evalCode lang@LanguageConfig { name, timeout, retries } numRetries code = withContainer \cnt -> do
    doneRef <- newMVar False -- For keeping track of if the evaluation is done, i.e. succeeded or timed out.
    void . fork $ timer doneRef -- `race` could not have been used here since some evals can't be cancelled.
    snowflakeGen <- asks snowflakeGen
    snowflake <- liftIO $ nextSnowflake snowflakeGen
    res <- try $ eval cnt snowflake
    case res of
        Left (SomeException err) -> do
            void $ killContainer name
            done <- readMVar doneRef
            if done
                -- If we find the eval is done from an exception, then it was timed out.
                then do
                    logError ["Code timed out in container ", cvs cnt, ", evaluation ", cvs $ show snowflake]
                    pure EvalTimedOut
                -- Otherwise, the container was killed from another eval, so we should retry.
                else do
                    writeMVar doneRef True
                    if numRetries < fromIntegral retries
                        then do
                            logError ["An exception occured in ", cvs cnt, ", evaluation ", cvs $ show snowflake, ", retrying:\n", cvs $ show err]
                            evalCode lang (numRetries + 1) code
                        else do
                            logError ["An exception occured in ", cvs cnt, ", evaluation ", cvs $ show snowflake, ":\n", cvs $ show err]
                            pure EvalErrored
        Right x -> do
            writeMVar doneRef True
            pure x
    where
        withContainer :: (ContainerName -> MyriadIO a) -> MyriadIO a
        withContainer f = do
            Env { containerSems, evalSems } <- ask
            csem <- (M.! name) <$> readMVar containerSems
            esem <- (M.! name) <$> readMVar evalSems
            bracket_ (waitQSem esem) (signalQSem esem) do
                cnt <- bracket_ (waitQSem csem) (signalQSem csem) $ setupContainer lang
                f cnt

        timer :: MVar Bool -> MyriadIO ()
        timer doneRef = do
            threadDelay $ fromIntegral timeout * 1000000
            done <- readMVar doneRef
            unless_ done do
                writeMVar doneRef True
                killContainer name

        eval :: ContainerName -> Snowflake -> MyriadIO EvalResult
        eval cnt snowflake = do
            logInfo ["Running code in container ", cvs cnt, ", evaluation ", cvs $ show snowflake, ":\n", cvs code]
            exec_ ["docker exec ", cvs cnt, " mkdir eval/", show snowflake]
            exec_ ["docker exec ", cvs cnt, " chmod 777 eval/", show snowflake]
            -- User 1001 will be used for the actual execution so that they can't access `eval` itself
            let args = ["exec", "-u1001:1001", "-w/tmp/eval/" <> show snowflake, cnt, "/bin/sh", "/var/run/run.sh", code]
            output <- readProcessInterleaved_ $ proc "docker" args
            exec_ ["docker exec ", cnt, " rm -rf eval/", show snowflake]
            logInfo ["Ran code in container ", cvs cnt, ", evaluation ", cvs $ show snowflake]
            pure $ EvalOk output
