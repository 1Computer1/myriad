module Myriad.Docker
    ( EvalResult(..)
    , buildImage
    , buildAllImages
    , startCleanup
    , setupContainer
    , killContainer
    , killAllContainers
    , killContainerMaybe
    , killAllContainersMaybe
    , evalCode
    ) where

import Control.Monad.Logger
import Control.Monad.Reader

import qualified Data.Map.Strict as M

import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.QSem.Lifted
import Control.Exception.Lifted
import Data.IORef.Lifted
import Data.Snowflake
import System.Process.Typed

import Myriad.Core
import Myriad.Util

exec :: MonadWithIO m => String -> MyriadT m ()
exec = runProcess_ . shell

buildImage :: MonadWithIO m => LanguageConfig -> MyriadT m ()
buildImage lang@LanguageConfig { name, concurrent } = do
    logInfoN $ mconcat ["Building image ", cvs $ imageName lang]
    let cmd = mconcat ["docker build -t ", imageName lang, " ./languages/", cvs name]
    runProcess_ . setStdout nullStream $ shell cmd
    logInfoN $ mconcat ["Built image ", cvs $ imageName lang]
    Env { config = MyriadConfig { prepareContainers }, containerSems, evalSems } <- ask
    csem <- newQSem 1 -- We only want one container to be set up at a time
    esem <- newQSem $ fromIntegral concurrent
    modifyIORef' containerSems $ M.insert name csem
    modifyIORef' evalSems $ M.insert name esem
    when_ prepareContainers $ setupContainer lang

buildAllImages :: MonadWithIO m => MyriadT m ()
buildAllImages = do
    MyriadConfig { languages, buildConcurrently } <- asks config
    if buildConcurrently
        then forConcurrently_ languages buildImage
        else forM_ languages buildImage

startCleanup :: MonadWithIO m => MyriadT m ()
startCleanup = do
    MyriadConfig { cleanupInterval } <- asks config
    when_ (cleanupInterval > 0) do
        let t = fromIntegral cleanupInterval * 60000000
        fork $ timer t
    where
        timer :: MonadWithIO m => Int -> MyriadT m ()
        timer t = forever do
            threadDelay t
            n <- killAllContainersMaybe
            logInfoN $ mconcat ["Cleaned up ", cvs $ show n, " containers"]
            timer t

setupContainer :: MonadWithIO m => LanguageConfig -> MyriadT m ContainerName
setupContainer lang@LanguageConfig { name, memory, cpus } = do
    ref <- asks containers
    cnts <- readIORef ref
    case cnts M.!? name of
        Just x  -> pure x
        Nothing -> do
            cnt <- newContainerName lang
            let cmd = mconcat
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
            runProcess_ . setStdout nullStream $ shell cmd
            -- The `eval` directory is where all the eval work is done
            -- 711 so that users can't traverse into other people's code
            exec $ mconcat ["docker exec ", cnt, " mkdir eval"]
            exec $ mconcat ["docker exec ", cnt, " chmod 711 eval"]
            modifyIORef' ref $ M.insert name cnt
            logInfoN $ mconcat ["Started container ", cvs cnt]
            pure cnt

killContainer :: MonadWithIO m => Language -> MyriadT m ()
killContainer lang = do
    ref <- asks containers
    containers <- readIORef ref
    case containers M.!? lang of
        Nothing  -> pure ()
        Just cnt -> do
            modifyIORef' ref $ M.delete lang
            let cmd = mconcat ["docker kill ", cnt]
            runProcess_ . setStderr nullStream . setStdout nullStream $ shell cmd
            logInfoN $ mconcat ["Killed container ", cvs cnt]

killContainerMaybe :: MonadWithIO m => Language -> MyriadT m Bool
killContainerMaybe lang = do
    containers <- asks containers >>= readIORef
    case containers M.!? lang of
        Nothing  -> pure False
        Just cnt -> do
            res :: Either SomeException () <- try $ killContainer lang
            case res of
                Left err -> do
                    logErrorN $ mconcat ["An exception occured when killing ", cvs cnt, ":\n", cvs $ show err]
                    pure False
                Right _  -> pure True

killAllContainers :: MonadWithIO m => MyriadT m ()
killAllContainers = do
    containers <- asks containers >>= readIORef
    forConcurrently_ (M.keys containers) $ killContainer

killAllContainersMaybe :: MonadWithIO m => MyriadT m Int
killAllContainersMaybe = do
    containers <- asks containers >>= readIORef
    xs <- forConcurrently (M.keys containers) $ killContainerMaybe
    pure . length $ filter id xs

evalCode :: MonadWithIO m => LanguageConfig -> Int -> String -> MyriadT m EvalResult
evalCode lang@LanguageConfig { name, timeout, retries } numRetries code = do
    Env { containerSems, evalSems } <- ask
    csem <- (M.! name) <$> readIORef containerSems
    esem <- (M.! name) <$> readIORef evalSems
    bracket_ (waitQSem esem) (signalQSem esem) $ do
        cnt <- bracket_ (waitQSem csem) (signalQSem csem) $ setupContainer lang
        doneRef <- newIORef False -- For keeping track of if the evaluation is done, i.e. succeeded or timed out.
        void . fork $ timer doneRef -- `race` could not have been used here since some evals can't be cancelled.
        res <- try $ eval cnt
        case res of
            Left (SomeException err) -> do
                void $ killContainerMaybe name
                done <- readIORef doneRef
                if done
                    -- If we find the eval is done from an exception, then it was timed out.
                    then do
                        logInfoN $ mconcat ["Code timed out in container ", cvs cnt]
                        pure EvalTimedOut
                    -- Otherwise, the container was killed from another eval, so we should retry.
                    else do
                        writeIORef doneRef True
                        if numRetries < fromIntegral retries
                            then evalCode lang (numRetries + 1) code
                            else do
                                logErrorN $ mconcat ["An exception occured when evaluating in ", cvs cnt, ":\n", cvs $ show err]
                                pure EvalErrored
            Right x -> do
                writeIORef doneRef True
                pure x
    where
        timer :: MonadWithIO m => IORef Bool -> MyriadT m ()
        timer doneRef = do
            threadDelay $ fromIntegral timeout * 1000000
            done <- readIORef doneRef
            unless_ done do
                writeIORef doneRef True
                killContainerMaybe name

        eval :: MonadWithIO m => ContainerName -> MyriadT m EvalResult
        eval cnt = do
            logInfoN $ mconcat ["Running code in container ", cvs cnt, ":\n", cvs code]
            snowflakeGen <- asks snowflakeGen
            snowflake <- liftIO $ nextSnowflake snowflakeGen
            exec $ mconcat ["docker exec ", cvs cnt, " mkdir eval/", show snowflake]
            exec $ mconcat ["docker exec ", cvs cnt, " chmod 777 eval/", show snowflake]
            -- User 1001 will be used for the actual execution so that they can't access `eval` itself
            let args = ["exec", "-u1001:1001", "-w/tmp/eval/" <> show snowflake, cnt, "/bin/sh", "/var/run/run.sh", code]
            output <- readProcessInterleaved_ $ proc "docker" args
            exec $ mconcat ["docker exec ", cnt, " rm -rf eval/", show snowflake]
            logInfoN $ mconcat ["Ran code in container ", cvs cnt]
            pure $ EvalOk output
