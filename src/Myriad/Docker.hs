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

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.QSem.Lifted
import Control.Exception.Lifted
import Data.Snowflake
import System.Process.Typed

import Myriad.Core
import Myriad.Util

exec :: String -> MyriadIO ()
exec = runProcess_ . shell

buildImage :: LanguageConfig -> MyriadIO ()
buildImage lang@LanguageConfig { name, concurrent } = do
    logInfoN $ mconcat ["Building image ", cvs $ imageName lang]
    let cmd = mconcat ["docker build -t ", imageName lang, " ./languages/", cvs name]
    runProcess_ . setStdout nullStream $ shell cmd
    logInfoN $ mconcat ["Built image ", cvs $ imageName lang]
    Env { config = MyriadConfig { prepareContainers }, containerSems, evalSems } <- ask
    csem <- newQSem 1 -- We only want one container to be set up at a time
    esem <- newQSem $ fromIntegral concurrent
    modifyMVar_ containerSems $ pure . M.insert name csem
    modifyMVar_ evalSems $ pure . M.insert name esem
    when_ prepareContainers $ setupContainer lang

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
            n <- killAllContainersMaybe
            logInfoN $ mconcat ["Cleaned up ", cvs $ show n, " containers"]
            timer t

setupContainer :: LanguageConfig -> MyriadIO ContainerName
setupContainer lang@LanguageConfig { name, memory, cpus } = do
    ref <- asks containers
    cnts <- readMVar ref
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
            modifyMVar_ ref $ pure . M.insert name cnt
            logInfoN $ mconcat ["Started container ", cvs cnt]
            pure cnt

killContainer :: Language -> MyriadIO ()
killContainer lang = do
    ref <- asks containers
    containers <- readMVar ref
    case containers M.!? lang of
        Nothing  -> pure ()
        Just cnt -> do
            modifyMVar_ ref $ pure . M.delete lang
            let cmd = mconcat ["docker kill ", cnt]
            runProcess_ . setStderr nullStream . setStdout nullStream $ shell cmd
            logInfoN $ mconcat ["Killed container ", cvs cnt]

killContainerMaybe :: Language -> MyriadIO Bool
killContainerMaybe lang = do
    containers <- asks containers >>= readMVar
    case containers M.!? lang of
        Nothing  -> pure False
        Just cnt -> do
            res <- try @_ @SomeException $ killContainer lang
            case res of
                Left err -> do
                    logErrorN $ mconcat ["An exception occured when killing ", cvs cnt, ":\n", cvs $ show err]
                    pure False
                Right _  -> pure True

killAllContainers :: MyriadIO ()
killAllContainers = do
    containers <- asks containers >>= readMVar
    forConcurrently_ (M.keys containers) $ killContainer

killAllContainersMaybe :: MyriadIO [ContainerName]
killAllContainersMaybe = do
    containers <- asks containers >>= readMVar
    xs <- forConcurrently (M.toList containers) \(k, v) -> (v,) <$> killContainerMaybe k
    pure . map fst $ filter snd xs

evalCode :: LanguageConfig -> Int -> String -> MyriadIO EvalResult
evalCode lang@LanguageConfig { name, timeout, retries } numRetries code = do
    Env { containerSems, evalSems } <- ask
    csem <- (M.! name) <$> readMVar containerSems
    esem <- (M.! name) <$> readMVar evalSems
    bracket_ (waitQSem esem) (signalQSem esem) $ do
        cnt <- bracket_ (waitQSem csem) (signalQSem csem) $ setupContainer lang
        doneRef <- newMVar False -- For keeping track of if the evaluation is done, i.e. succeeded or timed out.
        void . fork $ timer doneRef -- `race` could not have been used here since some evals can't be cancelled.
        res <- try $ eval cnt
        case res of
            Left (SomeException err) -> do
                void $ killContainerMaybe name
                done <- readMVar doneRef
                if done
                    -- If we find the eval is done from an exception, then it was timed out.
                    then do
                        logInfoN $ mconcat ["Code timed out in container ", cvs cnt]
                        pure EvalTimedOut
                    -- Otherwise, the container was killed from another eval, so we should retry.
                    else do
                        modifyMVar_ doneRef $ pure . const True
                        if numRetries < fromIntegral retries
                            then evalCode lang (numRetries + 1) code
                            else do
                                logErrorN $ mconcat ["An exception occured when evaluating in ", cvs cnt, ":\n", cvs $ show err]
                                pure EvalErrored
            Right x -> do
                modifyMVar_ doneRef $ pure . const True
                pure x
    where
        timer :: MVar Bool -> MyriadIO ()
        timer doneRef = do
            threadDelay $ fromIntegral timeout * 1000000
            done <- readMVar doneRef
            unless_ done do
                modifyMVar_ doneRef $ pure . const True
                killContainerMaybe name

        eval :: ContainerName -> MyriadIO EvalResult
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
