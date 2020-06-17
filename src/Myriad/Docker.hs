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
import Data.Maybe
import Data.Snowflake
import Data.String.Conversions

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.QSem.Lifted
import Control.Exception.Lifted
import System.FilePath ((</>))
import System.Process.Typed

import Myriad.Core

type Myriad = MyriadT IO

buildImage :: LanguageConfig -> Myriad ()
buildImage lang@LanguageConfig { name, concurrent } = do
    Env { config = MyriadConfig { prepareContainers }, languagesDir } <- ask
    logInfo ["Building image ", cs $ imageName lang]
    exec_ ["docker build -t ", imageName lang, " ", cs languagesDir </> cs name]
    setupQSems
    logInfo ["Built image ", cs $ imageName lang]
    when_ prepareContainers $ setupContainer lang
    where
        setupQSems ::  Myriad ()
        setupQSems = do
            Env { config = MyriadConfig { defaultLanguage }, containerSems, evalSems } <- ask
            csem <- newQSem 1 -- We only want one container to be set up at a time
            esem <- newQSem . fromIntegral $ fromMaybe (defConcurrent defaultLanguage) concurrent
            mapMVar containerSems $ M.insert name csem
            mapMVar evalSems $ M.insert name esem

buildAllImages :: Myriad ()
buildAllImages = do
    MyriadConfig { languages, buildConcurrently } <- asks config
    if buildConcurrently
        then forConcurrently_ languages buildImage
        else forM_ languages buildImage

startCleanup :: Myriad ()
startCleanup = do
    MyriadConfig { cleanupInterval } <- asks config
    when_ (cleanupInterval > 0) do
        let t = fromIntegral cleanupInterval * 60000000
        fork $ timer t
    where
        timer :: Int -> Myriad ()
        timer t = forever do
            threadDelay t
            n <- killContainers
            logInfo ["Cleaned up ", cs $ show n, " containers"]
            timer t

setupContainer :: LanguageConfig -> Myriad ContainerName
setupContainer lang@LanguageConfig { name, memory, cpus } = do
    cnts <- asks containers >>= readMVar
    case cnts M.!? name of
        Nothing  -> setup
        Just cnt -> pure cnt
    where
        setup :: Myriad ContainerName
        setup = do
            Env { config = MyriadConfig { defaultLanguage }, containers = ref } <- ask
            cnt <- newContainerName lang
            exec_
                [ "docker run --rm --name="
                , cs cnt
                -- User 1000 will be for setting up the environment
                , " -u1000:1000 -w/tmp/ -dt --net=none --cpus="
                , show $ fromMaybe (defCpus defaultLanguage) cpus
                , " -m="
                , cs $ fromMaybe (defMemory defaultLanguage) memory
                , " --memory-swap="
                , cs $ fromMaybe (defMemory defaultLanguage) memory
                , " "
                , imageName lang
                , " /bin/sh"
                ]
            -- The `eval` directory is where all the eval work is done
            -- 711 so that users can't traverse into other people's code
            exec_ ["docker exec ", cnt, " mkdir eval"]
            exec_ ["docker exec ", cnt, " chmod 711 eval"]
            mapMVar ref $ M.insert name cnt
            logInfo ["Started container ", cs cnt]
            pure cnt

killContainer :: Language -> Myriad Bool
killContainer lang = do
    containers <- asks containers >>= readMVar
    case containers M.!? lang of
        Nothing  -> pure False
        Just cnt -> do
            res <- kill cnt
            case res of
                Nothing  -> pure True
                Just err -> do
                    logError ["An exception occured when killing ", cs cnt, ":\n", cs $ show err]
                    pure False
    where
        kill :: ContainerName -> Myriad (Maybe SomeException)
        kill cnt = do
            ref <- asks containers
            mapMVar ref $ M.delete lang
            res <- try $ exec_ ["docker kill ", cnt]
            case res of
                Left err -> pure $ Just err
                Right _  -> do
                    logInfo ["Killed container ", cs cnt]
                    pure Nothing

killContainers :: Myriad [ContainerName]
killContainers = do
    containers <- asks containers >>= readMVar
    xs <- forConcurrently (M.toList containers) \(k, v) -> (v,) <$> killContainer k
    pure . map fst $ filter snd xs

evalCode :: LanguageConfig -> Int -> String -> Myriad EvalResult
evalCode lang@LanguageConfig { name, timeout, retries } numRetries code = withContainer \cnt -> do
    doneRef <- newMVar False -- For keeping track of if the evaluation is done, i.e. succeeded or timed out.
    void . fork $ timer doneRef -- `race` could not have been used here since some evals can't be cancelled.
    Env { config = MyriadConfig { defaultLanguage }, snowflakeGen } <- ask
    snowflake <- liftIO $ nextSnowflake snowflakeGen
    res <- try $ eval cnt snowflake
    case res of
        Left (SomeException err) -> do
            void $ killContainer name
            done <- readMVar doneRef
            if done
                -- If we find the eval is done from an exception, then it was timed out.
                then do
                    logError ["Code timed out in container ", cs cnt, ", evaluation ", cs $ show snowflake]
                    pure EvalTimedOut
                -- Otherwise, the container was killed from another eval, so we should retry.
                else do
                    writeMVar doneRef True
                    if numRetries < (fromIntegral $ fromMaybe (defRetries defaultLanguage) retries)
                        then do
                            logError ["An exception occured in ", cs cnt, ", evaluation ", cs $ show snowflake, ", retrying:\n", cs $ show err]
                            evalCode lang (numRetries + 1) code
                        else do
                            logError ["An exception occured in ", cs cnt, ", evaluation ", cs $ show snowflake, ":\n", cs $ show err]
                            pure EvalErrored
        Right x -> do
            writeMVar doneRef True
            pure x
    where
        withContainer :: (ContainerName -> Myriad a) -> Myriad a
        withContainer f = do
            Env { containerSems, evalSems } <- ask
            csem <- (M.! name) <$> readMVar containerSems
            esem <- (M.! name) <$> readMVar evalSems
            bracket_ (waitQSem esem) (signalQSem esem) do
                cnt <- bracket_ (waitQSem csem) (signalQSem csem) $ setupContainer lang
                f cnt

        timer :: MVar Bool -> Myriad ()
        timer doneRef = do
            Env { config = MyriadConfig { defaultLanguage } } <- ask
            threadDelay $ (fromIntegral $ fromMaybe (defTimeout defaultLanguage) timeout) * 1000000
            done <- readMVar doneRef
            unless_ done do
                writeMVar doneRef True
                killContainer name

        eval :: ContainerName -> Snowflake -> Myriad EvalResult
        eval cnt snowflake = do
            logInfo ["Running code in container ", cs cnt, ", evaluation ", cs $ show snowflake, ":\n", cs code]
            exec_ ["docker exec ", cs cnt, " mkdir eval/", show snowflake]
            exec_ ["docker exec ", cs cnt, " chmod 777 eval/", show snowflake]
            -- User 1001 will be used for the actual execution so that they can't access `eval` itself
            let cmd = concat ["docker exec -i -u1001:1001 -w/tmp/eval/", show snowflake, " ", cnt, " /bin/sh /var/run/run.sh | head -c 4K"]
                pr = setStdin (byteStringInput $ cs code) $ shell cmd
            output <- readProcessInterleaved_ pr
            exec_ ["docker exec ", cnt, " rm -rf eval/", show snowflake]
            logInfo ["Ran code in container ", cs cnt, ", evaluation ", cs $ show snowflake]
            pure $ EvalOk output

newContainerName :: LanguageConfig -> Myriad ContainerName
newContainerName LanguageConfig { name } = do
    snowflakeGen <- asks snowflakeGen
    snowflake <- liftIO $ nextSnowflake snowflakeGen
    pure $ "comp_iler-" <> cs name <> "-" <> show snowflake

imageName :: LanguageConfig -> ImageName
imageName LanguageConfig { name } = "1computer1/comp_iler:" <> cs name

when_ :: Applicative f => Bool -> f a -> f ()
when_ p = when p . void 

unless_ :: Applicative f => Bool -> f a -> f ()
unless_ p = unless p . void 
