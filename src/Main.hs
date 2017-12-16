{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, MultiWayIf #-}
module Main where

import Data.List

import Control.Monad.Reader
import Control.Monad.IO.Class

import Control.Exception

import System.IO
import System.Process
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  runJDBScript args script (liftIO . putStrLn . concat . init)

script :: JDB ()
script = do
  cmdNormal "stop in base.ghc.io.handle.Text$$wa7.enter"
  cmdNormal "stop in text.data.text.Show$sat_s3LW7.thunkEnter"
  cmdNormal "catch java.lang.NullPointerException"
  cmdMain "run"
  -- cmdMain "trace go methods"
  -- cmdMain "watch all ghc_prim.ghc.Types$ZCD.x1"
  cmdMain "watch all ghc_prim.ghc.Types$ZCD.x2"
  cmdMain "watch all text.data.text.Show$sat_s3LW7.x2"
  -- cmdMain "watch all text.data.text.Show$sat_s3LW7.x1"
  -- cmdMain "watch all text.data.text.Show$sat_s3LW7.x2"
  -- forever $ cmdMain "next"
  loop
  return ()
  where loop = do
          output <- cmdMain "stepi"
          when (outputHas "will be null" output) $ cmdMain "wherei" >> return ()
          if | not (inMethod output) -> loopStepup >> loop
             | otherwise -> loop
        inMethod = outputHas "base.ghc.io.handle.Text$$wa7.enter"
        loopStepup = cmdMain "step up" >>= \output -> do
          when (outputHas "will be null" output) $ cmdMain "wherei" >> return ()
          if inMethod output then return ()
          else loopStepup
          -- output <- cmdMain "stepi"
          -- if not (any ("base.ghc.io.handle.Text$$wa7.enter" `isInfixOf`) output)
          -- then cmdMain "step up" >> loop
          -- else loop
          -- output <- cmdMain "next"
          -- if (any ("null, will be null" `isInfixOf`) output)
          -- then cmdMain "wherei"
          -- else loop
        cmdMain = cmd "main[1] "
        cmdNormal = cmd defaultEnding
outputHas :: String -> [String] -> Bool
outputHas search = any (search `isInfixOf`)

defaultEnding = "\n> "

runJDBScript :: [String] -> JDB () -> ([String] -> JDB ()) -> IO ()
runJDBScript args jdbAction f =
  withCreateProcess (proc "jdb" args) {
    std_in = CreatePipe, std_out = CreatePipe
  } $ \(Just hin) (Just hout) _ ph -> do
    runJDB jdbAction (hin, hout) f
    terminateProcess ph

newtype JDB a = JDB { unJDB :: ReaderT (Handle, Handle) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (Handle, Handle), MonadIO)

runJDB :: JDB () -> (Handle, Handle) -> ([String] -> JDB ()) -> IO ()
runJDB jdbAction env f =
  flip runReaderT env $ unJDB $ do
    init <- drainUntilEnd defaultEnding
    f init
    jdbAction

getInputHandle :: JDB Handle
getInputHandle = fmap fst ask

getOutputHandle :: JDB Handle
getOutputHandle = fmap snd ask

cmd :: String -> String -> JDB [String]
cmd ending input = do
  hin <- getInputHandle
  liftIO $ hPutStrLn hin input >> hFlush hin
  output <- drainUntilEnd ending
  liftIO $ putStr $ unlines $ output
  return output

drainUntilEnd :: String -> JDB [String]
drainUntilEnd ending = fmap (lines . concat . reverse) $ parts []
  where revEnding = reverse ending
        lenEnding = length revEnding
        parts prev = do
          (string, finished) <- drainOutput
          let rev = reverse string
          if (take lenEnding rev == revEnding) || finished
          then return ((if finished then string else reverse (drop lenEnding rev)) : prev)
          else parts (string : prev)

drainOutput :: JDB (String, Bool)
drainOutput = getOutputHandle >>= main []
  where main strings hout = do
          mHasNext <- waitForInput hout
          case mHasNext of
            Just hasNext -> do
              if hasNext
              then do
                string <- go hout
                main (string:strings) hout
              else return (concat $ reverse strings, False)
            Nothing -> return (concat $ reverse strings, True)

        go hout = do
          hasNext <- liftIO $ hReady hout
          if hasNext
          then do
            x  <- liftIO (hGetChar hout)
            xs <- go hout
            return (x:xs)
          else do
            return []

waitForInput :: Handle -> JDB (Maybe Bool)
waitForInput hout = liftIO $
  catch (fmap Just $ hWaitForInput hout 1) (\(_ :: SomeException) -> return Nothing)
