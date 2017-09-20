module Lib
    ( testFunc
    ) where

import Control.Monad.Trans.State
import Data.IORef
import Data.Time.Clock

initLib :: IO (IORef ([UTCTime]))
initLib = newIORef []

resetCount :: IORef ([UTCTime]) -> IO ()
resetCount ioRef = do
  modifyIORef' ioRef returnEmptyList
  return ()

returnEmptyList :: [UTCTime] -> [UTCTime]
returnEmptyList _ = []

addCount :: IORef ([UTCTime]) -> IO ()
addCount ioRef = do
  currentTime <- getCurrentTime
  modifyIORef' ioRef (addToList currentTime)
  return ()

addToList :: a -> [a] -> [a]
addToList time list = time : list

showCount :: IORef ([UTCTime]) -> IO ()
showCount ioRef = do
  list <- (readIORef ioRef :: IO [UTCTime])
  putStrLn $ show list
  return ()

testFunc :: IO ()
testFunc = do
  myTVar <- initLib
  addCount   myTVar
  addCount   myTVar
  showCount  myTVar
  resetCount myTVar
  showCount  myTVar
  return ()
