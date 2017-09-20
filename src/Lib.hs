module Lib
    ( testFunc
    ) where

import Control.Monad.Trans.State
import Control.Concurrent.STM.TVar
import GHC.Conc.Sync
import Data.Time.Clock

initLib :: IO (TVar ([UTCTime]))
initLib = newTVarIO []

resetCount :: TVar ([UTCTime]) -> IO ()
resetCount tVar = do
  atomically $ writeTVar tVar returnEmptyList
  return ()

returnEmptyList :: [UTCTime]
returnEmptyList = []

addCount :: TVar ([UTCTime]) -> IO ()
addCount tVar = do
  currentTime <- getCurrentTime
  atomically $ modifyTVar' tVar (addToList currentTime)
  return ()

addToList :: a -> [a] -> [a]
addToList time list = time : list

showCount :: TVar ([UTCTime]) -> IO ()
showCount tVar = do
  list <- (readTVarIO tVar :: IO [UTCTime])
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
