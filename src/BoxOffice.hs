module BoxOffice
    ( initLib
    , resetCount
    , addCount
    , showList
    , getCount
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

showList :: TVar ([UTCTime]) -> IO ()
showList tVar = do
  list <- (readTVarIO tVar :: IO [UTCTime])
  putStrLn $ show $ length list
  return ()

getCount :: TVar ([UTCTime]) -> IO Int
getCount tVar = do
  list <- (readTVarIO tVar :: IO [UTCTime])
  return $ length list

getCountAfter :: UTCTime -> TVar ([UTCTime]) -> IO Int
getCountAfter time tVar = do
  list <- (readTVarIO tVar :: IO [UTCTime])
  return $ length $ filter (predicateAfter time) list

getCountBefore :: UTCTime -> TVar ([UTCTime]) -> IO Int
getCountBefore time tVar = do
  list <- (readTVarIO tVar :: IO [UTCTime])
  return $ length $ filter (predicateBefore time) list

predicateAfter :: UTCTime -> UTCTime -> Bool
predicateAfter argTime tVarTime = case compare argTime tVarTime of
  EQ -> True
  GT -> True
  LT -> False

predicateAfter :: UTCTime -> UTCTime -> Bool
predicateAfter argTime tVarTime = case compare argTime tVarTime of
  EQ -> True
  GT -> False
  LT -> True
