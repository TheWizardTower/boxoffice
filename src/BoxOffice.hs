module BoxOffice
    ( initLib
    , resetCount
    , addCount
    , showHitList
    , getCount
    ) where

import Control.Monad.Trans.State
import Control.Concurrent.STM.TVar
import GHC.Conc.Sync
import Data.Time.Clock

-- This is probably somewhat misnamed. The library isn't initialized, per se,
-- this is just a convenience method to get a TVar of the correct type. This
-- should be called by the main function of your application to get the TVar,
-- then thread it through to the various places where you'll be calling addCount
-- or getCount or the like.
initLib :: IO (TVar ([UTCTime]))
initLib = newTVarIO []

-- This clears the list the TVar holds. The semantic meaning of this is
-- resetting the number of hits this library has recorded.
resetCount :: TVar ([UTCTime]) -> IO ()
resetCount tVar = do
  atomically $ writeTVar tVar returnEmptyList
  return ()

-- Internal utility value. This is just an empty list that has the type
-- specified to the correct type.
returnEmptyList :: [UTCTime]
returnEmptyList = []

-- This is the function that will get the most use. This records a hit on your
-- website. It does this by appending the current time (the output of
-- getCurrentTime) to the list inside the TVar. It then returns unit -- ().
addCount :: TVar ([UTCTime]) -> IO ()
addCount tVar = do
  currentTime <- getCurrentTime
  atomically $ modifyTVar' tVar (addToList currentTime)
  return ()

-- Internal helper function used by addCount.
addToList :: a -> [a] -> [a]
addToList time list = time : list

-- This function returns the [UTCTime] inside the TVar. This can be useful if
-- the end user wants to do some analysis on what hits happened when. This is
-- great if you want to graph hits over time.
showHitList :: TVar ([UTCTime]) -> IO ()
showHitList tVar = do
  list <- (readTVarIO tVar :: IO [UTCTime])
  putStrLn $ show $ length list
  return ()

-- This function returns the length of the list the TVar is holding. In other
-- words, how many hits you've recorded since starting (or, at least, since
-- you've last cleared the list).
getCount :: TVar ([UTCTime]) -> IO Int
getCount tVar = do
  list <- (readTVarIO tVar :: IO [UTCTime])
  return $ length list

-- This returns the list inside the TVar, but after being passed through a
-- filter -- specifically, one asserting that the element is equal to or after
-- the argument supplied.
getCountAfter :: UTCTime -> TVar ([UTCTime]) -> IO Int
getCountAfter time tVar = do
  list <- (readTVarIO tVar :: IO [UTCTime])
  return $ length $ filter (predicateAfter time) list

-- As above, but nearly inverted -- this filters on equal to or before. The fact
-- that it wasn't a perfect inversion is why the functions aren't just
-- inversions of each other.
getCountBefore :: UTCTime -> TVar ([UTCTime]) -> IO Int
getCountBefore time tVar = do
  list <- (readTVarIO tVar :: IO [UTCTime])
  return $ length $ filter (predicateBefore time) list

-- Internal helper function for getCountAfter.
predicateAfter :: UTCTime -> UTCTime -> Bool
predicateAfter argTime tVarTime = case compare argTime tVarTime of
  EQ -> True
  GT -> True
  LT -> False

-- Internal helper function for getCountBefore.
predicateBefore :: UTCTime -> UTCTime -> Bool
predicateBefore argTime tVarTime = case compare argTime tVarTime of
  EQ -> True
  GT -> False
  LT -> True
