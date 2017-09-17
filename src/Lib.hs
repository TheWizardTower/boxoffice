module Lib
    ( someFunc
    ) where

import Control.Monad.Trans.State
import Data.IORef
import Data.Time.Clock

staticVar :: IO (IORef ([UTCTime]))
staticVar = newIORef []

resetCount :: IO ()
resetCount = do
  ((flip modifyIORef') returnEmptyList) <$> staticVar
  return ()

returnEmptyList :: [UTCTime] -> [UTCTime]
returnEmptyList _ = []

addCount :: IO ()
addCount = do
  currentTime <- getCurrentTime
  ((flip modifyIORef') (addToList currentTime)) <$> staticVar
  return ()

addToList :: a -> [a] -> [a]
addToList time list = time : list

showList = do
  list <- fmap readIORef staticVar
  fmap (putStrLn . show) list

someFunc :: IO ()
someFunc = putStrLn "someFunc"
