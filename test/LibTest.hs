module LibTest where

import           Lib
import           Control.Concurrent.Async
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as M
import           Test.QuickCheck.Modifiers

libTestSuite :: TestTree
libTestSuite = testGroup "boxoffice tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "boxoffice property tests" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "TVar holds the right value when incremented sequentially." $
    QC.forAll (QC.choose (1,1000)) $ \count ->
      propSequentialAddCount count
  , QC.testProperty "TVar is cleared after an arbitrary number of sequential increments." $
    QC.forAll (QC.choose (1,1000)) (\count ->
      propSequentialResetCount count)
  , QC.testProperty "TVar holds the right value when incremented in parallel." $
    QC.forAll (QC.choose (1,1000)) $ \count ->
      propParallelAddCount count
  , QC.testProperty "TVar is cleared after an arbitrary number of parallel increments." $
    QC.forAll (QC.choose (1,1000)) (\count ->
      propParallelResetCount count)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Sequential increments function correctly." $
      testAddThreeCount
  , testCase "Clear works correctly after sequential increments" $
      testAddThreeCountAndClear
  ]

-- testAddThreeCount :: IO ()
testAddThreeCount = do
  myTVar <- initLib
  addCount   myTVar
  addCount   myTVar
  addCount   myTVar
  ioCount <- getCount  myTVar
  ioCount @?= 3

testAddThreeCountAndClear = do
  myTVar <- initLib
  addCount   myTVar
  addCount   myTVar
  addCount   myTVar
  ioCount <- getCount  myTVar
  ioCount @?= 3
  resetCount myTVar
  ioClearedCount <- getCount  myTVar
  ioClearedCount @?= 0

propSequentialAddCount :: Int -> QC.Property
propSequentialAddCount count = M.monadicIO $ do
  myTVar <- M.run  initLib
  mapM_ (M.run . addCount) (take count $ repeat myTVar )
  ioCount <- M.run $ getCount myTVar
  M.assert $ ioCount == count

propSequentialResetCount :: Int -> QC.Property
propSequentialResetCount count = M.monadicIO $ do
  myTVar <- M.run  initLib
  mapM_ (M.run . addCount) (take count $ repeat myTVar )
  M.run $ resetCount myTVar
  ioCount <- M.run $ getCount myTVar
  M.assert $ ioCount == 0

propParallelAddCount :: Int -> QC.Property
propParallelAddCount count = M.monadicIO $ do
  myTVar <- M.run  initLib
  M.run $ mapConcurrently_ (addCount) (take count $ repeat myTVar )
  ioCount <- M.run $ getCount myTVar
  M.assert $ ioCount == count

propParallelResetCount :: Int -> QC.Property
propParallelResetCount count = M.monadicIO $ do
  myTVar <- M.run  initLib
  M.run $ mapConcurrently_ (addCount) (take count $ repeat myTVar )
  M.run $ resetCount myTVar
  ioCount <- M.run $ getCount myTVar
  M.assert $ ioCount == 0

