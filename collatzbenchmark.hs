
import Criterion.Main
import GHC.Conc 
import System.IO.Unsafe

import Control.Parallel.Barriers

main :: IO ()
main = defaultMain benchCollatz

-- Run then benchmarks first on a single core then on multiple cores (if avaiable). Hack but works.
compareSingleMulticore :: [Benchmark] -> [Benchmark]
compareSingleMulticore benchs = [
  setSingleCore `pseq` bgroup "SingleCore" benchs,
  setMultiCore `pseq` bgroup "MultiCore" benchs
  ]
  where
    setSingleCore =
      unsafePerformIO $ setNumCapabilities 1
    setMultiCore =
      unsafePerformIO $ setNumCapabilities =<< getNumProcessors


benchCollatz :: [Benchmark]
benchCollatz = compareSingleMulticore [
  bgroup "Collatz" [
    -- no parallelity for comparison
    bench "normal" $ whnf (maximum . collatzLength1to) n,
    -- parallel folds
    bench "parFold" $ whnf (parMaximum . collatzLength1to) n,
    bench "parFoldBarrier" $
      whnf (parMaximumBarrier . collatzLength1to) n,
    bench "parFoldSingleBarrier" $
      whnf (parMaximumBarrier' . collatzLength1to) n,
    -- parallel evaluation of the list elements
    bench "parList" $
      whnf (maximum . parList . collatzLength1to) n,
    bench "parListForce" $
      whnf (maximum . forceParList . collatzLength1to) n
    ]
  ]
  where
    n=10^3
    {-# INLINE collatzLength1to #-}
    collatzLength1to = map collatzLength . enumFromTo 1

-- see <http://projecteuler.net/problem=14> for what the collatz length is.
collatzLength :: Int -> Int
collatzLength = accum 0
  where
    accum result 1 = result
    accum acc n = accum (acc+1) (nextCollatz n)

nextCollatz :: Int -> Int
nextCollatz n =
  if even n
    then n `div` 2
    else 3*n + 1

parMaximum :: Ord a => [a] -> a
parMaximum = foldr1 (parMax)
  where
    parMax x y = x `par` y `pseq` max x y

parMaximumBarrier :: Ord a => [a] -> a
parMaximumBarrier = foldr1 (parMax)
  where
    parMax x y = x `par` y `pseqBarrier` max x y

parMaximumBarrier' :: Ord a => [a] -> a
parMaximumBarrier' [x] = x
parMaximumBarrier' [x,y] = x `par` y `pseqBarrier` max x y
parMaximumBarrier' (x:xs) = x `par` maxXs `pseq` max x maxXs
  where
    maxXs = parMaximumBarrier' xs
parMaximumBarrier' [] = error "parMaximumBarrier': empty list"

parList :: [a] -> [a]
parList xs = foldr par xs xs
