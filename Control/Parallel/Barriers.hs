module Control.Parallel.Barriers (
  -- * Parallel Helper Functions
  parBarrier, pseqBarrier, forceParallel,
  -- * Flush the Spark Pool
  flushSparkPool
) where

import Control.Parallel
import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc.Sync (runSparks, yield)

{- |
@ x `parBarrier` y @ will spark the evaluation of @x@, flush the spark pool,
/and then/ return y.
-}
infixr 0 `parBarrier`
parBarrier :: a -> b -> b
x `parBarrier` y = x `par` waitForSparks y

-- | Wait for sparks to be run, then return the supplied value.
waitForSparks :: a -> a
waitForSparks y =
  unsafePerformIO $ do
    flushSparkPool
    return y

flushSparkPool :: IO ()
flushSparkPool =
  do
    -- 'yield' allows other worker threads to steal work from the spark pool.
    yield
    runSparks

{- |
Conceptually similar to 'pseq'. The first argument is evaluated to weak head
normal form, waits for sparks to be run and then return the second argument.
This is usefull in cases like:

> x0 `'par'` x1 `'par'` ... `'par'` xn `'pseq'` sequentialCombinerOfX1toXn

might generates many "fizzled" sparks, if sequentialCombinerOfX1toXn uses the
xs faster than the sparks are run. Changing this to

> x0 `'par'` x1 `'par'` ... `'par'` xn `pseqBarrier` sequentialCombinerOfX1toXn

will avoid the "fizzling".
-}
infixr 0 `pseqBarrier`
pseqBarrier :: a -> b -> b
x `pseqBarrier` y = x `pseq` waitForSparks y

-- | @forceParallel x@ is the same as @x `'pseqBarrier'` x@. It is useful, if
-- the evaluation of @x@ sparks parallel evaluations and one wants to make
-- sure, that all sparks are run before @x@ is used. In other cases it is an
-- inefficient variant of 'id'.
forceParallel :: a -> a
forceParallel x = x `pseqBarrier` x
