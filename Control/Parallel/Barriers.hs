{- |
One drawback of Parallel Haskell is, that the sequential part of the programm
/races/ with the evaluation of sparks (sparked by 'par'). This often results in
duplicated work (\"fizzled\" sparks), lost parallelity and unnecessary
overhead, and limits the performance gain of running computations in parallel.

To avoid these races, this module allows to add \"barriers\". A barrier signals
the run time system, that the sparks should be run, before the sequential part
of the computation continues¹. Adding a barrier adds significant overhead and
might reduce parallelity. In general it is preferable, to add just a /single/
barrier.

Reasonable ways to achieve parallelity are:

 1. Evaluate the elements in data structures in parallel by using
 'forceParList' or 'forceParFoldable'.

 2. In a sequence of calls to 'par' and 'pseq', change the /last/ call of
 'pseq' to 'pseqBarrier'.

 3. Wrap a sequence of 'par' calls with 'forceParallel'.

¹ If bottom is in the spark pool, the sequential part might never be
continued.
-}
module Control.Parallel.Barriers (
  -- * Parallel Helper Functions
  parBarrier, pseqBarrier, forceParallel,
  -- * Utilities for Foldable Structures
  forceParList, forceParFoldable,
  -- * Flush the Spark Pool
  flushSparkPool
) where

import Control.Parallel
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc.Sync (runSparks, yield)

{- |
@ x \`parBarrier\` y @ will spark the evaluation of @x@, flush the spark pool,
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

> x0 `par` x1 `par` ... `par` xn `pseq` sequentialCombinerOfX1toXn

might generates many \"fizzled\" sparks, if sequentialCombinerOfX1toXn uses the
xs faster than the sparks are run. Changing this to

> x0 `par` x1 `par` ... `par` xn `pseqBarrier` sequentialCombinerOfX1toXn

will avoid the \"fizzling\".
-}
infixr 0 `pseqBarrier`
pseqBarrier :: a -> b -> b
x `pseqBarrier` y = x `pseq` waitForSparks y

-- | @forceParallel x@ is the same as @x \`pseqBarrier\` x@. It is useful, if
-- the evaluation of @x@ sparks parallel evaluations and one wants to make
-- sure, that all sparks are run before @x@ is used. In other cases it is an
-- inefficient variant of 'id'.
forceParallel :: a -> a
forceParallel x = x `pseqBarrier` x

{- |
Evaluate the elements of a list in parallel. See 'forceParFoldable' for more details.
-}
forceParList :: [a] -> [a]
forceParList xs = forceParallel $ foldr par xs xs

{- |
Evaluate the elements of a 'Foldable' structure in parallel. It is typically
used by changing @xs = ...@ to @ xs = 'forceParFoldable' $ ... @. The parallel
evaluation happens, when @xs@ is used for the first time. If @xs@ gets inlined
and used multiple times, the evaluation will be performed multiple times too.

'forceParFoldable' will /not/ copy the structure and evaluates the elements to
weak head normal form. If evaluation to normal form is needed, one might use
something like:

@ forceParDeep = 'forceParFoldable' . 'fmap' 'Control.DeepSeq.force' @

'forceParFoldable' adds strictness. In particular:

 1. If @xs@ is infinite, @ 'forceParFoldable' xs @ is bottom.

 2. If @xs@ contains bottom as element, @ 'forceParFoldable' xs @ might be
 bottom (non deterministic).
-}
forceParFoldable :: Foldable t => t a -> t a
forceParFoldable xs = forceParallel $ F.foldr par xs xs
