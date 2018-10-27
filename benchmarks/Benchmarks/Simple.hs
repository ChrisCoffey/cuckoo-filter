module Benchmarks.Simple (
    stdInBenchmark,
    tenPctPacked,
    fiftyPctPacked,
    ninetyPctPacked
) where

import Criterion
import Control.Monad (foldM)
import Data.CuckooFilter
import Data.Ratio (Ratio)
import Numeric.Natural (Natural)
import System.Environment
import System.Random

stdInBenchmark :: IO ()
stdInBenchmark = do
    [n, m] <- fmap read <$> getArgs
    let (Just s) = makeSize n
        filt = empty s
    print s
    filt' <- pure $ foldM (\ f a -> f `insert` a) filt [1..m]
    print $ member 1 <$> filt'

tenPctPacked :: Benchmark
tenPctPacked = bgroup "10% packed" [
    bench "Store 100k, 1% dupes" $ whnf (doTest oneMM (LF 10)) (D 1),
    bench "Store 100k, 10% dupes" $ whnf (doTest oneMM (LF 10)) (D 10),
    bench "Store 100k, 50% dupes" $ whnf (doTest oneMM (LF 10)) (D 50),
    bench "Store 10MM, 1% dupes" $ whnf (doTest oneHundredMM (LF 10)) (D 1),
    bench "Store 10MM, 10% dupes" $ whnf (doTest oneHundredMM (LF 10)) (D 10),
    bench "Store 10MM, 50% dupes" $ whnf (doTest oneHundredMM (LF 10)) (D 50)
    ]

fiftyPctPacked :: Benchmark
fiftyPctPacked = bgroup "50% packed" [
    bench "Store 500k, 1% dupes" $ whnf (doTest oneMM (LF 50)) (D 1),
    bench "Store 500k, 10% dupes" $ whnf (doTest oneMM (LF 50)) (D 10),
    bench "Store 500k, 50% dupes" $ whnf (doTest oneMM (LF 50)) (D 50),
    bench "Store 50MM, 1% dupes" $ whnf (doTest oneHundredMM (LF 50)) (D 1),
    bench "Store 50MM, 10% dupes" $ whnf (doTest oneHundredMM (LF 50)) (D 10),
    bench "Store 50MM, 50% dupes" $ whnf (doTest oneHundredMM (LF 50)) (D 50)
    ]

ninetyPctPacked :: Benchmark
ninetyPctPacked = bgroup "90% packed" [
    bench "Store 900k, 1% dupes" $ whnf (doTest oneMM (LF 90)) (D 1),
    bench "Store 900k, 10% dupes" $ whnf (doTest oneMM (LF 90)) (D 10),
    bench "Store 900k, 50% dupes" $ whnf (doTest oneMM (LF 90)) (D 50),
    bench "Store 90MM, 1% dupes" $ whnf (doTest oneHundredMM (LF 10)) (D 1),
    bench "Store 90MM, 10% dupes" $ whnf (doTest oneHundredMM (LF 10)) (D 10),
    bench "Store 90MM, 50% dupes" $ whnf (doTest oneHundredMM (LF 10)) (D 50)
    ]

oneMM :: Natural
oneMM = 1000000
oneHundredMM :: Natural
oneHundredMM = oneMM * 100
newtype DupePct = D Natural
newtype LoadFactor = LF Natural

doTest ::
    Natural
    -> LoadFactor
    -> DupePct
    -> Maybe (Filter Int)
doTest size (LF lf) (D d) =
    foldM insert (empty s) vals
    where
        valCount :: Int
        valCount = floor $ (fromIntegral size) * (realToFrac lf / 100.0)
        dupeCount :: Int
        dupeCount = floor $ (fromIntegral valCount) * (realToFrac d / 100.0)
        dupes = if d > 0
                then [1.. dupeCount]
                else []
        vals = dupes <> [1..(valCount - dupeCount)]
        Just s = makeSize size
