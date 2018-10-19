module Data.CuckooFilter
    (
    FingerPrint(..),
    Size,
    makeSize,

    Filter,
    empty,
    singleton,

    insert,
    lookup,
    delete
    ) where

import Data.Hashable (Hashable, hash)
import Data.Word (Word32, Word8)
import Numeric.Natural (Natural)
import qualified Data.Set as S

newtype Size = Size Natural
    deriving (Show, Eq, Ord)
makeSize :: Natural -> Maybe Size
makeSize n
    | n == 0 = Nothing
    | otherwise = Just . Size $ fromIntegral n

-- | A FingerPrint is a 7 bit hash of a value
newtype FingerPrint = FP Word8
    deriving (Show, Eq, Ord)
-- | A Bucket is a statically sized list of four FingerPrints
newtype Bucket = B (FingerPrint, FingerPrint, FingerPrint, FingerPrint)
    deriving (Show, Eq, Ord)
emptyBucket :: Bucket
emptyBucket = B (FP 0, FP 0, FP 0, FP 0)
-- TODO pull in a library for type

-- Initially going for correctness. Then measure it with benchmarks and tune it. Consider
-- unpacking and alternative data structures.
data Filter a = F {
    hashFunctionA :: !(a -> FingerPrint),
    hashFunctionB :: !(FingerPrint -> FingerPrint),
    buckets :: [Bucket], -- size / 4
    numBuckets :: !Natural, -- Track the number of buckets to avoid a length lookup
    load :: !Double, -- The current load ratio
    size :: !Size -- The number of buckets
    }

empty ::
    (a -> Hash) -- ^ First hash function
    -> (a -> Hash) -- ^ Second hash function
    -> Size -- ^ The initial size of the filter
    -> Filter a
empty hashA hashB (Size s) = F {
    hashFunctionA = hashA,
    hashFunctionB = hashB,
    buckets = replicate numBuckets emptyBucket,
    numBuckets = numBuckets,
    load = 0,
    size = Size s
    }
    where
        numBuckets = s `div` 4

insert :: (Hashable a) =>
    Filter a
    -> [a]
    -> Filter a
insert cfilt [] = cfilt
insert cfilt (x:xs) =
    (hashA cfilt) x


insertBucket ::
    FingerPrint
    -> Bucket
    -> Maybe Bucket
insertBucket fp (FP 0, a , b, c) =  Just (fp, a,b,c)
insertBucket fp (a, FP 0, b, c) = Just (a, fp, b, c)
insertBucket fp (a, b, FP 0, c) = Just (a, b, fp, c)
insertBucket fp (a, b, c, FP 0) = Just (a,b,c, fp)
insertBucket _ _ = Nothing

findBucket :: Hashable a =>
    a
    -> (Natural, [Bucket])
    -> Bucket
findBucket elem (size, buckets) =


