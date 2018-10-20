module Data.CuckooFilter
    (
    -- * The Cuckoo Filter
    FingerPrint(..),
    Index(..),
    Size,
    makeSize,
    Filter,
    empty,
    load,

    insert,
    member,
    delete
    ) where

import Data.Hashable (Hashable, hash)
import Data.Word (Word32, Word8)
import Numeric.Natural (Natural)
import Data.Bits (xor)
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM

newtype Size = Size Natural
    deriving (Show, Eq, Ord)
makeSize :: Natural -> Maybe Size
makeSize n
    | n == 0 = Nothing
    | otherwise = Just . Size $ fromIntegral n

-- | An Index represents the keys into buckets
newtype Index = I Natural
    deriving (Show, Eq, Ord)
-- | A FingerPrint is a 7 bit hash of a value
newtype FingerPrint = FP Word8
    deriving (Show, Eq, Ord)
-- | A Bucket is a statically sized list of four FingerPrints.
--
-- TODO use a Word32 instead of a 4-tuple
newtype Bucket = B (FingerPrint, FingerPrint, FingerPrint, FingerPrint)
    deriving (Show, Eq, Ord)
emptyBucket :: Bucket
emptyBucket = B (FP 0, FP 0, FP 0, FP 0)

-- Initially going for correctness. Then measure it with benchmarks and tune it. Consider
-- unpacking and alternative data structures.
data Filter a = F {
    hashFunctionA :: !(a -> Index),
    hashFunctionB :: !(Index -> Index),
    buckets :: IM.IntMap Bucket, -- size / 4.
    numBuckets :: !Natural, -- Track the number of buckets to avoid a length lookup
    load :: !Double, -- The current load ratio
    size :: !Size -- The number of buckets
    }

empty ::
    (a -> Index) -- ^ First hash function
    -> (Index -> Index) -- ^ Second hash function
    -> Size -- ^ The initial size of the filter
    -> Filter a
empty hashA hashB (Size s) = F {
    hashFunctionA = hashA,
    hashFunctionB = hashB,
    buckets = IM.fromList [(fromIntegral x, emptyBucket) | x <- [0..numBuckets]],
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
insert cfilt (x:xs) = undefined

member :: (Hashable a) =>
    a
    -> Filter a
    -> Bool
member a cFilter =
    inBucket fp bA || inBucket fp bB
    where
        (fp, I idxA, I idxB) = fpAndIndices a cFilter

        -- TODO Try to make this typesafe
        bA = buckets cFilter IM.! fromIntegral idxA
        bB = buckets cFilter IM.! fromIntegral idxA

        -- fp `elem` [a,b,c,d] is simpler, but it allocates an additional list unnecessarily
        inBucket fp (B (a,b,c,d)) = fp == a || fp == b || fp == c || fp == d

-- | Find the  fingerprint, primary and alternative index for an element
fpAndIndices :: Hashable a =>
    a
    -> Filter a
    -> (FingerPrint, Index, Index)
fpAndIndices a F {hashFunctionA, hashFunctionB, numBuckets} =
    (FP fp, I (idxA `mod` numBuckets), I idxB)
    where
        fp = fromIntegral $ hash a `mod` 255
        (I idxA) = hashFunctionA a
        (I hashB) = hashFunctionB (I idxA)
        idxB = (hashB `xor` idxA) `mod` numBuckets


delete :: (Hashable a) =>
    Filter a
    -> a
    -> Filter a
delete cFilt a = undefined

insertBucket ::
    FingerPrint
    -> Bucket
    -> Maybe Bucket
insertBucket fp (B (FP 0, a , b, c)) =  Just $ B (fp, a,b,c)
insertBucket fp (B (a, FP 0, b, c)) = Just $ B (a, fp, b, c)
insertBucket fp (B (a, b, FP 0, c)) = Just $ B (a, b, fp, c)
insertBucket fp (B (a, b, c, FP 0)) = Just $ B (a,b,c, fp)
insertBucket _ _ = Nothing

