{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

class Index a where
    toIndex :: a -> Int

-- | An Index represents the keys into buckets
newtype IndexA = IA Natural
    deriving (Show, Eq, Ord)
    deriving newtype Hashable
instance Index IndexA where
    toIndex (IA n) = fromIntegral n

newtype IndexB = IB Natural
    deriving (Show, Eq, Ord)
    deriving newtype Hashable
instance Index IndexB where
    toIndex (IB n) = fromIntegral n

-- | A FingerPrint is an 8 bit hash of a value
newtype FingerPrint = FP Word8
    deriving (Show, Eq, Ord)
    deriving newtype Hashable
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
    buckets :: IM.IntMap Bucket, -- size / 4.
    numBuckets :: !Natural, -- Track the number of buckets to avoid a length lookup
    load :: !Double, -- The current load ratio
    size :: !Size -- The number of buckets
    }



empty ::
    Size -- ^ The initial size of the filter
    -> Filter a
empty (Size s) = F {
    buckets = IM.fromList [(fromIntegral x, emptyBucket) | x <- [0..numBuckets]],
    numBuckets = numBuckets,
    load = 0,
    size = Size s
    }
    where
        numBuckets = s `div` 4

-- | TODO add documentation outlining the algorithm in psuedocode
insert :: (Hashable a) =>
    Filter a
    -> a
    -> Filter a
insert cfilt@(F {numBuckets}) val = let
    idxA = primaryIndex val numBuckets
    fp = makeFingerprint val
    bkts = buckets cfilt
    bucketA = bkts IM.! toIndex idxA
    in case insertBucket fp bucketA of
        Just bucketA' -> cfilt {buckets = IM.insert (toIndex idxA) bucketA bkts}
        Nothing -> let
            idxB = secondaryIndex fp numBuckets idxA
            in bumpHash maxNumKicks cfilt idxB fp
    where
        (Size s) = size cfilt
        maxNumKicks = floor $ 0.1 * fromIntegral s

        -- If the kick count is exhausted, the filter automatically resizes itself, then inserts the initial data. This
        -- is as expensive as you'd imagine, as it involves rehashing all of the indices
        bumpHash 0 _ _ _ = insert (resize cfilt) val
        bumpHash remaingKicks cfilt' idxB fp = let
            bkts = buckets cfilt'
            bucketB = bkts IM.! toIndex idxB
            in case insertBucket fp bucketB of
                Just bb' -> cfilt' {buckets = IM.insert (toIndex idxB) bb' bkts }
                Nothing -> let
                    (bumpedFP, bucketB') = replaceInBucket fp isBucketMinimum bucketB
                    nextStepFilter = cfilt' {buckets = IM.insert (toIndex idxB) bucketB' bkts }
                    kickedIndex = kickedSecondaryIndex bumpedFP numBuckets idxB
                    in bumpHash (remaingKicks - 1) nextStepFilter kickedIndex bumpedFP

        isBucketMinimum _ (B (a,b,c,d)) = let
            m = min a . min b $ min c d
            in (a == m, b == m, c == m, d == m)


replaceInBucket ::
    FingerPrint
    -> (FingerPrint -> Bucket -> (Bool, Bool, Bool, Bool)) -- ^ Bucket predicate
    -> Bucket -- existing bucket
    -> (FingerPrint, Bucket) -- Removed fingerprint and latest bucket state
replaceInBucket fp predicate bucket@(B (a,b,c,d)) = let
    results = predicate fp bucket
    in case results of
        (True, _, _, _) -> (a, B (fp, b, c, d))
        (_, True, _, _) -> (b, B (a, fp, c, d))
        (_, _, True, _) -> (c, B (a, b, fp, d))
        (_, _, _, True) -> (d, B (a, b, c, fp))
        _ -> (fp, bucket)




member :: (Hashable a) =>
    a
    -> Filter a
    -> Bool
member a cFilter =
    inBucket fp bA || inBucket fp bB
    where
        bktCount = numBuckets cFilter
        fp = makeFingerprint a
        idxA = primaryIndex a bktCount
        idxB = secondaryIndex fp bktCount idxA

        -- TODO Try to make this typesafe
        bA = buckets cFilter IM.! toIndex idxA
        bB = buckets cFilter IM.! toIndex idxB

        -- fp `elem` [a,b,c,d] is simpler, but it allocates an additional list unnecessarily
        inBucket fp (B (a,b,c,d)) = fp == a || fp == b || fp == c || fp == d


--
-- Algorithms for creating indexes/ hashes
--

makeFingerprint :: Hashable a =>
    a
    -> FingerPrint
makeFingerprint a = FP . fromIntegral $ hash a `mod` 255

primaryIndex :: Hashable a =>
    a
    -> Natural
    -> IndexA
primaryIndex a numBuckets = IA $ fromIntegral (hash a) `mod` numBuckets

secondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexA
    -> IndexB
secondaryIndex (FP fp) numBuckets (IA primary) =
    IB . (`mod` numBuckets) $ primary `xor` fromIntegral (hash fp)

kickedSecondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexB
    -> IndexB
kickedSecondaryIndex (FP fp) numBuckets (IB alt) =
    IB . (`mod` numBuckets) $ alt `xor` fromIntegral (hash fp)

-- | Deletes a single occurance of 'a' from the 'Filter'. It first checks for a value
-- in the primary index, then in the secondary index.
--
-- Deleting an element not in the Cuckoo Filter is a noop and returns the filter unchanged.
delete :: (Hashable a) =>
    Filter a
    -> a
    -> Filter a
delete cFilt@(F {numBuckets, buckets}) a
    | not $ member a cFilt = cFilt
    | otherwise = let
        bucketA = buckets IM.! toIndex idxA
        bucketB = buckets IM.! toIndex idxB
        (removedFromA, bucketA') = removeFromBucket bucketA
        (_, bucketB') = removeFromBucket bucketB
        in if removedFromA
           then cFilt {buckets = IM.insert (toIndex idxA) bucketA buckets}
           else cFilt {buckets = IM.insert (toIndex idxB) bucketB' buckets}
    where
        fp = makeFingerprint a
        idxA = primaryIndex fp numBuckets
        idxB = secondaryIndex fp numBuckets idxA
        -- TODO just use Control.Arrow
        matchesFP _ (B (a,b,c,d))= (fp == a, fp == b, fp == c, fp == d)
        removeFromBucket bucket = let
            (_, bucket') = replaceInBucket (FP 0) matchesFP  bucket
            in (bucket /= bucket', bucket')

insertBucket ::
    FingerPrint
    -> Bucket
    -> Maybe Bucket
insertBucket fp (B (FP 0, a , b, c)) =  Just $ B (fp, a,b,c)
insertBucket fp (B (a, FP 0, b, c)) = Just $ B (a, fp, b, c)
insertBucket fp (B (a, b, FP 0, c)) = Just $ B (a, b, fp, c)
insertBucket fp (B (a, b, c, FP 0)) = Just $ B (a,b,c, fp)
insertBucket _ _ = Nothing

resize ::
    Filter a
    -> Filter a
resize old =  undefined
    -- double the size when capacity is maxed
    -- reinsert all elements
