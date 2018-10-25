{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.CuckooFilter
    (
    -- * The Cuckoo Filter
    Size,
    makeSize,
    Filter,
    empty,
    falsePositiveProbability,

    -- * Public API
    insert,
    member,
    delete,
    resize,

    -- * Utilities exported for testing

    ) where

import qualified Data.IntMap.Strict as IM
import Data.Hashable (Hashable)

import Data.CuckooFilter.Internal

-- | TODO document the equation behind how fpp is calculated
falsePositiveProbability ::
    Filter a
    -> Double
falsePositiveProbability F {size, numBuckets} =
    undefined


-- | TODO add documentation outlining the algorithm in psuedocode
insert :: (Hashable a) =>
    Filter a
    -> a
    -> Maybe (Filter a)
insert cfilt@(F {numBuckets}) val = let
    idxA = primaryIndex val numBuckets
    fp = makeFingerprint val
    bkts = buckets cfilt
    bucketA = bkts IM.! toIndex numBuckets idxA
    in case insertBucket fp bucketA of
        Just bucketA' -> Just $ cfilt {buckets = IM.insert (toIndex numBuckets idxA) bucketA' bkts}
        Nothing -> let
            idxB = secondaryIndex fp numBuckets idxA
            in bumpHash maxNumKicks cfilt idxB fp
    where
        (Size s) = size cfilt
        maxNumKicks = floor $ 0.1 * fromIntegral s

        -- If the kick count is exhausted, the insert fails
        bumpHash 0 _ _ _ = Nothing
        bumpHash remaingKicks cfilt' idxB fp = let
            bkts = buckets cfilt'
            bucketB = bkts IM.! toIndex numBuckets idxB
            in case insertBucket fp bucketB of
                Just bb' -> Just $ cfilt' {buckets = IM.insert (toIndex numBuckets idxB) bb' bkts }
                Nothing -> let
                    (bumpedFP, bucketB') = replaceInBucket fp isBucketMinimum bucketB
                    nextStepFilter = cfilt' {buckets = IM.insert (toIndex numBuckets idxB) bucketB' bkts }
                    kickedIndex = kickedSecondaryIndex bumpedFP numBuckets idxB
                    in bumpHash (remaingKicks - 1) nextStepFilter kickedIndex bumpedFP

        isBucketMinimum _ bkt = let
            a = getCell bkt 0
            b = getCell bkt 1
            c = getCell bkt 2
            d = getCell bkt 3
            m = min a . min b $ min c d
            in (a == m, b == m, c == m, d == m)


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
        bA = buckets cFilter IM.! toIndex bktCount idxA
        bB = buckets cFilter IM.! toIndex bktCount idxB

        -- fp `elem` [a,b,c,d] is simpler, but it allocates an additional list unnecessarily
        inBucket fp bucket =
            fp == getCell bucket 0 ||
            fp == getCell bucket 1 ||
            fp == getCell bucket 2 ||
            fp == getCell bucket 3



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
        bucketA = buckets IM.! toIndex numBuckets idxA
        bucketB = buckets IM.! toIndex numBuckets idxB
        (removedFromA, bucketA') = removeFromBucket bucketA
        (_, bucketB') = removeFromBucket bucketB
        in if removedFromA
           then cFilt {buckets = IM.insert (toIndex numBuckets idxA) bucketA' buckets}
           else cFilt {buckets = IM.insert (toIndex numBuckets idxB) bucketB' buckets}
    where
        fp = makeFingerprint a
        idxA = primaryIndex a numBuckets
        idxB = secondaryIndex fp numBuckets idxA
        -- TODO just use Control.Arrow
        matchesFP _ bucket = (fp == getCell bucket 0,
                              fp == getCell bucket 1,
                              fp == getCell bucket 2,
                              fp == getCell bucket 3)
        removeFromBucket bucket = let
            (_, bucket') = replaceInBucket (FP 0) matchesFP bucket
            in (bucket /= bucket', bucket')


-- | This is expensive, so it aggressively increases its size. This will cause problems
-- for filters that are already large, so its worth modifying a 'Filter' to accept the
-- scaling factor.
--
-- The algorithm is :
-- 1) Create an empty filter of twice the size as the current one
--
-- 2) Re-index all stored elements into the new filter
--
-- 3) Return the new filter
--
-- can this be implemented without the source values? The new indices cannot be computed from only the fingerprints, they require both an index and fingerprint...
-- Consider tweaking insert to fail
resize ::
    Filter a
    -> Filter a
resize F {buckets, numBuckets, size} =
    undefined
    where
        s' = Size (s*2)
        (Size s) = size
        -- vals = [ undefined | n <- elems buckets, x <- [0..3] ]


