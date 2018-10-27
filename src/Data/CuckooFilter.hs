{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Data.CuckooFilter
Copyright   : (c) Chris Coffey, 2018
License     : MIT
Maintainer  : chris@foldl.io
Stability   : experimental

Cuckoo filters are an alternative data structure to Bloom filters. They use a different
approach to hashing items into the filter, which provides different behavior under load.

Inserting an item in to a Bloom filter always succeeds, although as the load factor on
the filter increases the false positive probability trends towards /%100/. Cuckoo filters
on the other hand hold the false positive probability constant under load, but will
begin to fail inserts.

Cuckoo filters also support deletion natively, which allows reclaiming some used space
from the filter to ward off insertion failures.

For more details see: Fan, . Cuckoo Filter: Practically Better Than Bloom. Retrieved August 22, 2016, from https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf
-}

module Data.CuckooFilter
    (
    -- * The Cuckoo Filter
    Size,
    makeSize,
    Filter,
    empty,

    -- * Working with a Cuckoo Filter
    insert,
    member,
    delete
    ) where

import Data.Hashable (Hashable)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)

import Data.CuckooFilter.Internal


-- | In exchange for the stable false-positive probability, insertion into a cuckoo filter
-- may fail as the load factor increases.
--
-- Amoritized /O(1)/
--
-- Note, because of how cuckoo hashing works, inserts will fail when there's a set of items /s/
-- that hash to the same fingerprint and share either IndexA or IndexB with probability /(2/numBuckets * 1/256)^ (|s|-1)/.
-- Alternatively, inserting the same item /2b+1/ times will trigger the failure as well.
--
insert :: (Hashable a) =>
    Filter a -- ^ Current filter state
    -> a -- ^ Item to hash and store in the filter
    -> Maybe (Filter a)
insert cfilt@(F {numBuckets}) val = let
    idxA = primaryIndex val numBuckets
    fp = makeFingerprint val
    bkts = buckets cfilt
    bucketA = fromMaybe emptyBucket $ toIndex numBuckets idxA `IM.lookup` bkts
    in case insertBucket fp bucketA of
        Just bucketA' -> Just $ cfilt {buckets = IM.insert (toIndex numBuckets idxA) bucketA' bkts}
        Nothing -> let
            idxB = secondaryIndex fp numBuckets idxA
            in bumpHash maxNumKicks cfilt idxB fp
    where
        (Size s) = size cfilt
        maxNumKicks = floor $ 0.1 * fromIntegral s

        -- The details of this algorithm can be found in https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf
        -- If the kick count is exhausted, the insert fails
        bumpHash 0 _ _ _ = Nothing
        bumpHash remaingKicks cfilt' idxB fp = let
            bkts = buckets cfilt'
            bucketB = fromMaybe emptyBucket $ toIndex numBuckets idxB `IM.lookup` bkts
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

-- | Checks whether a given item is within the filter.
--
-- /O(1)/
member :: (Hashable a) =>
    a -- ^ Check if this element is in the filter
    -> Filter a -- ^ The filter
    -> Bool
member a cFilter =
    inBucket fp bA || inBucket fp bB
    where
        bktCount = numBuckets cFilter
        fp = makeFingerprint a
        idxA = primaryIndex a bktCount
        idxB = secondaryIndex fp bktCount idxA
        bkts = buckets cFilter

        -- TODO Try to make this typesafe
        bA = fromMaybe emptyBucket $ toIndex bktCount idxA `IM.lookup` bkts
        bB = fromMaybe emptyBucket $ toIndex bktCount idxB `IM.lookup` bkts

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
--
-- /O(1)/
delete :: (Hashable a) =>
    Filter a
    -> a
    -> Filter a
delete cFilt@(F {numBuckets, buckets}) a
    | not $ member a cFilt = cFilt
    | otherwise = let
        bucketA = fromMaybe emptyBucket $ toIndex numBuckets idxA `IM.lookup` buckets
        bucketB = fromMaybe emptyBucket $ toIndex numBuckets idxB `IM.lookup` buckets
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
