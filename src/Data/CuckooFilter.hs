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
    MFilter,
    initialize,

    -- * Working with a Cuckoo Filter
    insert,
    member,
    delete
    ) where

import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)

import Data.CuckooFilter.Internal
import Data.CuckooFilter.Pure
import Data.CuckooFilter.Mutable


-- | In exchange for the stable false-positive probability, insertion into a cuckoo filter
-- may fail as the load factor increases.
--
-- Amoritized /O(1)/
--
-- Note, because of how cuckoo hashing works, inserts will fail when there's a set of items /s/
-- that hash to the same fingerprint and share either IndexA or IndexB with probability /(2/numBuckets * 1/256)^ (|s|-1)/.
-- Alternatively, inserting the same item /2b+1/ times will trigger the failure as well.
--
insert :: (Hashable a, Monad m, CuckooFilter filt m) =>
    filt a -- ^ Current filter state
    -> a -- ^ Item to hash and store in the filter
    -> m (Maybe (filt a))
insert cfilt val = do
    numBuckets <- bucketCount cfilt
    let idxA = primaryIndex val numBuckets
        fp = makeFingerprint val
    bucketA <- readBucket (toIndex numBuckets idxA) cfilt
    case insertBucket fp bucketA of
        Just bucketA' ->
            Just <$> writeBucket (toIndex numBuckets idxA) bucketA' cfilt
        Nothing -> let
            idxB = secondaryIndex fp numBuckets idxA
            in bumpHash numBuckets maxNumKicks cfilt idxB fp
    where
        maxNumKicks = 1200

        -- The details of this algorithm can be found in https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf
        -- If the kick count is exhausted, the insert fails. Otherwise, it will loop until it finds an open cell,
        -- insert the value, then return the filter
        bumpHash numBuckets 0 _ _ _ = pure Nothing
        bumpHash numBuckets remaingKicks cfilt' idxB fp = do
            bucketB <- readBucket (toIndex numBuckets idxB) cfilt'
            case insertBucket fp bucketB of
                Just bb' ->
                    Just <$> writeBucket (toIndex numBuckets idxB) bb' cfilt
                Nothing -> do
                    let (bumpedFP, bucketB') = replaceInBucket fp isBucketMinimum bucketB
                        kickedIndex = kickedSecondaryIndex bumpedFP numBuckets idxB
                    nextStepFilter <- writeBucket (toIndex numBuckets idxB) bucketB' cfilt'
                    bumpHash numBuckets (remaingKicks - 1) nextStepFilter kickedIndex bumpedFP

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
member :: (Hashable a, Monad m, CuckooFilter filt m) =>
    a -- ^ Check if this element is in the filter
    -> filt a -- ^ The filter
    -> m Bool
member a cFilter = do
    numBuckets <- bucketCount cFilter
    let idxA = primaryIndex a numBuckets
        idxB = secondaryIndex fp numBuckets idxA
    bA <- readBucket ( toIndex numBuckets idxA ) cFilter
    bB <- readBucket ( toIndex numBuckets idxB ) cFilter
    pure $ inBucket fp bA || inBucket fp bB
    where
        fp = makeFingerprint a

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
delete :: (Hashable a, Monad m, CuckooFilter filt m) =>
    filt a
    -> a
    -> m (filt a)
delete cFilt a = do
    isMember <- member a cFilt
    if isMember
    then do
        numBuckets <- bucketCount cFilt
        let idxA = primaryIndex a numBuckets
            idxB = secondaryIndex fp numBuckets idxA
        bucketA <- readBucket ( toIndex numBuckets idxA ) cFilt
        bucketB <- readBucket ( toIndex numBuckets idxB ) cFilt
        let (removedFromA, bucketA') = removeFromBucket bucketA
            (_, bucketB') = removeFromBucket bucketB
        if removedFromA
        then writeBucket (toIndex numBuckets idxA) bucketA' cFilt
        else writeBucket (toIndex numBuckets idxB) bucketB' cFilt
    else pure cFilt

    where
        fp = makeFingerprint a
        -- TODO just use Control.Arrow
        matchesFP _ bucket = (fp == getCell bucket 0,
                              fp == getCell bucket 1,
                              fp == getCell bucket 2,
                              fp == getCell bucket 3)
        removeFromBucket bucket = let
            (_, bucket') = replaceInBucket (FP 0) matchesFP bucket
            in (bucket /= bucket', bucket')
