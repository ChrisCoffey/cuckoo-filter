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
    delete
    ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Bits (xor, (.&.), (.|.), shiftR, shiftL)
import Data.Hashable (Hashable, hash)
import qualified Data.IntMap.Strict as IM
import Data.Serialize (Serialize)
import qualified Data.Set as S
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Debug.Trace

newtype Size = Size Natural
    deriving (Show, Eq, Ord)
    deriving stock Generic
    deriving newtype (Serialize, ToJSON, FromJSON)
makeSize :: Natural -> Maybe Size
makeSize n
    | n == 0 = Nothing
    | otherwise = Just . Size $ fromIntegral n

class Index a where
    toIndex :: a -> Int

-- | An Index represents the keys into buckets
newtype IndexA = IA Natural
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Hashable)
    deriving anyclass Serialize
instance Index IndexA where
    toIndex (IA n) = fromIntegral n

newtype IndexB = IB Natural
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Hashable)
    deriving anyclass Serialize
instance Index IndexB where
    toIndex (IB n) = fromIntegral n

-- | A FingerPrint is an 8 bit hash of a value
newtype FingerPrint = FP Word8
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Hashable)
    deriving anyclass Serialize
emptyFP :: FingerPrint
emptyFP = FP 0
-- | A Bucket is a statically sized list of four FingerPrints.
--
newtype Bucket = B Word32
    deriving (Show, Ord)
    deriving stock Generic
    deriving newtype (ToJSON, FromJSON, Eq)
    deriving anyclass Serialize
emptyBucket :: Bucket
emptyBucket = B 0

getCell ::
    Bucket
    -> Natural -- Really just 0-3. Is it worth creating a custom datatype for this?
    -> FingerPrint
getCell (B bucket) cellNumber =
    FP . fromIntegral $ (bucket .&. mask) `shiftR` offset
    where
        offset = (fromIntegral cellNumber) * 8
        mask = (255 :: Word32) `shiftL` offset

setCell ::
    Bucket
    -> Natural
    -> FingerPrint
    -> Bucket
setCell (B bucket) cellNumber (FP fp) =
    B $ zeroed .|. mask
    where
        offset = (fromIntegral cellNumber) * 8
        zeroed = (bucket .|. zeroMask) `xor` zeroMask
        zeroMask = (255 :: Word32) `shiftL` offset
        mask = (fromIntegral fp :: Word32) `shiftL` offset

-- Initially going for correctness. Then measure it with benchmarks and tune it. Consider
-- unpacking and alternative data structures.
data Filter a = F {
    buckets :: IM.IntMap Bucket, -- size / 4.
    numBuckets :: !Natural, -- Track the number of buckets to avoid a length lookup
    size :: !Size -- The number of buckets
    }
    deriving (Show, Eq, Generic, Serialize, ToJSON, FromJSON)


-- | TODO document the equation behind how fpp is calculated
falsePositiveProbability ::
    Filter a
    -> Double
falsePositiveProbability F {size, numBuckets} =
    undefined

empty ::
    Size -- ^ The initial size of the filter
    -> Filter a
empty (Size s) = F {
    buckets = IM.fromList [(fromIntegral x, emptyBucket) | x <- [0..numBuckets]],
    numBuckets = numBuckets,
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
        Just bucketA' -> cfilt {buckets = IM.insert (toIndex idxA) bucketA' bkts}
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

        isBucketMinimum _ bkt = let
            a = getCell bkt 0
            b = getCell bkt 1
            c = getCell bkt 2
            d = getCell bkt 3
            m = min a . min b $ min c d
            in (a == m, b == m, c == m, d == m)

replaceInBucket ::
    FingerPrint
    -> (FingerPrint -> Bucket -> (Bool, Bool, Bool, Bool)) -- ^ Bucket predicate
    -> Bucket -- existing bucket
    -> (FingerPrint, Bucket) -- Removed fingerprint and latest bucket state
replaceInBucket fp predicate bucket = let
    results = predicate fp bucket
    in case results of
        (True, _, _, _) -> (getCell bucket 0, setCell bucket 0 fp)
        (_, True, _, _) -> (getCell bucket 1, setCell bucket 1 fp)
        (_, _, True, _) -> (getCell bucket 2, setCell bucket 2 fp)
        (_, _, _, True) -> (getCell bucket 3, setCell bucket 3 fp)
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
        inBucket fp bucket =
            fp == getCell bucket 0 ||
            fp == getCell bucket 1 ||
            fp == getCell bucket 2 ||
            fp == getCell bucket 3


--
-- Algorithms for creating indexes/ hashes
--

makeFingerprint :: Hashable a =>
    a
    -> FingerPrint
makeFingerprint a = FP . max 1 $  fromIntegral (natHash a) `mod` 255

primaryIndex :: Hashable a =>
    a
    -> Natural
    -> IndexA
primaryIndex a numBuckets = IA $ natHash a `mod` numBuckets

secondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexA
    -> IndexB
secondaryIndex (FP fp) numBuckets (IA primary) =
    IB . (`mod` numBuckets) $ primary `xor` natHash fp

kickedSecondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexB
    -> IndexB
kickedSecondaryIndex (FP fp) numBuckets (IB alt) =
    IB . (`mod` numBuckets) $ alt `xor` natHash fp

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
           then cFilt {buckets = IM.insert (toIndex idxA) bucketA' buckets}
           else cFilt {buckets = IM.insert (toIndex idxB) bucketB' buckets}
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

insertBucket ::
    FingerPrint
    -> Bucket
    -> Maybe Bucket
insertBucket fp bucket =
    case (a,b,c,d) of
        (True, _, _, _) -> Just $ setCell bucket 0 fp
        (_, True, _, _) -> Just $ setCell bucket 1 fp
        (_, _, True, _) -> Just $ setCell bucket 2 fp
        (_, _, _, True) -> Just $ setCell bucket 3 fp
        _ -> Nothing
    where
        -- TODO factor out all of this duplicated code
        a = emptyFP == getCell bucket 0
        b = emptyFP == getCell bucket 1
        c = emptyFP == getCell bucket 2
        d = emptyFP == getCell bucket 3

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


natHash :: Hashable a =>
    a ->
    Natural
natHash = fromIntegral . abs . hash
