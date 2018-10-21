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

-- | An Index represents the keys into buckets
newtype IndexA = IA Natural
    deriving (Show, Eq, Ord)
    deriving newtype Hashable
newtype IndexB = IB Natural
    deriving (Show, Eq, Ord)
    deriving newtype Hashable
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

insert :: (Hashable a) =>
    Filter a
    -> [a]
    -> Filter a
insert cfilt [] = cfilt
insert cfilt vals = let
    numBuckets = numBuckets cfilt
    -- TODO make this prettier
    fingerprints = (\x -> (primaryInde x numBuckets, makeFingerprint x)) <$> vals
    in doWork maxNumKicks cfilt fingerprints
    -- compute fingerprint of x
    -- compute indexA of x
    -- compute indexB of x = indexA `xor` hash fingerprint
    --
    -- try to insert into indexA, then indexB
    -- if indexB fails
    --     insert fingerprint, returning an old fingerprint (ofp)
    --     compute indexO via indexB `xor` hash ofp
    --     repeat until ticks are exhausted
    --
    --if ticks are exhausted, resize
    --
    where
        (Size s) = size cfilt
        maxNumKicks = floor $ 0.1 * s

        -- If the kick count is exhausted, the filter automatically resizes itself, then inserts the initial data. This
        -- is as expensive as you'd imagine, as it involves rehashing all of the indices
        doWork :: Natural -> Filter a -> [(IndexA, FingerPrint)] -> Filter a
        doWork 0 _ _ = insert (resize cfilt) vals
        doWork remaingKicks cfilt' ((val, fp):xs) = let
            bkts = buckets cfilt'
            bA = bkts IM.! fromIntegral idxA
            bB = bkts IM.! fromIntegral idxB
            in case (insertBucket fp bA, insertBucket fp bB) of
                (Just ba', _) -> cfilt' {buckets = IM.insert (fromIntegral idxA) ba' bkts }
                (_, Just bb') -> cfilt' {buckets = IM.insert (fromIntegral idxB) bb' bkts }
                (Nothing, Nothing) -> let
                    (bumpedFP, bucket') = replaceBucketMinimum fp bB
                    nextStepFilter = cfilt' {buckets = IM.insert (fromIntegral idxB) bucket' bkts }
                    in doWork (remaingKicks - 1) nextStepFilter (bumpedFP:xs)
            where
                (IA idxA, IB idxB) = calcIndices val fp cfilt'


        replaceBucketMinimum ::
            FingerPrint -- new value
            -> Bucket -- existing bucket
            -> (FingerPrint, Bucket) -- smallest old fingerprint and updated bucket
        replaceBucketMinimum fp (B (a,b,c,d)) = let
            m = min a . min b $ min c d
            in case (a == m, b == m, c == m, d == m) of
                (True, _, _, _) -> (a, B (fp, b, c, d))
                (_, True, _, _) -> (b, B (a, fp, c, d))
                (_, _, True, _) -> (c, B (a, b, fp, d))
                (_, _, _, True) -> (d, B (a, b, c, fp))

primaryIndex :: Hashable a =>
    a
    -> Natural
    -> IndexA
primaryIndex a numBuckets = FP $ hash a `mod` numBuckets

secondaryIndex ::
    FingerPrint
    -> IndexA
    -> IndexB
secondaryIndex (FP fp) (IA primary) = IB $ primary `xor` hash fp

kickedSecondaryIndex ::
    FingerPrint
    -> IndexB
    -> IndexA -- This is because there really isn't a primary or secondary just a or b
kickedSecondaryIndex (FP fp) (IB alt) = IA $ alt `xor` hash fp


member :: (Hashable a) =>
    a
    -> Filter a
    -> Bool
member a cFilter =
    inBucket fp bA || inBucket fp bB
    where
        fp = makeFingerprint a
        (IA idxA, IB idxB) = calcIndices a fp cFilter

        -- TODO Try to make this typesafe
        bA = buckets cFilter IM.! fromIntegral idxA
        bB = buckets cFilter IM.! fromIntegral idxB

        -- fp `elem` [a,b,c,d] is simpler, but it allocates an additional list unnecessarily
        inBucket fp (B (a,b,c,d)) = fp == a || fp == b || fp == c || fp == d

-- | Find the primary and alternative index for an element
calcIndices :: Hashable a =>
    a
    -> FingerPrint
    -> Filter a
    -> (IndexA, IndexB)
calcIndices a fp F {numBuckets} =
    (IA $ idxA `mod` numBuckets, IB $ idxB `mod` numBuckets)
    where
        (IA idxA) = hashFunctionA a
        (IB idxB) = hashFunctionB fp (IA idxA)

makeFingerprint :: Hashable a =>
    a
    -> FingerPrint
makeFingerprint a = FP . fromIntegral $ hash a `mod` 255


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

resize ::
    Filter a
    -> Filter a
resize old =  undefined
    -- double the size when capacity is maxed
    -- reinsert all elements
