{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Data.CuckooFilter.Internal
Description : Internal functions and data types for Data.CuckooFilter
Copyright   : (c) Chris Coffey, 2018
License     : MIT
Maintainer  : chris@foldl.io
Stability   : experimental

This is the internal API and implemntation of 'Data.CuckooFilter'. It is subject to
change at any time and should not be used. Instead, use the exports from 'Data.CuckooFilter'.
-}

module Data.CuckooFilter.Internal (
    -- * Constructing a Cuckoo Filter
    Size(..),
    makeSize,
    Filter(..),
    empty,

    -- * Fingerprints
    FingerPrint(..),
    emptyFP,
    makeFingerprint,

    -- * Working with indices
    Bucket(..),
    emptyBucket,
    Index(..),
    IndexA(..),
    IndexB(..),
    replaceInBucket,
    insertBucket,
    primaryIndex,
    secondaryIndex,
    kickedSecondaryIndex,

    -- ** Bucket Cells,
    getCell,
    setCell
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Bits (xor, (.&.), (.|.), shiftR, shiftL)
import Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IM
import Data.Hashable (Hashable, hash)
import Data.Serialize (Serialize)
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | A non-zero natural number. Generally this is a power of two, although there's no hard requirement
-- for that given the current implementation.
newtype Size = Size Natural
    deriving (Show, Eq, Ord)
    deriving stock Generic
    deriving newtype (Serialize, ToJSON, FromJSON)
-- | Safely make a 'Size' or fail if a 0 is provided.
makeSize :: Natural -> Maybe Size
makeSize n
    | n == 0 = Nothing
    | otherwise = Just . Size $ fromIntegral n

class Index a where
    toIndex :: Natural -> a -> Int

-- | An Index represents the keys into buckets
newtype IndexA = IA Word32
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Hashable)
    deriving anyclass Serialize
instance Index IndexA where
    toIndex numBuckets (IA n) = fromIntegral n `mod` fromIntegral numBuckets

newtype IndexB = IB Word32
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Hashable)
    deriving anyclass Serialize
instance Index IndexB where
    toIndex numBuckets (IB n) = fromIntegral n `mod` fromIntegral numBuckets

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

-- | A Cuckoo Filter with a fixed size. The current implementation uses 8 bit fingerprints
-- and 4 element buckets.
data Filter a = F {
    buckets :: IM.IntMap Bucket, -- size / 4.
    numBuckets :: !Natural, -- Track the number of buckets to avoid a length lookup
    size :: !Size -- The number of buckets
    }
    deriving (Show, Eq, Generic, Serialize, ToJSON, FromJSON)

-- | Creates a new & empty 'Filter' of size s
empty ::
    Size -- ^ The initial size of the filter
    -> Filter a
empty (Size s) = F {
    -- By using an empty map, we're able to avoid allocating any memory for elements that aren't stored.
    -- If the filter is packed densely the additional memory for the IntMap hurts quite a bit, but at load
    -- factors
    buckets = IM.empty,
    numBuckets = numBuckets,
    size = Size s
    }
    where
        addBucket rawFilt n = IM.insert (fromIntegral n) emptyBucket  rawFilt
        numBuckets = s `div` 4

--
-- Working with Buckets
--
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

--
-- Index and hashes
--

-- | hash a % 255. Fingerprints are 8 bits each, and completely opaque to the
-- lookup algorithm.
makeFingerprint :: Hashable a =>
    a
    -> FingerPrint
makeFingerprint a = FP . max 1 $  fromIntegral (abs $ hash a) `mod` 255

-- | (hash a) % numBuckets
primaryIndex :: Hashable a =>
    a
    -> Natural
    -> IndexA
primaryIndex a numBuckets =
    IA . fromIntegral $ hash a

-- | (indexA `xor` hash fp) % numBuckets
secondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexA
    -> IndexB
secondaryIndex fp numBuckets (IA primary) =
    IB (primary `xor` fpHash)
    where
        fpHash = fromIntegral $ hash fp

kickedSecondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexB
    -> IndexB
kickedSecondaryIndex fp numBuckets (IB alt) =
    secondaryIndex fp numBuckets (IA alt)
