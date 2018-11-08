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
    CuckooFilter(..),

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

-- | A low-level interface for working with cuckoo filter storage.
class Monad m => CuckooFilter filt m where
    -- | Create a new cuckoo filter of the specified size
    initialize :: Size -> m (filt a)

    -- | Return the number of buckets contained in the filter. This is distinct from the total size of the filter (size /4)
    bucketCount :: filt a -> m Natural

    -- | Write the new contents of a bucket to the storage
    writeBucket :: Int -> Bucket -> filt a -> m (filt a)

    -- | Read the contents of a bucket from the storage
    readBucket :: Int -> filt a -> m Bucket


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
{-# INLINE getCell #-}
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
{-# INLINE setCell #-}
setCell (B bucket) cellNumber (FP fp) =
    B $ zeroed .|. mask
    where
        offset = (fromIntegral cellNumber) * 8
        zeroed = (bucket .|. zeroMask) `xor` zeroMask
        zeroMask = (255 :: Word32) `shiftL` offset
        mask = (fromIntegral fp :: Word32) `shiftL` offset


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
{-# INLINE makeFingerprint #-}
makeFingerprint a = FP . max 1 $  fromIntegral (abs $ hash a) `mod` 255

-- | (hash a) % numBuckets
primaryIndex :: Hashable a =>
    a
    -> Natural
    -> IndexA
{-# INLINE primaryIndex #-}
primaryIndex a numBuckets =
    IA . fromIntegral $ hash a

-- | (indexA `xor` hash fp) % numBuckets
secondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexA
    -> IndexB
{-# INLINE secondaryIndex #-}
secondaryIndex fp numBuckets (IA primary) =
    IB (primary `xor` fpHash)
    where
        fpHash = fromIntegral $ hash fp

kickedSecondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexB
    -> IndexB
{-# INLINE kickedSecondaryIndex #-}
kickedSecondaryIndex fp numBuckets (IB alt) =
    secondaryIndex fp numBuckets (IA alt)
