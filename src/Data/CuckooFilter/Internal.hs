{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.CuckooFilter.Internal (
    -- * Consting a CuckooFilter
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
import qualified Data.IntMap.Strict as IM
import Data.Hashable (Hashable, hash)
import Data.Serialize (Serialize)
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

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

makeFingerprint :: Hashable a =>
    a
    -> FingerPrint
makeFingerprint a = FP . max 1 $  fromIntegral (natHash a) `mod` 255

primaryIndex :: Hashable a =>
    a
    -> Natural
    -> IndexA
primaryIndex a numBuckets = IA $ (natHash a) `mod` numBuckets

secondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexA
    -> IndexB
secondaryIndex (FP fp) numBuckets (IA primary) =
    IB . (`mod` numBuckets) $ primary `xor` (natHash fp)

kickedSecondaryIndex ::
    FingerPrint
    -> Natural
    -> IndexB
    -> IndexB
kickedSecondaryIndex fp numBuckets (IB alt) =
    secondaryIndex fp numBuckets (IA alt)

natHash :: Hashable a =>
    a ->
    Natural
natHash = fromIntegral . abs . hash
