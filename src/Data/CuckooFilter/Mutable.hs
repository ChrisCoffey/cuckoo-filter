{-|
Module      : Data.CuckooFilter.Mutable
Copyright   : (c) Chris Coffey, 2018
License     : MIT
Maintainer  : chris@foldl.io
Stability   : experimental

An unboxed, mutable array implementation of the cuckoo filter. This is quite space efficient,
using only x% of the memory the pure IntMap implementation uses. Prefer this if you need memory
or cpu performance.
-}

module Data.CuckooFilter.Mutable (
    MFilter
    ) where

import Data.CuckooFilter.Internal (CuckooFilter(..), Bucket(..), Size(..), emptyBucket)

import Control.Monad.ST (stToIO)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Array.IO as IOA
import qualified Data.Array.MArray as A
import Data.Serialize (Serialize)
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- Write it in IO first
-- Given a mutable array, can I actually fix a C array & use that?

data MFilter a = MF {
    buckets :: IOA.IOUArray Int Word32,
    size :: !Size,
    numBuckets :: !Natural
    }
    deriving (Eq)

instance CuckooFilter MFilter IO where
    initialize (Size s) = do
        rawArray <- A.newArray (0::Int, fromIntegral nb) 0
        pure MF {
            buckets = rawArray,
            size = Size s,
            numBuckets = nb
            }
        where
            nb = s `div` 4

    {-# INLINE bucketCount #-}
    bucketCount MF { numBuckets } = pure numBuckets

    {-# INLINE writeBucket #-}
    writeBucket index (B val) filt = do
        A.writeArray (buckets filt) index val
        pure filt

    {-# INLINE readBucket #-}
    readBucket index filt =
        B <$> A.readArray (buckets filt)index


