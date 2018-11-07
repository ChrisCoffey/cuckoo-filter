{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Data.CuckooFilter.Pure
Copyright   : (c) Chris Coffey, 2018
License     : MIT
Maintainer  : chris@foldl.io
Stability   : experimental

A strict IntMap-based implementation of a cuckoo filter. This is still quite efficient in terms of
throughput, but the IntMap's internal structures baloon the memory usage, making it far less
practical for real usecases.
-}

module Data.CuckooFilter.Pure (
    Filter(..),
    empty,
) where

import Data.CuckooFilter.Internal (Bucket, Size(..))

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.IntMap.Strict as IM
import Data.Serialize (Serialize)
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | A Cuckoo Filter with a fixed size. The current implementation uses 8 bit fingerprints
-- and 4 element buckets.
data Filter a = F {
    buckets :: IM.IntMap Bucket, -- size / 4.
    numBuckets :: !Natural, -- Track the number of buckets to avoid a length lookup
    size :: !Size -- The number of buckets
    }
    deriving (Show, Eq, Generic, Serialize, ToJSON, FromJSON)

instance Monad m => CuckooFilter Filter m where
    initialize (Size s) = pure $
        -- By using an empty map, we're able to avoid allocating any memory for elements that aren't stored.
        -- If the filter is packed densely the additional memory for the IntMap hurts quite a bit, but at load
        -- factors
        F {
            buckets = IM.empty,
            numBuckets = numBuckets,
            size = Size s
            }
        where
            numBuckets = s `div` 4

    writeBucket index Bucket filt = pure $

