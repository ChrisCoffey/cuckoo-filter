import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Control.Monad (foldM)
import Data.Hashable (Hashable)
import Data.Word
import Numeric.Natural (Natural)

import Data.CuckooFilter
import Data.CuckooFilter.Internal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.CuckooFilter" [
    -- testProperty "insert x increases load factor" undefined,
    testProperty "insert x >> delete x is idempotent" $ \s -> let
        Just f = insert defaultFilter s
        f' = delete f s
        in defaultFilter == f'

    ,testCase "delete x on empty == empty" $ let
        (Just s) = makeSize 10
        in delete (empty s) "Foobar" @=? empty s

    -- the bucket size is hardcoded to 4 based on the recommendations from the paper, hence 8 below
    ,testCase "More than 2b deletes is a noop" $ do
        let Just f' = insertNTimes 9 "Foobar" defaultFilter
            g = deleteNTimes 7 "Foobar" f'
            h = deleteNTimes 8 "Foobar" f'
            i = deleteNTimes 90 "Foobar" f'
        member "Foobar" f' @=? True
        member "Foobar" g @=? True
        member "Foobar" h @=? False
        member "Foobar" i @=? False

    --
    ,testProperty "insert x >> member x == True" $ \ s -> let
        Just f = insert defaultFilter s
        in member s f

    ,testProperty "n inserts & n-1 deletes results in member x == True" $ \(s,n) -> let
        posN = abs n
        posM = max 0 (posN -1)
        f = insertNTimes posN s defaultFilter
        in case f of
            Nothing -> posN > 8
            Just filt -> let
                f' = deleteNTimes posM s filt
                in if n == 0
                    then True
                    else member s f'

    , indexTests
    , bucketTests
    ]

indexTests :: TestTree
indexTests = testGroup "Indices" [
    testProperty "primaryIndex == kickedSecondaryIndex" $ \(s::String) n -> let
        fp = makeFingerprint s
        pi = primaryIndex s n
        IA piv = pi
        si = secondaryIndex fp n pi
        in (IB piv) == kickedSecondaryIndex fp n si


    ]

bucketTests :: TestTree
bucketTests = testGroup "Buckets" [
    testCase "getCell i on an empty bucket == 0" $ do
        getCell emptyBucket 0 @=? emptyFP
        getCell emptyBucket 1 @=? emptyFP
        getCell emptyBucket 2 @=? emptyFP
        getCell emptyBucket 3 @=? emptyFP

    ,testProperty "(`getCell` n) $ setCell b n fp) == fp" $ \(IC n, fp) -> let
        b = setCell emptyBucket n fp
        in getCell b n == fp

    ,testProperty "insertBucket fails on a full bucket" $ \(FB b, fp) ->
        maybe True (const False) $ insertBucket fp b

    ]


--
-- Utilities
--

defaultFilter :: Filter String
defaultFilter = empty s
    where
        (Just s) = makeSize 100

insertNTimes :: Hashable a =>
    Int
    -> a
    -> Filter a
    -> Maybe (Filter a)
insertNTimes n a filt =
    foldM (const . (`insert` a )) filt [1..n]


deleteNTimes :: Hashable a =>
    Int
    -> a
    -> Filter a
    -> Filter a
deleteNTimes n a filt = foldl (const . (`delete` a )) filt [1..n]

--
-- Instances
--
instance Arbitrary FingerPrint where
    arbitrary = FP <$> arbitrary

instance Arbitrary Bucket where
    arbitrary = B <$> arbitrary

newtype IndexCell = IC Natural
    deriving Show
instance Arbitrary IndexCell where
    arbitrary = IC <$> elements [0..3]

instance Arbitrary Natural where
    arbitrary = ((+ 1) . fromIntegral . abs) <$> (arbitrary :: Gen Int)

newtype FullBucket = FB Bucket
    deriving Show
instance Arbitrary FullBucket where
    arbitrary = (FB . B) <$> choose (1677217, maxBound)
