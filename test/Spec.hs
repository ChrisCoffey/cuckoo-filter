import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Control.Monad (foldM, replicateM)
import Data.Functor.Identity (runIdentity)
import Data.Hashable (Hashable)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Word
import Numeric.Natural (Natural)

import Data.CuckooFilter
import Data.CuckooFilter.Internal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.CuckooFilter" [
    -- testProperty "insert x increases load factor" undefined,
    testProperty "insert x >> delete x is idempotent" $ \s -> runIdentity $ do
        f <- fromMaybe defaultFilter <$> insert defaultFilter s
        f' <- delete f s
        not <$> member s f'

    ,testProperty "inserts into a full filter will fail" $ \s n -> let
        f = insertNTimes (100000 + abs n) s defaultFilter
        in isNothing f

    ,testCase "delete x on empty == empty" $ let
        (Just s) = makeSize 10
        filt = runIdentity $ initialize s :: Filter String
        in runIdentity (delete filt "Foobar") @=? filt

    ,testProperty "Looking up a non existent value is False" $ \s ->
        not (runIdentity $ member s defaultFilter)

    -- the bucket size is hardcoded to 4 based on the recommendations from the paper, hence 8 below
    ,testCase "More than 2b deletes is a noop" $ do
        let Just f' = insertNTimes 8 "Foobar" defaultFilter
            g = deleteNTimes 7 "Foobar" f'
            h = deleteNTimes 8 "Foobar" f'
            i = deleteNTimes 90 "Foobar" f'
        memberIdent "Foobar" f' @=? True
        memberIdent "Foobar" g @=? True
        memberIdent "Foobar" h @=? False
        memberIdent "Foobar" i @=? False

    --
    ,testProperty "insert x >> member x == True" $ \ s -> let
        Just f = runIdentity $ insert defaultFilter s
        in runIdentity $ member s f

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
defaultFilter = runIdentity $ initialize s
    where
        (Just s) = makeSize 100000

insertNTimes :: Hashable a =>
    Int
    -> a
    -> Filter a
    -> Maybe (Filter a)
insertNTimes n a filt = let
    insertIdent x f = runIdentity $ insert f a
    in foldM (const . insertIdent a) filt [1..n]


deleteNTimes :: Hashable a =>
    Int
    -> a
    -> Filter a
    -> Filter a
deleteNTimes n a filt = let
    deleteIdent x f = runIdentity $ delete f x
    in foldl (const . deleteIdent a) filt [1..n]

memberIdent :: Hashable a =>
    a
    -> Filter a
    -> Bool
memberIdent a filt = runIdentity $ member a filt

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
    arbitrary = do
        res <- replicateM 4 $ choose (1,255)
        let [a,b,c,d] = res
            a' = setCell emptyBucket 0 (FP a)
            b' = setCell a' 1 (FP b)
            c' = setCell b' 2 (FP c)
            d' = setCell c' 3 (FP d)
        pure (FB d')

