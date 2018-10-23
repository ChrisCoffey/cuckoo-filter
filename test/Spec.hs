import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Hashable (Hashable)
import Data.CuckooFilter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.CuckooFilter" [
    -- testProperty "insert x increases load factor" undefined,
    testProperty "insert x >> delete x is idempotent" $ \s -> let
        f = insert defaultFilter s
        f' = delete f s
        in defaultFilter == f'

    ,testCase "delete x on empty == empty" $ let
        (Just s) = makeSize 10
        in delete (empty s) "Foobar" @=? empty s

    -- the bucket size is hardcoded to 4 based on the recommendations from the paper, hence 8 below
    ,testCase "More than 2b deletes is a noop" $ do
        let f' = insertNTimes 9 "Foobar" defaultFilter
            g = deleteNTimes 7 "Foobar" f'
            h = deleteNTimes 8 "Foobar" f'
            i = deleteNTimes 90 "Foobar" f'
        member "Foobar" f' @=? True
        member "Foobar" g @=? True
        member "Foobar" h @=? False
        member "Foobar" i @=? False

    --
    ,testProperty "insert x >> member x == True" $ \ s -> let
        f = insert defaultFilter s
        in member s f

    ,testProperty "n inserts & n-1 deletes results in member x == True" $ \(s,n) -> let
        posN = abs n
        posM = max 0 (posN -1)
        f = insertNTimes posN s defaultFilter
        f' = deleteNTimes posM s f
        in if n == 0
            then True
            else member s f'
    ]

defaultFilter :: Filter String
defaultFilter = empty s
    where
        (Just s) = makeSize 100

insertNTimes :: Hashable a =>
    Int
    -> a
    -> Filter a
    -> Filter a
insertNTimes n a filt = foldl (const . (`insert` a)) filt [1..n]


deleteNTimes :: Hashable a =>
    Int
    -> a
    -> Filter a
    -> Filter a
deleteNTimes n a filt = foldl (const . (`delete` a )) filt [1..n]
