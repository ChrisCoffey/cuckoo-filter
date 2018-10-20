import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.CuckooFilter" [
    testProperty "insert x increases load factor" undefined,
    testProperty "insert x >> delete x is idempotent" undefined,
    testCase "delete x on empty == empty" undefined,
    testProperty "insert x >> lookup x == True" undefined,
    testProperty "n inserts & n-1 deletes results in lookup x == True" undefined
    ]
