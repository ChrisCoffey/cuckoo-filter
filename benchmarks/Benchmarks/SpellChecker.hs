-- This code is mostly borrowed directly from Bryan O'Sullivan's bloom filter library. It is used
-- as a parity check between the two libraries.
module Benchmarks.SpellChecker (runSpellCheck ) where

import Control.Monad (mapM_, filterM, foldM)
import qualified Data.CuckooFilter as CF
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock (diffUTCTime, getCurrentTime)

runSpellCheck = do
    let dict = "/usr/share/dict/words"
    a <- getCurrentTime
    words <- B.lines `fmap` B.readFile dict
    putStrLn $ {-# SCC "words/length" #-} show (length words) ++ " words"
    b <- getCurrentTime
    putStrLn $ show (diffUTCTime b a) ++ "s to count words"
    let Just s = CF.makeSize 500000
    filt <- {-# SCC "construct" #-} CF.initialize s :: IO (CF.MFilter B.ByteString)
    Just filt' <- foldM ins (Just filt) words
    c <- getCurrentTime
    putStrLn $ show (diffUTCTime c b) ++ "s to construct filter"
    missing <- filterM (fmap not . (`CF.member` filt')) words
    {-# SCC "query" #-} print $ length missing
    d <- getCurrentTime
    putStrLn $ show (diffUTCTime d c) ++ "s to query every element"

    where
        ins filt@(Just f) a = do
            res <- CF.insert f a
            case res of
                Just f' -> pure res
                Nothing -> pure filt
