import Benchmarks.Simple
import Benchmarks.SpellChecker

import Criterion.Main

-- main = stdInMutableBenchmark
-- main = stdInBenchmark
main = runSpellCheck
{-
main = defaultMain [
    tenPctPacked,
    fiftyPctPacked,
    ninetyPctPacked
    ]
-}
