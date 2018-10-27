import Benchmarks.Simple

import Criterion.Main


main = defaultMain [
    tenPctPacked,
    fiftyPctPacked,
    ninetyPctPacked
    ]
