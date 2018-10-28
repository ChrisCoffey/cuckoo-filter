# cuckoo-filter

[![Build Status](https://travis-ci.org/ChrisCoffey/cuckoo-filter.svg?branch=master)](https://travis-ci.org/ChrisCoffey/cuckoo-filter)

Cuckoo filters are a probabilistic data structure used to answer questions like "Have I already seen this user" or "Is this word in the English language?". They're _probabilistic_ because each membership operation has a false positive probability. It guarnatees that there will never be a false negative, but may have a low chance of false positives.

Bloom filters are the cannonical probabilistic filter structure, and cuckoo filters are a simlar but different tool. As a bloom filter's load factor increases, the chance of false positive trends towards 100%, but the inserts will never fail. On the other hand, a Cuckoo filter retains a relatively stable false positive probability under load, but as load approahes 95% inserts will begin to fail. In either case you probably want to resize your filter...

This implementation has the following properties:
- Buckets of 4 elements
- 8 bit fingerprints
- Cycle termination during item kicking occurs after (0.1 * size) buckets have been checked.
- Size may be any non-zero natural number (not limited to powers of 2)

For more details about how Cuckoo filters work, I recommend you read Fan et. al.'s 2016 paper https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf.

### Usage
Cuckoo filters support three operations: `insert`, `member`, and `delete`. See the [haddocks](https://hackage.haskell.org/package/cuckoo-filter) for details.

### Performance
As you'll find in the criterion results, the pure version of the filter can handle ~1.6 million insertions/s. From memory profiles, the vast majority of the memory is taken up by the underlying implementation of `Filter`, so this is an obvious area for improvement.

The current implementation avoids pre-allocating memory for the filter, so the heap usage will incrase linearly with `insert` calls. This obviously helps keep heap usage low for sparse filters, but also means inserts are slower than they would be in a mutable implementation.

### TODO
- [ ] Benchmark against a Bloom filter implementation
- [ ] Introduce a mutable version of `Filter` and a typeclass for the storage interactions
