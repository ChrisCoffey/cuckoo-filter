# cuckoo-filter
Cuckoo filters are a probabilistic data structure used to answer questions like "Have I already seen this user" or "Is this word in the English language?". They're _probabilistic_ because each membership operation has a false positive probability. It guarnatees that there will never be a false negative, but may have a low chance of false positives.

Bloom filters are the cannonical probabilistic filter structure, and cuckoo filters are a simlar but different tool. As a bloom filter's load factor increases, the chance of false positive trends towards 100%, but the inserts will never fail. On the other hand, a Cuckoo filter retains a relatively stable false positive probability under load, but as load approahes 95% inserts will begin to fail. In either case you probably want to resize your filter...

This implementation has the following properties:
- Buckets of 4 elements
- 8 bit fingerprints
- Size may be any non-zero natural number (not limited to powers of 2)

For more details about how Cuckoo filters work, I recommend you read Fan et. al.'s 2016 paper https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf.

### TODO
- [] Benchmark against Bloom filter implementation
- [] Profile for memory usage
- [] Provide criterion performance graphs
