# rangeQuery2d

Various implementations of
[rangeQuery2d](https://cmuparlay.github.io/pbbsbench/benchmarks/rangeQuery2d.html).
We reuse the data files for the convex hull benchmark. We store only validation
data for the smallest workloads. It is straightforward to generate for larger
workloads if you need it, e.g:

```
$ futhark script -b naive.fut 'main ($loaddata "data/2DonSphere_10K.in")' > data/2DonSphere_10K.out
```

We follow the same input convention as PBBS, namely that of the `p` input
points, the first `2*(n/3)` are used to construct rectangles, and the latter are
query points. This means we will always have a large number of both rectangles
and queries, and the implementations here may well take advantage of that.
