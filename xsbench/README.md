# Port of [XSBench](https://github.com/ANL-CESAR/XSBench)

To generate a dataset, first run XSBench itself to generate a data file:

```
$ ./XSBench -s small -m event -b write
```

This produces `XS_data.dat`.  Pass this though the `data2fut` program
to generate a Futhark-readable binary data file:

```
$ ./data2fut < XS_data.dat > futhark.in
```

You may need to modify `data2fut.c`.  I only tested that it could
handle the small dataset.  The small dataset is also the only one that
is stored in the repository, as the big one takes hundreds of MiB.
