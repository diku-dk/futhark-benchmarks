# futhark-benchmarks

Futhark implementations of various benchmarks.  The intention is that
these implementations are of existing published benchmarks, and thus
show where we do well or poorly.  We highly encourage everyone to make
use of it when evaluating Futhark performance.  The following contains
instructions on how to do that.

## Running benchmarks

Some of the benchmarks depend on large datasets that are managed with the
`get-data.sh` tool. To retrieve them, run `./get-data.sh external-data.txt`.

The recommended method for running all benchmarks is

    futhark bench * --backend=opencl --ignore-files /lib/

This uses the `opencl` backend, and ignores the test suites of any
embedded libraries (which are usually too small to be useful
benchmarks, and would merely be noise).  You can replace `opencl` with
another backend.  While the `c` backend is likely too slow to be
practical for many of the benchmarks, the `cuda` backend should
perform fine.

You can also add `--ignore-files /micro/` to ignore the
microbenchmarks of single language constructs, which are often not
very interesting, but do take a long time to run.

If necessary, you can use `--pass-option` to pass more options to the
generated Futhark programs.  This may be necessary if you are using
the OpenCL backend on a system with more than one OpenCL device, and
you wish to use another device than the default.  For example, to ask
Futhark to use the first device that contains "NVIDIA" in its name,
you could use `--pass-option=-dNVIDIA`.

If you wish to analyse the performance results with a program, passing
`--json=results.json` to `futhark bench` may be useful.

For more information, see the [documentation for `futhark bench`][0].

[0]: https://futhark.readthedocs.io/en/latest/man/futhark-bench.html

## Internal details

Many of the input/output files are stored in a simple binary format
for performance and space reasons.  This format is [documented in the
Futhark reference manual][1], and can be converted to a textual
representation via the [futhark dataset][2] tool.

[1]: http://futhark.readthedocs.io/en/latest/binary-data-format.html
[2]: http://futhark.readthedocs.io/en/latest/man/futhark-dataset.html

## Working with large datasets

Some of the larger datasets are not stored directly in the Git
repository.  Instead they are stored on a separate server and fetched
using the self-contained `get-data.sh` script.

* **To add a dataset:** First place the dataset on a publicly
  accessible webserver.  We use [ERDA](https://erda.dk), which is run
  by UCPH.  Then add a line to the `external-data.txt` file of the format:

  ```
  OUTPUT URL SHA256
  ```

  where `OUTPUT` is local path, `URL` is the shared link from ERDA and `SHA256`
  is the sha256 checksum of the file.

  For instance, the line

  ```
  external-data/accelerate/canny/data/lena256.in https://sid.erda.dk/share_redirect/FlhwY8rtfk/accelerate/canny/lena256.in 8c63faf15d9f8028826ed7d9a1647917a833092c1bd291f017b6618584124707
  ```

  Specifies where `lena256.in` should be placed, where it can be downloaded
  from, and what the checksum is.

  We recommend storing large datasets in the `external-data` directory which is
  ignored by our `.gitignore`, and then adding symlinks to the actual place you
  want to use the data. For example:

  ```
  ln -s -r external-data/accelerate/canny/data/lena256.in accelerate/canny/data/lena256.in
  git add accelerate/canny/data/lena256.in
  ```

  There is a script [add-data.sh](add-data.sh) that does some of this automatically. **Read it before running it.**

  Remember to commit your changes.
