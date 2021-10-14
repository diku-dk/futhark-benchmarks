# futhark-benchmarks

Futhark implementations of various benchmarks.  The intention is that
these implementations are of existing published benchmarks, and thus
show where we do well or poorly.  We highly encourage everyone to make
use of it when evaluating Futhark performance.  The following contains
instructions on how to do that.

## Running benchmarks

Some of the benchmarks depend on large datasets that are managed with
[git-annex](https://git-annex.branchable.com/).  To retrieve them, run
`git-annex sync --content --no-push`.

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
using [git-annex](https://git-annex.branchable.com/), which you must install.

* **To add a dataset:** First place the dataset on a publicly
  accessible webserver.  We use [ERDA](https://erda.dk), which is run
  by UCPH.  Then run `git annex addurl URL`.  For example:

  ```
  git annex addurl https://sid.erda.dk/share_redirect/FlhwY8rtfk/xsbench/small.in.gz
  ```

  You may need `--no-check-gitignore`.  You may need to fix the file
  name with `git mv` in case the webserver does a redirection that
  confuses `git-annex`.
