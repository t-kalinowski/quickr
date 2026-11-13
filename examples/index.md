# Runnable quickr examples

These scripts define an R function, compile it with `quick()`, and compare
the compiled result with the original. Each script can run on its own.

| Script | Shows |
| --- | --- |
| [01-scalar-loop.R](01-scalar-loop.R) | Compiling a loop and timing repeated calls separately from compilation |
| [02-convolution.R](02-convolution.R) | Nested loops, vector inputs, and an output whose length depends on the inputs |
| [03-column-sums.R](03-column-sums.R) | Matrix declarations and visiting rows within each column |

Install quickr and the C and Fortran compilers required by R, then run a
script from the repository root:

```sh
Rscript examples/01-scalar-loop.R
```

When working on a checkout, load the development version first:

```r
devtools::load_all()
source("examples/02-convolution.R")
```

The scripts also run a short benchmark if the optional `bench` package is
installed. They compile before timing and check the results before
benchmarking. Timings depend on the input, compiler, and machine; the scripts
do not assume a particular speedup.

`declare()` supplies information to the compiler. It does not compile a
function by itself. Call `quick()` and use the function it returns:

```r
compiled <- quick(original)
compiled(x)
```

R and Fortran both store matrix columns contiguously. The column-sums example
keeps the row index in the inner loop to visit adjacent elements. quickr
preserves the order of explicitly written loops.
