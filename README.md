
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quickr <img src="man/figures/logo.png" align="right" height="138"/>

<!-- ![](https://i.giphy.com/media/v1.Y2lkPTc5MGI3NjExMjBhNWt1Z3Q4ZW56cG00c2hncmtwbGJycm53M3JxYWdscjRkaDJobCZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/12haGO61oFZ28w/giphy.gif){alt="An animated GIF showing two characters in a spaceship cockpit rapidly accelerating into hyperspace, with stars stretching into bright streaks, creating a sensation of rapid acceleration and motion."} -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/t-kalinowski/quickr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/t-kalinowski/quickr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/t-kalinowski/quickr/graph/badge.svg)](https://app.codecov.io/gh/t-kalinowski/quickr)

<!-- badges: end -->

The goal of quickr is to make your R code run quicker.

## Overview

R is an extremely flexible and dynamic language, but that flexibility
and dynamicism can come at the expense of speed. This package lets you
trade back some of that flexibility for some speed, for the context of a
single function.

<!-- Programming language design requires some hard decisions and trade-ofs. -->

<!-- When you want to have it all, you typically end up have two (or more!) languages. -->

<!-- An interpreted, dynamic language full of conveniences, and a staticly-typed, explicit, high-performance language. -->

<!-- This is sometimes called the "Two Language Problem". -->

<!-- Like [Numba](https://numba.pydata.org) in Python, or the [Julia](https://julialang.org) language, quickr is a solution to the [two-language problem](https://juliadatascience.io/julia_accomplish). -->

<!-- Unlike those tools however, quickr does not bundle most of LLVM, keeping dependencies lightweight. -->

<!-- Quickr works by translating the R code to Fortran. -->

<!-- Fortran might seem like a surprising choice, but it has many compelling properties: -->

<!-- -   Superb performance. -->

<!--     As a stalwart of the numerical computing community, Fortran has accrued the benefit of countless person-hours from top-tier computer scientists and compiler engineers. -->

<!--     There is a reason that over 20% of R itself, (and numpy, and ...) are still in Fortran, and it's not merely because of legacy. -->

<!--     And this trend of compiler engineers focusing on Fortran is not stopping. -->

<!-- -   Large overlap with R semantics and syntax for numerical computing. -->

<!--     Fortran and R have very similar syntax for operating on arrays. -->

<!--     Like R, Fortran has builtin-in support for nd-arrays, provides vectorized operators on arrays, convenient array slicing semantics that match many capabilities of R's `[` , 1-based indexing, and a well-populated collection of operators for working on those arrays like `min`, `max`, `any` `all`, etc. -->

<!--     This means that it's relatively straightforward to translate R to Fortran, often just a 1:1 mapping of semantics, with some changes to syntax. -->

<!--     This is also why Fortran has such superb performance, and why it attracts compiler engineers to work on it. -->

<!--     Because the language spec guarantees things like, static shapes for nd-arrays, views of those arrays, etc, it provides many opportunities for compiler engineers to do things like generate SIMD instructions, or automatically parallelize code. -->

<!-- -   Excellent support in R. -->

<!--     One of the original motivations for R was to serve as a front-end for Fortran. -->

<!--     Since its inception, R has supported Fortran extensions, and supported them well. -->

<!--     It also means that any computing environment where R build tools are available, Fortran is supported. -->

<!--     The barrier to entry and thorny questions, that, for example, using Rust in CRAN might raise, is non-existent for Fortran. -->

The main exported function is `quick()`, here is how you use it.

``` r
library(quickr)

convolve <- quick(function(a, b) {
  declare(type(a = double(NA)),
          type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i+j-1] <- ab[i+j-1] + a[i] * b[j]
    }
  }
  ab
})
```

`quick()` returns a quicker R function. How much quicker? Let’s
benchmark it! For reference, we’ll also compare it to a
[pure-C](https://cran.r-project.org/doc/FAQ/R-exts.html#Calling-_002eCall-1)
implementation.

``` r
slow_convolve <- function(a, b) {
  declare(type(a = double(NA)),
          type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i+j-1] <- ab[i+j-1] + a[i] * b[j]
    }
  }
  ab
}

library(quickr)
quick_convolve <- quick(slow_convolve)

convolve_c <- inline::cfunction(
  sig = c(a = "SEXP", b = "SEXP"), body = r"({
    int na, nb, nab;
    double *xa, *xb, *xab;
    SEXP ab;

    a = PROTECT(Rf_coerceVector(a, REALSXP));
    b = PROTECT(Rf_coerceVector(b, REALSXP));
    na = Rf_length(a); nb = Rf_length(b); nab = na + nb - 1;
    ab = PROTECT(Rf_allocVector(REALSXP, nab));
    xa = REAL(a); xb = REAL(b); xab = REAL(ab);
    for(int i = 0; i < nab; i++) xab[i] = 0.0;
    for(int i = 0; i < na; i++)
        for(int j = 0; j < nb; j++)
            xab[i + j] += xa[i] * xb[j];
    UNPROTECT(3);
    return ab;
})")



a <- runif (100000); b <- runif (100)

timings <- bench::mark(
  r = slow_convolve(a, b),
  quickr = quick_convolve(a, b),
  c = convolve_c(a, b),
  min_time = 2
)
timings
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 r             487ms 487.01ms      2.05     847KB     8.21
#> 2 quickr        932µs   1.07ms    931.       782KB    15.8 
#> 3 c             916µs   1.07ms    931.       782KB    15.7
plot(timings) + bench::scale_x_bench_time(base = NULL)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

In the case of `convolve()`, `quick()` returns a function approximately
*200* times quicker, giving similar performance to the C function.

`quick()` can accelerate any R function, with some restrictions:

- Function arguments must have their types and shapes declared using
  `declare()`.
- Only atomic vectors, matrices, and array are currently supported:
  `integer`, `double`, `logical`, and `complex`.
- The return value must be an atomic array (e.g., not a list)
- Named variables must have consistent shapes throughout their
  lifetimes.
- `NA` values are not supported.
- Only a subset of R’s vocabulary is currently supported.

<!-- -->

    #>  [1] -         :         !         !=        (         [         [<-      
    #>  [8] [<<-      {         *         /         &         &&        %/%      
    #> [15] %%        ^         +         <         <-        <<-       <=       
    #> [22] =         ==        >         >=        |         ||        Arg      
    #> [29] Conj      Fortran   Im        Mod       Re        abs       acos     
    #> [36] array     as.double asin      atan      break     c         cat      
    #> [43] cbind     ceiling   character cos       declare   dim       double   
    #> [50] exp       floor     for       if        ifelse    integer   length   
    #> [57] log       log10     logical   matrix    max       min       ncol     
    #> [64] next      nrow      numeric   print     prod      raw       repeat   
    #> [71] runif     seq       seq_along seq_len   sin       sqrt      sum      
    #> [78] tan       which.max which.min while

Many of these restrictions are expected to be relaxed as the project
matures. However, quickr is intended primarily for high-performance
numerical computing, so features like polymorphic dispatch or support
for complex or dynamic types are out of scope.

## `declare(type())` syntax:

The shape and mode of all function arguments must be declared. Local and
return variables may optionally also be declared.

`declare(type())` also has support for declaring size constraints, or
size relationships between variables. Here are some examples of declare
calls:

``` r
declare(type(x = double(NA))) # x is a 1-d double vector of any length
declare(type(x = double(10))) # x is a 1-d double vector of length 10
declare(type(x = double(1)))  # x is a scalar double

declare(type(x = integer(2, 3)))  # x is a 2-d integer matrix with dim (2, 3)
declare(type(x = integer(NA, 3))) # x is a 2-d integer matrix with dim (<any>, 3)

# x is a 4-d logical matrix with dim (<any>, 24, 24, 3)
declare(type(x = logical(NA, 24, 24, 3)))

# x and y are 1-d double vectors of any length
declare(type(x = double(NA)),
        type(y = double(NA)))

# x and y are 1-d double vectors of the same length
declare(
  type(x = double(n)),
  type(y = double(n)),
)

# x and y are 1-d double vectors, where length(y) == length(x) + 2
declare(type(x = double(n)),
        type(y = double(n+2)))
```

## More examples:

### `viterbi`

The Viterbi algorithm is an example of a dynamic programming algorithm
within the family of Hidden Markov Models
(<https://en.wikipedia.org/wiki/Viterbi_algorithm>). Here, `quick()`
makes the `viterbi()` approximately 50 times faster.

``` r
slow_viterbi <- function(observations, states, initial_probs, transition_probs, emission_probs) {
    declare(
      type(observations = integer(num_steps)),
      type(states = integer(num_states)),
      type(initial_probs = double(num_states)),
      type(transition_probs = double(num_states, num_states)),
      type(emission_probs = double(num_states, num_obs)),
    )

    trellis <- matrix(0, nrow = length(states), ncol = length(observations))
    backpointer <- matrix(0L, nrow = length(states), ncol = length(observations))
    trellis[, 1] <- initial_probs * emission_probs[, observations[1]]

    for (step in 2:length(observations)) {
      for (current_state in 1:length(states)) {
        probabilities <- trellis[, step - 1] * transition_probs[, current_state]
        trellis[current_state, step] <- max(probabilities) * emission_probs[current_state, observations[step]]
        backpointer[current_state, step] <- which.max(probabilities)
      }
    }

    path <- integer(length(observations))
    path[length(observations)] <- which.max(trellis[, length(observations)])
    for (step in seq(length(observations) - 1, 1)) {
      path[step] <- backpointer[path[step + 1], step + 1]
    }

    out <- states[path]
    out
}

quick_viterbi <- quick(slow_viterbi)

set.seed(1234)
num_steps <- 16
num_states <- 8
num_obs <- 16

observations <- sample(1:num_obs, num_steps, replace = TRUE)
states <- 1:num_states
initial_probs <- runif (num_states)
initial_probs <- initial_probs / sum(initial_probs)  # normalize to sum to 1
transition_probs <- matrix(runif (num_states * num_states), nrow = num_states)
transition_probs <- transition_probs / rowSums(transition_probs)  # normalize rows
emission_probs <- matrix(runif (num_states * num_obs), nrow = num_states)
emission_probs <- emission_probs / rowSums(emission_probs)  # normalize rows

timings <- bench::mark(
  slow_viterbi = slow_viterbi(observations, states, initial_probs,
                              transition_probs, emission_probs),
  quick_viterbi = quick_viterbi(observations, states, initial_probs,
                                transition_probs, emission_probs)
)
timings
#> # A tibble: 2 × 6
#>   expression         min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 slow_viterbi   60.56µs  70.44µs    13960.     178KB     37.7
#> 2 quick_viterbi   1.76µs   2.01µs   478162.        0B      0
plot(timings)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### Diffusion simulation

Simulate how heat spreads over time across a 2D grid, using the [finite
difference method](https://en.wikipedia.org/wiki/Finite_difference)
applied to the [Heat
Equation](https://en.wikipedia.org/wiki/Heat_equation).

Here, `quick()` returns a function over 100 times faster.

``` r
diffuse_heat <- function(nx, ny, dx, dy, dt, k, steps) {
  declare(
    type(nx = integer(1)),
    type(ny = integer(1)),
    type(dx = integer(1)),
    type(dy = integer(1)),
    type(dt = double(1)),
    type(k = double(1)),
    type(steps = integer(1))
  )

  # Initialize temperature grid
  temp <- matrix(0, nx, ny)
  temp[nx / 2, ny / 2] <- 100  # Initial heat source in the center

  # Local helper that updates `temp` in-place.
  apply_boundary_conditions <- function() {
    temp[1, ] <<- 0
    temp[nx, ] <<- 0
    temp[, 1] <<- 0
    temp[, ny] <<- 0
    NULL
  }

  update_temperature <- function(temp, k, dx, dy, dt) {
    temp_new <- temp
    for (i in 2:(nx - 1)) {
      for (j in 2:(ny - 1)) {
        temp_new[i, j] <- temp[i, j] + k * dt *
          ((temp[i + 1, j] - 2 * temp[i, j] + temp[i - 1, j]) / dx ^ 2 +
             (temp[i, j + 1] - 2 * temp[i, j] + temp[i, j - 1]) / dy ^ 2)
      }
    }
    temp_new
  }

  # Time stepping
  for (step in seq_len(steps)) {
    apply_boundary_conditions()
    temp <- update_temperature(temp, k, dx, dy, dt)
  }

  temp
}

quick_diffuse_heat <- quick(diffuse_heat)

# Parameters
nx <- 100L      # Grid size in x
ny <- 100L      # Grid size in y
dx <- 1L        # Grid spacing
dy <- 1L        # Grid spacing
dt <- 0.01      # Time step
k <- 0.1        # Thermal diffusivity
steps <- 500L   # Number of time steps

timings <- bench::mark(
  diffuse_heat = diffuse_heat(nx, ny, dx, dy, dt, k, steps),
  quick_diffuse_heat = quick_diffuse_heat(nx, ny, dx, dy, dt, k, steps)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
summary(timings, relative = TRUE)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression           min median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
#> 1 diffuse_heat        93.7   87.4       1        515.     1   
#> 2 quick_diffuse_heat   1      1        87.4        1      1.37
plot(timings)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### Rolling Mean

Here is quickr used to calculate a rolling mean. Note that the CRAN
package RcppRoll already provides a highly optimized rolling mean, which
we include in the benchmarks for comparison.

``` r
slow_roll_mean <- function(x, weights, normalize = TRUE) {
  declare(
    type(x = double(NA)),
    type(weights = double(NA)),
    type(normalize = logical(1))
  )
  out <- double(length(x) - length(weights) + 1)
  n <- length(weights)
  if (normalize)
    weights <- weights/sum(weights)*length(weights)

  for(i in seq_along(out)) {
    out[i] <- sum(x[i:(i+n-1)] * weights) / length(weights)
  }
  out
}

quick_roll_mean <- quick(slow_roll_mean)

x <- dnorm(seq(-3, 3, len = 100000))
weights <- dnorm(seq(-1, 1, len = 100))

timings <- bench::mark(
  r = slow_roll_mean(x, weights),
  rcpp = RcppRoll::roll_mean(x, weights = weights),
  quickr = quick_roll_mean(x, weights = weights)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
timings
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 r           67.31ms  81.53ms      9.77  124.31MB    23.5 
#> 2 rcpp         6.23ms   6.33ms    153.      4.44MB     1.98
#> 3 quickr        2.3ms   2.46ms    402.    781.35KB     3.98

timings$expression <- factor(names(timings$expression), rev(names(timings$expression)))
plot(timings) + bench::scale_x_bench_time(base = NULL)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

## Parallelize loops with OpenMP

Use `declare(parallel())` to annotate the next `for` loop or `sapply()`
call for OpenMP parallelization. Parallel loops must be
order-independent: avoid shared-state updates or inter-iteration
dependencies. OpenMP adds overhead, so it can be slower for small
workloads, but substantially faster for larger ones.

Here is a concrete example using `colSums()`. At smaller sizes, the
quickr serial version is fastest (even faster than base `colSums()`). As
sizes grow, the two serial versions converge, and the parallel version
pulls ahead. However, the speedup is not linear with core count (e.g.,
with 12 cores, the speedup is closer to ~6x).

``` r
colSums_quick_parallel <- quick(function(x) {
  declare(type(x = double(NA, NA)))
  declare(parallel())
  sapply(seq_len(nrow(x)), \(r) sum(x[, r]))
})

colSums_quick <- quick(function(x) {
  declare(type(x = double(NA, NA)))
  sapply(seq_len(nrow(x)), \(r) sum(x[, r]))
})

r <- bench::press(
  n = 2^(4:14),
  {
    m <- array(runif(n * n), c(n, n))
    bench::mark(
      parallel = colSums_quick_parallel(m),
      quick = colSums_quick(m),
      base = colSums(m),
    )
  },
  .quiet = TRUE
)

library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

r |>
  mutate(.before = 1,
         desc = attr(expression, "description")) |>
  select(desc, n, median) |>
  ggplot(aes(x = n, y = median, color = desc)) +
  geom_point() + geom_line() +
  scale_x_log10() + bench::scale_y_bench_time()
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

By default, quickr will set `OMP_NUM_THREADS` to
`floor(parallel::detectCores(logical = FALSE) * 0.75)` if it is unset,
so a 16-core machine will use 12 threads. You can override this by
setting `OMP_NUM_THREADS` before compiling a function.

## Using `quickr` in an R package

When called in a package, `quick()` will pre-compile the quick functions
and place them in the `./src` directory. Run `devtools::load_all()` or
`quickr::compile_package()` to ensure that the generated files in
`./src` and `./R` are in sync with each other.

In a package, you must provide a function name to `quick()`. For
example:

``` r
my_fun <- quick(name = "my_fun", function(x) ....)
```

## Installation

You can install quickr from CRAN with:

``` r
install.packages("quickr")
```

You can install the development version of quickr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("t-kalinowski/quickr")
```

You will also need a C and Fortran compiler, preferably the same ones
used to build R itself.

On macOS:

- Make sure xcode tools and gfortran are installed, as described in
  <https://mac.r-project.org/tools/>. In Terminal, run:

  ``` zsh
  sudo xcode-select --install
  # curl -LO https://mac.r-project.org/tools/gfortran-12.2-universal.pkg # R 4.4
  curl -LO https://mac.r-project.org/tools/gfortran-14.2-universal.pkg   # R 4.5
  sudo installer -pkg gfortran-12.2-universal.pkg -target /
  ```

- Optional: install `flang-new` via Homebrew (used by quickr on macOS
  when available):

  ``` zsh
  brew install flang
  ```

On Windows:

- Install the latest version of
  [Rtools](https://cran.r-project.org/bin/windows/Rtools/)

On Linux:

- The “Install Required Dependencies” section
  [here](https://docs.posit.co/resources/install-r-source.html#install-required-dependencies)
  provides detailed instructions for installing R build tools on various
  Linux flavors.
