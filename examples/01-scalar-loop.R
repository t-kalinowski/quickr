# Run from the repository root: Rscript examples/01-scalar-loop.R
library(quickr)

sum_to <- function(n, total = 0) {
  declare(type(n = integer(1)), type(total = double(1)))
  for (i in seq_len(n)) {
    total <- total + i
  }
  total
}

# declare() leaves sum_to() as an ordinary R function. quick() returns the
# compiled function; keep both so they can be compared on the same inputs.
compilation <- system.time(compiled_sum_to <- quick(sum_to))
print(compilation)

stopifnot(identical(compiled_sum_to(0L), sum_to(0L)))
n <- 1000000L
expected <- sum_to(n)
stopifnot(identical(compiled_sum_to(n), expected))
print(expected)

# Compilation and the first call have already happened. Only repeated calls
# on the same input are timed here.
if (requireNamespace("bench", quietly = TRUE)) {
  print(bench::mark(
    R = sum_to(n),
    compiled = compiled_sum_to(n),
    min_time = 0.2
  ))
}
