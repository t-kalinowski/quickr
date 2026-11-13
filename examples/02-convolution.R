# Run from the repository root: Rscript examples/02-convolution.R
library(quickr)

convolve_vectors <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  out <- double(length(a) + length(b) - 1L)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      out[i + j - 1L] <- out[i + j - 1L] + a[i] * b[j]
    }
  }
  out
}

# This example uses nonempty vectors. The output length is computed from
# their lengths; the declarations do not fix either input's length.
a <- sin(seq_len(2000L))
b <- c(0.1, 0.2, 0.4, 0.2, 0.1)
compiled_convolve <- quick(convolve_vectors)
expected <- convolve_vectors(a, b)
stopifnot(isTRUE(all.equal(compiled_convolve(a, b), expected)))
print(head(expected))

if (requireNamespace("bench", quietly = TRUE)) {
  print(bench::mark(
    R = convolve_vectors(a, b),
    compiled = compiled_convolve(a, b),
    min_time = 0.2
  ))
}
