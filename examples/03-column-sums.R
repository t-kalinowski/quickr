# Run from the repository root: Rscript examples/03-column-sums.R
library(quickr)

column_sums <- function(x) {
  declare(type(x = double(n, m)))
  out <- double(ncol(x))
  for (j in seq_len(ncol(x))) {
    # R and Fortran store columns contiguously, so varying the row index
    # in the inner loop visits adjacent elements.
    for (i in seq_len(nrow(x))) {
      out[j] <- out[j] + x[i, j]
    }
  }
  out
}

x <- matrix(sin(seq_len(60000L)), nrow = 200L)
compiled_column_sums <- quick(column_sums)
expected <- column_sums(x)
stopifnot(isTRUE(all.equal(compiled_column_sums(x), expected)))
stopifnot(isTRUE(all.equal(compiled_column_sums(x), colSums(x))))
print(head(expected))

# colSums() is already implemented in native code. Include it as a reference
# when deciding whether a custom loop is useful for your own calculation.
if (requireNamespace("bench", quietly = TRUE)) {
  print(bench::mark(
    R = column_sums(x),
    compiled = compiled_column_sums(x),
    base = colSums(x),
    min_time = 0.2
  ))
}
