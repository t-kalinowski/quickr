suppressPackageStartupMessages(library(pkgload))

pkg_path <- commandArgs(trailingOnly = TRUE)[1L]
pkgload::load_all(pkg_path, quiet = TRUE)

iter_parallel <- quick(function(x, n, iters) {
  declare(
    type(x = double(n)),
    type(n = integer(1)),
    type(iters = integer(1)),
    type(out = double(n))
  )
  out <- double(n)
  declare(parallel())
  for (i in seq_len(n)) {
    v <- x[i]
    for (k in seq_len(iters)) {
      v <- v * 1.000001 + sin(v)
    }
    out[i] <- v
  }
  out
})

n <- 2000000L
iters <- 100L
set.seed(1)
x <- runif(n)

invisible(iter_parallel(x, n, iters))

timing <- system.time(iter_parallel(x, n, iters))
cpu_fields <- intersect(
  names(timing),
  c("user.self", "sys.self", "user.child", "sys.child", "user", "system")
)
cpu <- sum(timing[cpu_fields])

cat(sprintf("elapsed=%.6f cpu=%.6f\n", timing[["elapsed"]], cpu))
