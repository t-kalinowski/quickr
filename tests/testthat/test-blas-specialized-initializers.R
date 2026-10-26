test_that("specialized initializers retain allocation before BLAS reuse", {
  initializers <- alist(
    f(),
    sapply(seq_len(ncol(a)), function(j) a[, j] + 0)
  )
  replacements <- alist(
    crossprod(a),
    tcrossprod(a),
    outer(a[, 1L], a[, 1L]),
    a %*% a
  )
  for (initializer in initializers) {
    for (replacement in replacements) {
      fn <- eval(bquote(function(a) {
        declare(type(a = double(n, n)))
        f <- function() a + 0
        out <- .(initializer)
        before <- sum(out)
        out <- .(replacement)
        before + sum(out)
      }))
      expect_quick_equal(
        fn,
        list(matrix(as.double(1:4), 2, 2)),
        list(matrix(as.double(1:9), 3, 3))
      )
    }
  }
})
