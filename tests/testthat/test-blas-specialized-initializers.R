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

test_that("later initializers retain allocation after conditional BLAS writes", {
  producers <- alist(
    crossprod(a),
    tcrossprod(a),
    outer(a[, 1L], a[, 1L]),
    a %*% a
  )
  initializers <- alist(
    matrix(1, nrow(a), ncol(a)),
    diag(1, nrow(a), ncol(a)),
    f(),
    sapply(seq_len(ncol(a)), function(j) a[, j] + 1)
  )
  for (producer in producers) {
    for (initializer in initializers) {
      fn <- eval(bquote(function(a, flag) {
        declare(type(a = double(n, n)), type(flag = logical(1)))
        f <- function() a + 1
        if (flag) {
          out <- .(producer)
        }
        out <- .(initializer)
        sum(out)
      }))
      expect_quick_equal(
        fn,
        list(matrix(as.double(1:4), 2, 2), FALSE),
        list(matrix(as.double(1:4), 2, 2), TRUE),
        list(matrix(as.double(1:9), 3, 3), FALSE),
        list(matrix(as.double(1:9), 3, 3), TRUE)
      )
    }
  }
})

test_that("diagonal extraction retains allocation after a conditional solve", {
  fn <- function(a, b, flag) {
    declare(type(a = double(n, n)), type(b = double(n)))
    declare(type(flag = logical(1)))
    if (flag) {
      out <- solve(a, b)
    }
    out <- diag(a)
    sum(out)
  }
  expect_quick_equal(
    fn,
    list(diag(as.double(1:2)), as.double(1:2), FALSE),
    list(diag(as.double(1:2)), as.double(1:2), TRUE),
    list(diag(as.double(1:3)), as.double(1:3), FALSE),
    list(diag(as.double(1:3)), as.double(1:3), TRUE)
  )
})
