test_that("raw comparisons preserve unsigned byte values", {
  values <- as.raw(c(0, 1, 127, 128, 255))
  pairs <- expand.grid(x = seq_along(values), y = seq_along(values))
  cases <- lapply(seq_len(nrow(pairs)), function(i) {
    list(values[pairs$x[i]], values[pairs$y[i]])
  })

  for (op in c("<", "<=", ">", ">=", "==", "!=")) {
    scalar <- eval(bquote(function(x, y) {
      declare(type(x = raw(1)), type(y = raw(1)))
      .(call(op, quote(x), quote(y)))
    }))
    vector <- eval(bquote(function(x, y) {
      declare(type(x = raw(n)), type(y = raw(n)))
      .(call(op, quote(x), quote(y)))
    }))

    expect_quick_identical(scalar, !!!cases)
    expect_quick_identical(
      vector,
      list(values[pairs$x], values[pairs$y])
    )
  }
})

test_that("raw and integer comparisons preserve unsigned byte values", {
  values <- as.raw(c(0, 1, 127, 128, 255))
  integers <- c(-1L, 0L, 1L, 127L, 128L, 255L, 256L)
  pairs <- expand.grid(x = seq_along(values), y = seq_along(integers))
  inputs <- list(values[pairs$x], integers[pairs$y])

  for (op in c("<", "<=", ">", ">=", "==", "!=")) {
    left_raw <- eval(bquote(function(x, y) {
      declare(type(x = raw(n)), type(y = integer(n)))
      .(call(op, quote(x), quote(y)))
    }))
    right_raw <- eval(bquote(function(x, y) {
      declare(type(x = raw(n)), type(y = integer(n)))
      .(call(op, quote(y), quote(x)))
    }))

    expect_quick_identical(left_raw, inputs)
    expect_quick_identical(right_raw, inputs)
  }
})

test_that("raw and logical comparisons coerce raw values to logical", {
  values <- as.raw(c(0, 1, 127, 128, 255))
  logicals <- c(FALSE, TRUE)
  pairs <- expand.grid(x = seq_along(values), y = seq_along(logicals))
  inputs <- list(values[pairs$x], logicals[pairs$y])

  for (op in c("<", "<=", ">", ">=", "==", "!=")) {
    left_raw <- eval(bquote(function(x, y) {
      declare(type(x = raw(n)), type(y = logical(n)))
      .(call(op, quote(x), quote(y)))
    }))
    right_raw <- eval(bquote(function(x, y) {
      declare(type(x = raw(n)), type(y = logical(n)))
      .(call(op, quote(y), quote(x)))
    }))

    expect_quick_identical(left_raw, inputs)
    expect_quick_identical(right_raw, inputs)
  }
})
