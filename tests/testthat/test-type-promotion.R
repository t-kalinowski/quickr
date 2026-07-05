# Unit tests for type promotion: result modes follow R's lattice
# (logical < integer < double < complex) and operands are cast wherever
# Fortran requires same-typed arguments.

skip_on_cran()

test_that("integer + double promotes to double", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    x + 0.5
  }
  # R returns doubles; the misreported integer mode used to declare an
  # integer output and silently truncate
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(-2:2))
})

test_that("real-valued RHS with integer operand declares a double binding", {
  fn <- function(x) {
    declare(type(x = double(1)))
    out <- x * 3L
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(0.5))
})

test_that("misreported mode does not defeat subscript coercion", {
  # `runif(1) * 3L` is real-valued but used to report mode integer, so the
  # `[` handler's double-coercion backstop never fired and the raw real
  # expression landed in the subscript
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- x[as.integer(runif(1) * 3) + 1L]
    out
  }
  expect_translation_snapshots(fn)
  qfn <- quick(fn)
  x <- c(10, 20, 30)
  set.seed(42)
  r_res <- fn(x)
  set.seed(42)
  q_res <- qfn(x)
  expect_identical(q_res, r_res)
})

test_that("c() promotes mixed elements", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    c(1L, 2.5, x)
  }
  # mixed-mode array constructors used to be a gfortran type error
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(1:3))

  fn_lgl <- function(x) {
    declare(type(x = logical(1)))
    c(x, 2L)
  }
  expect_quick_equal(fn_lgl, list(TRUE))
})

test_that("multi-arg max()/min() promote", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    max(x, 2.5)
  }
  # mixed-mode max()/min() used to be a gfortran type error
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(1:3), list(5:9))

  fn_min <- function(x) {
    declare(type(x = integer(n)))
    min(x, 2.5)
  }
  expect_quick_equal(fn_min, list(1:3), list(5:9))

  fn_sum <- function(x) {
    declare(type(x = integer(n)))
    sum(x, 0.5)
  }
  expect_quick_equal(fn_sum, list(1:3))
})

test_that("%% and %/% with mixed modes promote", {
  fn <- function(a, b) {
    declare(type(a = integer(n)), type(b = double(n)))
    a %% b
  }
  # modulo() requires same-typed args; this used to be a gfortran type error
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(-5:5, rep(2.5, 11)))

  fn_intdiv <- function(a, b) {
    declare(type(a = integer(n)), type(b = double(n)))
    a %/% b
  }
  expect_quick_equal(fn_intdiv, list(-5:5, rep(2.5, 11)))
})

test_that("^ always returns double; integer exponent stays exact", {
  fn <- function(x) {
    declare(type(x = integer(1)))
    x ^ -1L
  }
  # locks the `** (...)` parenthesization and the double result mode:
  # integer ** used to return integer (R: 0.5, bug: 0) through
  # non-standard `** -1_c_int` syntax
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(2L))

  fn2 <- function(x) {
    declare(type(x = integer(n)))
    x ^ 2L
  }
  expect_quick_equal(fn2, list(-3:3))

  fn3 <- function(x) {
    declare(type(x = double(1)))
    x ^ 2L
  }
  # negative base with whole-number exponent is defined in R (and in
  # Fortran's real ** int, unlike real ** real)
  expect_quick_equal(fn3, list(-2), list(2.5))

  fn4 <- function(x) {
    declare(type(x = double(1)))
    x ^ 2.5
  }
  expect_quick_equal(fn4, list(2))
})

test_that("logical operands participate in arithmetic as integers", {
  fn <- function(a, b) {
    declare(type(a = logical(1)), type(b = logical(1)))
    a + b
  }
  # R: TRUE + TRUE is 2L; Fortran has no logical arithmetic, so this used
  # to be a gfortran type error
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(TRUE, TRUE), list(TRUE, FALSE))

  fn_mixed <- function(a, x) {
    declare(type(a = logical(n)), type(x = double(n)))
    a * x
  }
  expect_quick_equal(fn_mixed, list(c(TRUE, FALSE, TRUE), c(1.5, 2.5, 3.5)))

  fn_neg <- function(a) {
    declare(type(a = logical(1)))
    -a
  }
  # R: -TRUE is -1L
  expect_quick_equal(fn_neg, list(TRUE), list(FALSE))

  fn_pow <- function(a, b) {
    declare(type(a = logical(1)), type(b = logical(1)))
    a ^ b
  }
  # R: TRUE ^ TRUE is 1 (double)
  expect_quick_equal(fn_pow, list(TRUE, TRUE), list(FALSE, TRUE))
})

test_that("comparisons accept logical operands like R", {
  fn <- function(x) {
    declare(type(x = logical(n)))
    x > 0L
  }
  # R compares logicals as integers; `.true. > 0` is not Fortran
  expect_quick_equal(fn, list(c(TRUE, FALSE)))

  fn_eq <- function(x, y) {
    declare(type(x = logical(n)), type(y = logical(n)))
    x == y
  }
  expect_quick_equal(fn_eq, list(c(TRUE, FALSE, TRUE), c(TRUE, TRUE, FALSE)))
})

test_that("reassignment that would narrow the mode is a compile error", {
  fn <- function(x) {
    declare(type(x = integer(1)))
    x <- x + 0.5
    x
  }
  # R promotes x to double; Fortran cannot re-type a variable, and the
  # assignment used to silently truncate (quickr returned 2, R returns 2.5)
  expect_error(quick(fn), "narrow double to integer")

  fn_lgl <- function(x) {
    declare(type(x = logical(1)))
    x <- x + 1L
    x
  }
  expect_error(quick(fn_lgl), "narrow integer to logical")

  # same-mode and widening-safe reassignments still work
  fn_ok <- function(x) {
    declare(type(x = double(1)))
    x <- x + 1L
    x
  }
  expect_quick_equal(fn_ok, list(1.5))
})

test_that("subassignment that would narrow the mode is a compile error", {
  fn <- function(x) {
    declare(type(x = integer(3)))
    x[1L] <- 2.5
    x
  }
  # R promotes the whole vector to double; the emitted element assignment
  # used to silently truncate (quickr returned c(2L, 2L, 3L))
  expect_error(quick(fn), "narrow double to integer")

  fn_range <- function(x) {
    declare(type(x = integer(n)))
    x[1:2] <- x[1:2] / 2
    x
  }
  expect_error(quick(fn_range), "narrow double to integer")

  # widening-safe subassignment still works
  fn_ok <- function(x) {
    declare(type(x = double(3)))
    x[1L] <- 5L
    x
  }
  expect_quick_equal(fn_ok, list(c(1.5, 2.5, 3.5)))
})
