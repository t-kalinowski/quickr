# Unit test for ifelse translation

test_that("ifelse", {
  fn <- function(a, b) {
    declare(
      type(a = integer(n)),
      type(b = integer(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_identical(fn, list(-10:10, integer(21)))

  # double version of above
  fn <- function(a, b) {
    declare(
      type(a = double(n)),
      type(b = double(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_equal(fn, list(seq(-5, 5, length.out = 20), double(20)))
})

test_that("ifelse promotes branches and shapes like test", {
  fn <- function(c, a) {
    declare(type(c = logical(n)), type(a = double(n)))
    ifelse(c, 1L, a)
  }
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(c(TRUE, FALSE, TRUE), c(2, 4, 6)))

  # logical branches join as logical
  fn2 <- function(c, a) {
    declare(type(c = logical(n)), type(a = logical(n)))
    ifelse(c, FALSE, a)
  }
  expect_quick_equal(fn2, list(c(TRUE, FALSE, TRUE), c(TRUE, TRUE, FALSE)))
})

test_that("ifelse keeps pure known-shape branches inline", {
  fn <- function(test, a, b, no) {
    declare(
      type(test = logical(3)),
      type(a = double(3)),
      type(b = double(3)),
      type(no = double(3))
    )
    ifelse(test, a + b, no)
  }

  fsub <- r2f(fn)
  expect_false(grepl("block", as.character(fsub), fixed = TRUE))
  expect_quick_identical(
    fn,
    list(
      test = c(TRUE, FALSE, TRUE),
      a = c(1, 2, 3),
      b = c(4, 5, 6),
      no = c(10, 20, 30)
    )
  )
})

test_that("ifelse with scalar test and array branch errors cleanly", {
  fn <- function(c, a) {
    declare(type(c = logical(1)), type(a = double(n)))
    ifelse(c, a, 0)
  }
  expect_error(quick(fn), "shape of `test`")
})

test_that("ifelse defers statically mismatched branch lengths", {
  fn <- function(c, a) {
    declare(type(c = logical(3)), type(a = double(2)))
    ifelse(c, a, 0)
  }

  qfn <- quick(fn)
  expect_equal(qfn(rep(FALSE, 3), c(1, 2)), rep(0, 3))
  expect_error(
    qfn(c(TRUE, FALSE, FALSE), c(1, 2)),
    "R-style recycling is not supported"
  )
})

test_that("ifelse defers branch rank mismatches", {
  # merge() requires conformable arguments: a matrix branch under a vector
  # `test` is R recycling, not broadcasting.
  fn <- function(c, m) {
    declare(type(c = logical(3)), type(m = double(3, 3)))
    ifelse(c, m, 0)
  }
  qfn <- quick(fn)
  m <- matrix(as.double(1:9), 3, 3)
  expect_equal(qfn(rep(FALSE, 3), m), rep(0, 3))
  expect_error(
    qfn(c(TRUE, FALSE, FALSE), m),
    "R-style recycling is not supported"
  )

  # the mirror image, and in `no` position: a vector branch under a matrix test
  fn2 <- function(c, a) {
    declare(type(c = logical(2, 2)), type(a = double(4)))
    ifelse(c, 0, a)
  }
  qfn2 <- quick(fn2)
  a <- as.double(1:4)
  expect_equal(qfn2(matrix(TRUE, 2, 2), a), matrix(0, 2, 2))
  expect_error(
    qfn2(matrix(c(FALSE, TRUE, TRUE, TRUE), 2, 2), a),
    "R-style recycling is not supported"
  )
})

test_that("ifelse guards unknown branch lengths at runtime", {
  fn <- function(c, a, b) {
    declare(type(c = logical(NA)), type(a = double(NA)), type(b = double(NA)))
    ifelse(c, a, b)
  }
  # locks the size guards: a bare merge() with runtime-mismatched
  # assumed-shape vectors read past the shorter branch (returned garbage
  # like 4.65e-310 where R recycles)
  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  cc <- c(TRUE, FALSE, TRUE)
  a <- c(10, 20, 30)
  b <- c(1, 2, 3)
  expect_identical(qfn(cc, a, b), ifelse(cc, a, b))

  expect_error(qfn(cc, c(10, 20), b), "match the shape of `test`")
  expect_error(qfn(cc, a, c(1, 2, 3, 4)), "match the shape of `test`")
})

test_that("ifelse evaluates earlier branches before later shape errors", {
  fn <- function(x) {
    declare(type(x = double(n)))
    ifelse(c(TRUE, FALSE, TRUE), runif(3), runif(3) + x)
  }
  qfn <- quick(fn)

  set.seed(914)
  expect_error(qfn(c(1, 2)), "elementwise vector operations")
  actual_seed <- .Random.seed

  set.seed(914)
  runif(3)
  runif(3)
  expect_identical(actual_seed, .Random.seed)
})

test_that("ifelse does not evaluate unselected branches", {
  fn <- function(test) {
    declare(type(test = logical(3)))
    ifelse(test, runif(3), runif(3))
  }
  qfn <- quick(fn)

  for (test in list(rep(TRUE, 3), rep(FALSE, 3))) {
    set.seed(613)
    expected <- fn(test)
    expected_seed <- .Random.seed

    set.seed(613)
    actual <- qfn(test)
    actual_seed <- .Random.seed

    expect_equal(actual, expected)
    expect_identical(actual_seed, expected_seed)
  }
})

test_that("ifelse defers malformed calls in unselected branches", {
  skipped <- function() {
    ifelse(FALSE, abs(), 1)
  }
  reached <- function() {
    ifelse(TRUE, abs(), 1)
  }

  expect_quick_identical(skipped, list())
  qfn <- quick(reached)
  expect_error(qfn(), "abs")

  reached_with_effects <- function() {
    ifelse(TRUE, runif(1) + abs(), 1)
  }
  qfn <- quick(reached_with_effects)
  set.seed(818)
  expect_error(qfn(), "abs")
  actual_seed <- .Random.seed

  set.seed(818)
  runif(1)
  expect_identical(actual_seed, .Random.seed)
})

test_that("ifelse defers unary mode errors in unselected branches", {
  skipped_yes <- function() {
    ifelse(FALSE, !1, TRUE)
  }
  skipped_no <- function() {
    ifelse(TRUE, TRUE, !1)
  }
  reached_yes <- function() {
    ifelse(TRUE, !1, TRUE)
  }
  reached_no <- function() {
    ifelse(FALSE, TRUE, !1)
  }

  expect_quick_identical(skipped_yes, list())
  expect_quick_identical(skipped_no, list())
  expect_error(quick(reached_yes)(), "expects a logical value", fixed = TRUE)
  expect_error(quick(reached_no)(), "expects a logical value", fixed = TRUE)
})

test_that("ifelse defers unresolved names in unselected branches", {
  skipped_yes <- function() {
    ifelse(FALSE, missing_name, 1)
  }
  skipped_no <- function() {
    ifelse(TRUE, 1, missing_name)
  }
  reached_yes <- function() {
    ifelse(TRUE, missing_name, 1)
  }
  reached_no <- function() {
    ifelse(FALSE, 1, missing_name)
  }

  expect_quick_identical(skipped_yes, list())
  expect_quick_identical(skipped_no, list())
  expect_error(quick(reached_yes)(), "missing_name", fixed = TRUE)
  expect_error(quick(reached_no)(), "missing_name", fixed = TRUE)
})

test_that("ifelse defers unresolved names inside branch expressions", {
  skipped_yes <- function() {
    ifelse(FALSE, missing_name + 1, 1)
  }
  skipped_no <- function() {
    ifelse(TRUE, 1, (missing_name))
  }
  reached_yes <- function() {
    ifelse(TRUE, missing_name + 1, 1)
  }
  reached_no <- function() {
    ifelse(FALSE, 1, (missing_name))
  }

  expect_quick_identical(skipped_yes, list())
  expect_quick_identical(skipped_no, list())
  expect_error(quick(reached_yes)(), "missing_name", fixed = TRUE)
  expect_error(quick(reached_no)(), "missing_name", fixed = TRUE)
})

test_that("valueless ifelse branches are deferred until selected", {
  skipped_yes <- function() {
    ifelse(FALSE, NULL, 1L)
  }
  reached_yes <- function() {
    ifelse(TRUE, NULL, 1L)
  }
  skipped_no <- function() {
    ifelse(TRUE, 1L, NULL)
  }
  reached_no <- function() {
    ifelse(FALSE, 1L, NULL)
  }

  expect_quick_identical(skipped_yes, list())
  expect_error(
    quick(reached_yes)(),
    "ifelse() branches must produce a value",
    fixed = TRUE
  )
  expect_quick_identical(skipped_no, list())
  expect_error(
    quick(reached_no)(),
    "ifelse() branches must produce a value",
    fixed = TRUE
  )
})

test_that("ifelse defers constructor diagnostics until branch selection", {
  skipped <- function() {
    ifelse(FALSE, matrix(1, nrow = 1, ncol = 1, byrow = TRUE), 1)
  }
  reached <- function() {
    ifelse(TRUE, matrix(1, nrow = 1, ncol = 1, byrow = TRUE), 1)
  }

  expect_quick_identical(skipped, list())
  expect_error(
    quick(reached)(),
    "matrix(byrow=TRUE) is not supported",
    fixed = TRUE
  )
})

test_that("ifelse defers anonymous local closure diagnostics", {
  skipped <- function() {
    ifelse(FALSE, (function(x) x + 1)(), 1)
  }
  reached <- function() {
    ifelse(TRUE, (function(x) x + 1)(), 1)
  }

  expect_quick_identical(skipped, list())
  qreached <- quick(reached)
  expect_error(qreached(), "missing required argument")
})

test_that("ifelse defers shape errors in unselected branches", {
  fn <- function(test) {
    declare(type(test = logical(3)))
    ifelse(test, logical(2) & logical(3), logical(3))
  }

  expect_quick_identical(fn, list(rep(FALSE, 3)))
  qfn <- quick(fn)
  expect_error(
    qfn(c(TRUE, FALSE, FALSE)),
    "elementwise vector operations",
    fixed = TRUE
  )

  direct <- function(test) {
    declare(type(test = logical(3)))
    ifelse(test, logical(2), FALSE)
  }
  expect_quick_identical(direct, list(rep(FALSE, 3)))
  qdirect <- quick(direct)
  expect_error(
    qdirect(c(TRUE, FALSE, FALSE)),
    "match the shape of `test`",
    fixed = TRUE
  )
})

test_that("ifelse defers mode errors in unselected branches", {
  skipped <- function() {
    ifelse(FALSE, 1L & 1L, FALSE)
  }
  reached <- function() {
    ifelse(TRUE, 1L & 1L, FALSE)
  }

  expect_quick_identical(skipped, list())
  expect_error(quick(reached)(), "requires logical operands")
})

test_that("ifelse temporaries do not bind later source names", {
  fn <- function(mask, x) {
    declare(type(mask = logical(1)), type(x = integer(2)))
    y <- ifelse(mask, 1.5, 2.5)
    for (Tmp1. in x) {}
    Tmp1.
  }

  expect_quick_identical(fn, list(FALSE, c(1L, 2L)))
})

test_that("ifelse allocates impure branch temporaries only when selected", {
  fn <- function(test) {
    declare(type(test = logical(NA)))
    ifelse(test, runif(length(test)), 0)
  }

  code <- as.character(r2f(fn))
  branch <- regexpr("if (any(", code, fixed = TRUE)
  allocation <- regexpr("allocate(btmp", code, fixed = TRUE)
  expect_gt(branch, 0)
  expect_gt(allocation, branch)

  qfn <- quick(fn)
  for (test in list(rep(FALSE, 32), rep(TRUE, 32))) {
    set.seed(729)
    expected <- fn(test)
    expected_seed <- .Random.seed

    set.seed(729)
    actual <- qfn(test)
    actual_seed <- .Random.seed

    expect_equal(actual, expected)
    expect_identical(actual_seed, expected_seed)
  }
})

test_that("ifelse point-allocates named impure branch temporaries", {
  fn <- function(test, x) {
    declare(type(test = logical(k, k)), type(x = double(n, k)))
    ifelse(test, crossprod(x), 0)
  }

  code <- strsplit(as.character(r2f(fn)), "\n", fixed = TRUE)[[1L]]
  branch_line <- grep("if (any(", code, fixed = TRUE)[[1L]]
  where_line <- grep("^ *where .* = [[:alnum:]_]+$", code)[[1L]]
  branch_name <- sub(".* = ([[:alnum:]_]+)$", "\\1", code[[where_line]])
  allocation_line <- grep(
    paste0("allocate(", branch_name, "("),
    code,
    fixed = TRUE
  )
  expect_length(allocation_line, 1L)
  expect_lt(branch_line, allocation_line)

  x <- matrix(as.double(1:6), 3, 2)
  qfn <- quick(fn)
  for (test in list(matrix(FALSE, 2, 2), matrix(TRUE, 2, 2))) {
    expect_equal(qfn(test, x), fn(test, x))
  }
})

test_that("ifelse allocates results before selector guards", {
  fn <- function(test, yes) {
    declare(type(test = logical(NA)), type(yes = double(NA)))
    ifelse(test, yes, 0)
  }

  fsub <- as.character(r2f(fn))
  guard <- regexpr("ifelse() `yes` and `no`", fsub, fixed = TRUE)[[1L]]
  allocations <- gregexpr("allocate(", fsub, fixed = TRUE)[[1L]]
  result_allocation <- tail(allocations[allocations > 0L], 1L)
  expect_lt(result_allocation, guard)

  qfn <- quick(fn)
  expect_error(
    qfn(c(TRUE, FALSE, TRUE), c(1, 2)),
    "must be scalars or match the shape",
    fixed = TRUE
  )
})

test_that("ifelse accepts matching empty inputs", {
  static <- function() {
    ifelse(logical(), numeric(), numeric())
  }
  dynamic <- function(test, yes, no) {
    declare(
      type(test = logical(NA)),
      type(yes = double(NA)),
      type(no = double(NA))
    )
    ifelse(test, yes, no)
  }

  expect_no_error(r2f(static))
  qdynamic <- quick(dynamic)
  expect_identical(qdynamic(logical(), numeric(), numeric()), numeric())

  lazy_dynamic <- function(test) {
    declare(type(test = logical(NA)))
    ifelse(test, floor(1i), runif(length(test)))
  }
  qlazy <- quick(lazy_dynamic)
  set.seed(915)
  seed <- .Random.seed
  expect_identical(qlazy(logical()), numeric())
  expect_identical(.Random.seed, seed)
})
