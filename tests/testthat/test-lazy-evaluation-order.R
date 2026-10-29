test_that("selected lazy branches retain side effects before nested errors", {
  expressions <- alist(
    (runif(1) + abs()) + 1,
    (runif(1) + abs()) %*% 1,
    t(runif(1) + abs()) %*% 1,
    sum(runif(1) + abs()),
    prod(runif(1) + abs()),
    min(runif(1) + abs()),
    max(runif(1) + abs())
  )
  withr::local_seed(147)
  for (expr in expressions) {
    fn <- eval(bquote(function(flag) {
      declare(type(flag = logical(1)))
      flag && (.(expr) > 0)
    }))
    qfn <- quick(fn)

    set.seed(147)
    expected <- fn(FALSE)
    expected_seed <- .Random.seed
    set.seed(147)
    expect_identical(qfn(FALSE), expected)
    expect_identical(.Random.seed, expected_seed)

    set.seed(147)
    expect_error(fn(TRUE), "abs")
    expected_seed <- .Random.seed
    set.seed(147)
    expect_error(qfn(TRUE), "abs")
    expect_identical(.Random.seed, expected_seed, info = deparse1(expr))
  }
})

test_that("nested short-circuit errors retain only the evaluated prefix", {
  expressions <- alist(
    flag && ((runif(1) + abs()) + runif(2) > 0),
    !flag || ((runif(1) + abs()) + runif(2) > 0),
    flag && (FALSE || ((runif(1) + abs()) + runif(2) > 0))
  )
  withr::local_seed(148)
  for (expr in expressions) {
    fn <- eval(bquote(function(flag) {
      declare(type(flag = logical(1)))
      .(expr)
    }))
    qfn <- quick(fn)

    set.seed(148)
    expected <- fn(FALSE)
    expected_seed <- .Random.seed
    set.seed(148)
    expect_identical(qfn(FALSE), expected)
    expect_identical(.Random.seed, expected_seed)

    set.seed(148)
    expect_error(fn(TRUE), "abs")
    expected_seed <- .Random.seed
    set.seed(148)
    expect_error(qfn(TRUE), "abs")
    expect_identical(.Random.seed, expected_seed)
  }
})

test_that("lazy closure errors preserve their evaluated body prefix", {
  bodies <- alist(
    {
      draw <- runif(1)
      abs()
    },
    {
      draw <- runif(1)
      invalid <- abs()
      later <- runif(2)
      TRUE
    },
    {
      draw <- runif(1)
      (runif(1) + abs()) + runif(2)
    },
    {
      draw <- runif(1)
      nested <- function() {
        other <- runif(1)
        abs()
      }
      nested()
    }
  )
  branches <- alist(
    flag && predicate(),
    !flag || predicate()
  )
  withr::local_seed(149)
  for (body in bodies) {
    for (branch in branches) {
      fn <- eval(bquote(function(flag) {
        declare(type(flag = logical(1)))
        predicate <- function() .(body)
        .(branch)
      }))
      qfn <- quick(fn)

      set.seed(149)
      expected <- fn(FALSE)
      expected_seed <- .Random.seed
      set.seed(149)
      expect_identical(qfn(FALSE), expected)
      expect_identical(.Random.seed, expected_seed)

      set.seed(149)
      expect_error(fn(TRUE), "abs")
      expected_seed <- .Random.seed
      set.seed(149)
      expect_error(qfn(TRUE))
      expect_identical(.Random.seed, expected_seed, info = deparse1(body))
    }
  }
})

test_that("deferred closure errors stay inside nested control flow", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    predicate <- function() {
      if (flag) {
        draw <- runif(1)
        invalid <- abs()
        later <- runif(2)
      }
      TRUE
    }
    TRUE && predicate()
  }
  qfn <- quick(fn)
  withr::local_seed(150)
  before <- .Random.seed
  expect_identical(qfn(FALSE), fn(FALSE))
  expect_identical(.Random.seed, before)
  set.seed(150)
  expect_error(fn(TRUE), "abs")
  expected_seed <- .Random.seed
  set.seed(150)
  expect_error(qfn(TRUE))
  expect_identical(.Random.seed, expected_seed)
})

test_that("invalid NULL closure results retain preceding effects", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    predicate <- function() {
      draw <- runif(1)
      NULL
    }
    flag && predicate()
  }
  qfn <- quick(fn)
  withr::local_seed(152)
  before <- .Random.seed
  expect_identical(qfn(FALSE), fn(FALSE))
  expect_identical(.Random.seed, before)
  set.seed(152)
  expect_error(fn(TRUE))
  expected_seed <- .Random.seed
  set.seed(152)
  expect_error(qfn(TRUE))
  expect_identical(.Random.seed, expected_seed)
})
