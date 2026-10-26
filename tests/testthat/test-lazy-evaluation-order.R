test_that("selected lazy branches retain side effects before nested errors", {
  expressions <- alist(
    (runif(1) + abs()) + 1,
    (runif(1) + abs()) %*% 1,
    t(runif(1) + abs()) %*% 1,
    sum(runif(1) + abs(), 1),
    prod(runif(1) + abs(), 1),
    min(runif(1) + abs(), 1),
    max(runif(1) + abs(), 1),
    min(runif(1), abs()),
    max(runif(1), abs())
  )
  withr::local_seed(147)
  for (expr in expressions) {
    fn <- eval(bquote(function(flag) {
      declare(type(flag = logical(1)))
      ifelse(flag, .(expr), 0)
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
