closure_error_bodies <- alist(
  "nonreturn prefix before an invalid assignment" = {
    draw <- runif(1)
    invalid <- abs()
    later <- runif(2)
    TRUE
  },
  "nested closure assignment" = {
    draw <- runif(1)
    nested <- function() {
      other <- runif(1)
      abs()
    }
    value <- nested()
    later <- runif(2)
    TRUE
  },
  "sapply initializer" = {
    draw <- runif(1)
    result <- sapply(seq_len(2L), function(i) {
      other <- runif(1)
      abs()
    })
    later <- runif(2)
    TRUE
  },
  "final stop after a draw" = {
    draw <- runif(1)
    stop("sentinel")
  }
)

for (i in seq_along(closure_error_bodies)) {
  case <- names(closure_error_bodies)[[i]]
  test_that(paste("lazy closure errors retain effects in", case), {
    fn <- eval(bquote(function(flag) {
      declare(type(flag = logical(1)))
      predicate <- function() .(closure_error_bodies[[i]])
      flag && predicate()
    }))
    diagnostic <- c("abs", "abs", "abs", "sentinel")[[i]]
    qfn <- quick(fn)
    withr::local_seed(153)

    expected <- fn(FALSE)
    expected_seed <- .Random.seed
    set.seed(153)
    expect_identical(qfn(FALSE), expected)
    expect_identical(.Random.seed, expected_seed)

    set.seed(153)
    expect_error(fn(TRUE), diagnostic, fixed = TRUE)
    expected_seed <- .Random.seed
    set.seed(153)
    expect_error(qfn(TRUE), diagnostic, fixed = TRUE)
    expect_identical(.Random.seed, expected_seed)
  })
}

test_that("short-circuit predicates allow void tail-call chains", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    calls <- 0L
    effect <- function(i) {
      calls <<- calls + i
      NULL
    }
    wrapper <- function(i) effect(i)
    predicate <- function() {
      wrapper(1L)
      TRUE
    }
    and <- flag && predicate()
    or <- !flag || predicate()
    list(and = and, or = or, calls = calls)
  }
  expect_quick_identical(fn, list(FALSE), list(TRUE))
})

test_that("root-defined errors stop callers before later effects", {
  bodies <- alist(
    {
      outer <- function(flag) {
        fail(flag)
        NULL
      }
      outer(flag)
      draw <- runif(1)
      draw
    },
    {
      middle <- function(flag) {
        fail(flag)
        NULL
      }
      outer <- function(flag) {
        middle(flag)
        NULL
      }
      outer(flag)
      draw <- runif(1)
      draw
    },
    {
      outer <- function(flag) {
        fail(flag)
        1
      }
      value <- outer(flag) + runif(1)
      value
    },
    {
      outer <- function(flag) {
        fail(flag)
        1
      }
      value <- outer(flag)
      value + runif(1)
    },
    {
      outer <- function(i) {
        fail(flag)
        i
      }
      values <- sapply(seq_len(2L), outer)
      draw <- runif(1)
      draw
    }
  )
  for (body in bodies) {
    fn <- function(flag) {
      declare(type(flag = integer(1)))
      fail <- function(flag) {
        if (flag != 0L) {
          stop("bad")
        }
        NULL
      }
    }
    body(fn) <- as.call(c(as.list(body(fn)), as.list(body)[-1L]))
    qfn <- quick(fn)
    withr::local_seed(168)
    expect_error(fn(1L), "bad", fixed = TRUE)
    expected_seed <- .Random.seed
    set.seed(168)
    expect_error(qfn(1L), "bad", fixed = TRUE)
    expect_identical(.Random.seed, expected_seed)

    set.seed(168)
    expected <- fn(0L)
    expected_seed <- .Random.seed
    set.seed(168)
    expect_identical(qfn(0L), expected)
    expect_identical(.Random.seed, expected_seed)
  }
})

test_that("error-free closure chains need no error buffer", {
  fn <- function(x) {
    declare(type(x = double(1)))
    add_one <- function(value) value + 1
    outer <- function(value) add_one(value)
    outer(x)
  }
  expect_quick_identical(fn, 1, 10)
  expect_false(grepl("quickr_err_msg", as.character(r2f(fn)), fixed = TRUE))
})
