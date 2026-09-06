test_that("conditional and loop-only bindings cannot escape uninitialized", {
  bodies <- list(
    quote({
      if (flag) {
        out <- 1L
      }
      out
    }),
    quote({
      while (flag) {
        out <- 1L
        flag <- FALSE
      }
      out
    }),
    quote({
      for (i in seq_len(n)) {
        out <- 1L
      }
      out
    }),
    quote({
      for (i in seq_len(n)) {
        n <- n
      }
      i
    }),
    quote({
      if (flag) {
        out <- 1L
      }
      copy <- out
      copy
    }),
    quote({
      if (flag) {
        out <- 1L
      }
      list(flag, out)
    }),
    quote({
      repeat {
        if (flag) {
          break
        }
        out <- 1L
        break
      }
      out
    })
  )
  for (expr in bodies) {
    fn <- function(flag, n) {}
    body(fn) <- as.call(c(
      list(
        quote(`{`),
        quote(declare(type(flag = logical(1)), type(n = integer(1))))
      ),
      as.list(expr)[-1L]
    ))
    expect_error(quick(fn), "may be uninitialized")
  }
})

test_that("initialization on every path preserves compiled results", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      out <- 1L
    } else {
      out <- 2L
    }
    out
  }
  expect_quick_identical(fn, TRUE, FALSE)

  fn <- function(flag, n) {
    declare(type(flag = logical(1)), type(n = integer(1)))
    out <- 0L
    if (flag) {
      out <- 1L
    }
    for (i in seq_len(n)) {
      out <- out + 1L
    }
    out
  }
  expect_quick_identical(fn, list(FALSE, 0L), list(TRUE, 0L), list(FALSE, 3L))
})

test_that("local closure returns require initialization", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    inner <- function() {
      if (flag) {
        out <- 1L
      }
      out
    }
    inner()
  }
  expect_error(quick(fn), "may be uninitialized")
})

test_that("closure captures require initialization where the closure is used", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    inner <- function() x
    inner()
  }
  expect_error(quick(fn), "may be uninitialized")

  fn <- function(flag, n) {
    declare(type(flag = logical(1)), type(n = integer(1)))
    out <- double(n)
    if (flag) {
      x <- 1
    }
    out <- sapply(seq_along(out), function(i) x)
    out
  }
  expect_error(quick(fn), "may be uninitialized")

  fn <- function(flag, n) {
    declare(type(flag = logical(1)), type(n = integer(1)))
    out <- double(n)
    if (flag) {
      x <- 1
    }
    f <- function(i) x
    out <- sapply(seq_along(out), f)
    out
  }
  expect_error(quick(fn), "may be uninitialized")

  # A superassignment target is the host binding, not a closure local.
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    f <- function() {
      x <<- x + 1L
      x
    }
    f()
  }
  expect_error(quick(fn), "may be uninitialized")

  # A parenthesized callee is a supported call form.
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    f <- function() x
    (f)()
  }
  expect_error(quick(fn), "may be uninitialized")

  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    (function() x)()
  }
  expect_error(quick(fn), "may be uninitialized")
})

test_that("initialized captures keep compiling and returning R's result", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    } else {
      x <- 2L
    }
    inner <- function() x
    inner()
  }
  expect_quick_identical(fn, TRUE, FALSE)

  fn <- function(flag, n) {
    declare(type(flag = logical(1)), type(n = integer(1)))
    out <- double(n)
    x <- 0
    if (flag) {
      x <- 1
    }
    out <- sapply(seq_along(out), function(i) x + as.double(i))
    out
  }
  expect_quick_identical(fn, list(TRUE, 3L), list(FALSE, 2L))

  fn <- function(flag) {
    declare(type(flag = logical(1)))
    x <- 0L
    if (flag) {
      x <- 1L
    }
    f <- function() {
      x <<- x + 1L
      x
    }
    f()
  }
  expect_quick_identical(fn, TRUE, FALSE)

  fn <- function(flag) {
    declare(type(flag = logical(1)))
    x <- 5L
    f <- function() x
    (f)()
  }
  expect_quick_identical(fn, TRUE)
})

test_that("nested closure bindings do not hide enclosing captures", {
  bodies <- list(
    quote({
      g <- function(x) x
      x
    }),
    quote({
      g <- function() {
        x <- 2L
        x
      }
      x
    }),
    quote({
      g <- function() {
        for (x in seq_len(2L)) {}
        0L
      }
      x
    }),
    quote({
      g <- function() x
      g()
    })
  )
  for (expr in bodies) {
    fn <- function(flag) {
      declare(type(flag = logical(1)))
      if (flag) {
        x <- 1L
      }
      f <- function() {}
      f()
    }
    body(fn)[[4L]][[3L]][[3L]] <- expr
    expect_error(quick(fn), "local variable `x` may be uninitialized")

    # The same lexical scopes remain legal with initialization in both arms.
    body(fn)[[3L]] <- quote(if (flag) x <- 1L else x <- 2L)
    expect_quick_identical(fn, TRUE, FALSE)
  }
})

test_that("closure requirements propagate through calls and function arguments", {
  uses <- list(
    quote({
      g()
    }),
    quote({
      (g)()
    }),
    quote({
      (function() g())()
    }),
    quote({
      out <- integer(2L)
      out <- sapply(seq_along(out), function(i) g())
      out
    })
  )
  for (use in uses) {
    fn <- function(flag) {
      declare(type(flag = logical(1)))
      if (flag) {
        x <- 1L
      }
      f <- function() x
      g <- function() f()
      0L
    }
    body(fn) <- as.call(c(as.list(body(fn))[1:5], as.list(use)[-1L]))
    expect_error(quick(fn), "local variable `x` may be uninitialized")
    body(fn)[[3L]] <- quote(if (flag) x <- 1L else x <- 2L)
    expect_quick_identical(fn, TRUE, FALSE)
  }
})

test_that("nested shadowing does not require an unrelated host binding", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    f <- function() {
      g <- function(x) x + 1L
      g(2L)
    }
    f()
  }
  expect_quick_identical(fn, TRUE, FALSE)

  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    f <- function() {
      g <- function() {
        x <- 2L
        x + 1L
      }
      g()
    }
    f()
  }
  expect_quick_identical(fn, TRUE, FALSE)
})

test_that("cyclic closure dependencies still check captures", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    f <- function() {
      # This creates a capture dependency on f without executing recursion.
      g <- function() f()
      x
    }
    f()
  }
  expect_error(quick(fn), "local variable `x` may be uninitialized")
  body(fn)[[3L]] <- quote(if (flag) x <- 1L else x <- 2L)
  expect_quick_identical(fn, TRUE, FALSE)
})

test_that("omitted closure defaults require initialized captures", {
  uses <- list(
    quote(f()),
    quote((f)()),
    quote(f(a = )),
    quote((function(a = x) a)()),
    quote(g())
  )
  for (use in uses) {
    fn <- function(flag) {
      declare(type(flag = logical(1)))
      if (flag) {
        x <- 1L
      }
      f <- function(a = x) a
      g <- function() f()
      0L
    }
    body(fn)[[6L]] <- use
    expect_error(quick(fn), "local variable `x` may be uninitialized")
    body(fn)[[3L]] <- quote(if (flag) x <- 1L else x <- 2L)
    expect_quick_identical(fn, TRUE, FALSE)
  }
})

test_that("supplied closure arguments do not read unused defaults", {
  uses <- list(
    quote(f(2L)),
    quote(f(a = 2L)),
    quote((f)(2L)),
    quote((function(a = x) a)(2L))
  )
  for (use in uses) {
    fn <- function(flag) {
      declare(type(flag = logical(1)))
      if (flag) {
        x <- 1L
      }
      f <- function(a = x) a
      0L
    }
    body(fn)[[5L]] <- use
    expect_quick_identical(fn, TRUE, FALSE)
  }
})

test_that("nested closure defaults preserve their enclosing scope", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    f <- function() {
      g <- function(a = x + 1L) a
      g()
    }
    f()
  }
  expect_error(quick(fn), "local variable `x` may be uninitialized")
  body(fn)[[3L]] <- quote(if (flag) x <- 1L else x <- 2L)
  expect_quick_identical(fn, TRUE, FALSE)

  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    f <- function(x) {
      g <- function(a = x + 1L) a
      g()
    }
    f(2L)
  }
  expect_quick_identical(fn, TRUE, FALSE)
})

test_that("closure definitions cannot depend on runtime control flow", {
  branches <- list(
    quote(
      if (flag) {
        z <- 0L
        f <- function() 2L
      }
    ),
    quote(
      if (flag) {
        f <- function() 2L
      } else {
        f <- function() 3L
      }
    ),
    quote(
      while (flag) {
        f <- function() 2L
        flag <- FALSE
      }
    ),
    quote(
      for (i in seq_len(n)) {
        f <- function() 2L
      }
    ),
    quote(
      repeat {
        f <- function() 2L
        break
      }
    ),
    quote(
      if (flag) {
        # fmt: skip
        f = function() 2L
      }
    )
  )
  for (branch in branches) {
    fn <- function(flag, n) {
      declare(type(flag = logical(1)), type(n = integer(1)))
      f <- function() 1L
      NULL
      f()
    }
    body(fn)[[4L]] <- branch
    expect_error(quick(fn), "local closure definitions must be outside")
  }

  fn <- function(flag) {
    declare(type(flag = logical(1)))
    f <- function() {
      g <- function() 1L
      if (flag) {
        z <- 0L
        g <- function() 2L
      }
      g()
    }
    f()
  }
  expect_error(quick(fn), "local closure definitions must be outside")
})

test_that("static closures can be called conditionally and in loops", {
  fn <- function(flag, n) {
    declare(type(flag = logical(1)), type(n = integer(1)))
    f <- function() {
      g <- function() 2L
      out <- 1L
      if (flag) {
        out <- g()
      }
      out
    }
    out <- 0L
    if (flag) {
      out <- f()
    }
    for (i in seq_len(n)) {
      out <- out + f()
    }
    out
  }
  expect_quick_identical(
    fn,
    list(FALSE, 0L),
    list(TRUE, 0L),
    list(FALSE, 2L),
    list(TRUE, 2L)
  )

  fn <- function(flag) {
    declare(type(flag = logical(1)))
    out <- 0L
    if (flag) {
      out <- (function() {
        g <- function() 2L
        g()
      })()
    }
    out
  }
  expect_quick_identical(fn, TRUE, FALSE)
})

test_that("local function names have a single binding per scope", {
  bodies <- list(
    quote({
      f <- function() 1L
      f <- function() 2L
      f()
    }),
    quote({
      f <- function() 1L
      # fmt: skip
      f = function() 2L
      f()
    }),
    quote({
      f <- function() 1L
      f <- 2L
      f
    }),
    quote({
      f <- 1L
      f <- function() 2L
      f()
    }),
    quote({
      f <- function() 1L
      for (f in seq_len(2L)) {}
      0L
    }),
    quote({
      g <- function() {
        f <- function() 1L
        f <- function() 2L
        f()
      }
      g()
    })
  )
  for (expr in bodies) {
    fn <- function() {}
    body(fn) <- expr
    expect_error(quick(fn), "local closure `f` cannot be redefined")
  }

  fn <- function(f) {
    declare(type(f = integer(1)))
    f <- function() 1L
    f()
  }
  expect_error(quick(fn), "local closure `f` cannot be redefined")
})

test_that("separate scopes may use the same local function name", {
  fn <- function() {
    f <- function() 1L
    g <- function() {
      f <- function() 2L
      f()
    }
    f() + g()
  }
  expect_quick_identical(fn, list())

  fn <- function(n) {
    declare(type(n = integer(1)))
    f <- function(i) i
    g <- function() {
      f <- function(i) i + 1L
      out <- integer(n)
      out <- sapply(seq_along(out), f)
      sum(out)
    }
    g() + f(1L)
  }
  expect_quick_identical(fn, 2L, 3L)
})

test_that("static closure registries still check initialization at use points", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    f <- function() g()
    g <- function() x
    if (flag) {
      x <- 1L
    }
    f()
  }
  expect_error(quick(fn), "local variable `x` may be uninitialized")
  body(fn)[[5L]] <- quote(if (flag) x <- 1L else x <- 2L)
  expect_quick_identical(fn, TRUE, FALSE)
})

test_that("closure defaults resolve matched formal arguments", {
  fn <- function() {
    a <- 2L
    f <- function(a, b = a) b
    f(1L)
  }
  expect_quick_identical(fn, list())

  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      a <- 9L
    }
    f <- function(b = a + 1L, a = 2L, c = b + a) c
    f()
  }
  expect_quick_identical(fn, TRUE, FALSE)

  fn <- function() {
    a <- 7L
    f <- function(a, b = a + 1L) b
    f(b = , a = a + 2L)
  }
  expect_quick_identical(fn, list())

  fn <- function() {
    f <- function(a = b, b = a) a
    f()
  }
  expect_error(quick(fn), "recursive local closure default")
})

test_that("defaults affected by body assignments are refused", {
  fn <- function() {
    a <- 2L
    f <- function(a, b = a) {
      a <- 3L
      b
    }
    f(1L)
  }
  expect_error(
    quick(fn),
    "defaults cannot depend on bindings assigned in the body"
  )
  body(fn)[[4L]] <- quote(f(1L, 4L))
  expect_quick_identical(fn, list())
})
