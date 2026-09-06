# Unit tests for logical operations

test_that("between", {
  between <- function(x, left, right) {
    declare({
      type(x = double(n))
      type(left = double(1))
      type(right = double(1))
    })
    out <- x >= left & x <= right
    out
  }

  expect_translation_snapshots(between)
  expect_quick_identical(between, list(x = runif(100), left = .4, right = .6))
})

test_that("logical ops", {
  test_args <- list(
    list(1, 2),
    list(2, 1),
    list(-2, 2),
    list(-2, -2),
    list(3, 3),
    list(4, 1),
    list(1, 4)
  )

  fn <- function(a, b) {
    declare(
      type(a = double(1)),
      type(b = double(1))
    )

    delta <- a - b
    if (delta < 0) {
      delta <- (-1) * delta
    }

    a_gt_b <- a > b
    b_gt_a <- b > a
    delta_lt_3 <- delta <= 3

    out <- (a_gt_b || b_gt_a) && delta_lt_3
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # simpler version of above
  fn <- function(a, b) {
    declare({
      type(a = double(1))
      type(b = double(1))
    })

    delta <- abs(a - b)
    out <- (a != b) & (delta <= 3)
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # even simpler version
  fn <- function(a, b) {
    declare(type(a = double(1)), type(b = double(1)))
    out <- (a != b) && abs(a - b) <= 3
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # vectorized version
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    out <- (a != b) & abs(a - b) <= 3
    out
  }
  expect_translation_snapshots(fn)
  .[a, b] <- .mapply(c, test_args, NULL)
  expect_quick_identical(fn, list(a, b))
})

test_that("parentheses preserve logical precedence", {
  fn_a <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    cond <- (x > 8L || x <= 0L) && (y > 8L || y <= 0L)
    cond
  }

  fn_b <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    cond_x <- x > 8L || x <= 0L
    cond_y <- y > 8L || y <= 0L
    cond_x && cond_y
  }

  cases <- list(
    list(9L, 1L),
    list(9L, 9L),
    list(0L, 9L),
    list(1L, 0L),
    list(5L, 5L)
  )

  expect_translation_snapshots(fn_a)
  expect_translation_snapshots(fn_b)
  expect_quick_identical(fn_a, !!!cases)
  expect_quick_identical(fn_b, !!!cases)
})

test_that("&& and || require length-1 operands, like R", {
  vec_and <- function(x, y) {
    declare(type(x = logical(3)), type(y = logical(3)))
    x && y
  }
  expect_error(quick(vec_and), "length-1 operands")

  numeric_and <- function(a, b) {
    declare(type(a = double(1)), type(b = double(1)))
    a && b
  }
  expect_error(quick(numeric_and), "logical operands")

  vector_matrix_and <- function(x, y) {
    declare(type(x = logical(2)), type(y = logical(2, 2)))
    x && y
  }
  expect_error(quick(vector_matrix_and), "requires length-1 operands")

  matrix_vector_or <- function(x, y) {
    declare(type(x = logical(2, 2)), type(y = logical(2)))
    x || y
  }
  expect_error(quick(matrix_vector_or), "requires length-1 operands")
})

test_that("&& and || guard unknown operand lengths at runtime", {
  and_fn <- function(x, y) {
    declare(type(x = logical(NA)), type(y = logical(1)))
    x && y
  }
  qand <- quick(and_fn)
  expect_identical(qand(TRUE, FALSE), FALSE)
  expect_error(qand(c(TRUE, FALSE), TRUE), "requires length-1 operands")

  or_fn <- function(x, y) {
    declare(type(x = logical(1)), type(y = logical(NA)))
    x || y
  }
  qor <- quick(or_fn)
  expect_identical(qor(FALSE, TRUE), TRUE)
  expect_identical(qor(TRUE, c(FALSE, TRUE)), TRUE)
  expect_error(qor(FALSE, c(FALSE, TRUE)), "requires length-1 operands")
})

test_that("short-circuited right operands defer diagnostics", {
  expressions <- list(
    quote(FALSE && logical(2)),
    quote(TRUE || logical(2)),
    quote(FALSE && (1L & 1L)),
    quote(TRUE || (1L & 1L)),
    quote(FALSE && 1L),
    quote(TRUE || 1L),
    quote(FALSE && ((1L & 1L) == FALSE)),
    quote(TRUE && logical(2)),
    quote(FALSE || logical(2)),
    quote(TRUE && (1L & 1L)),
    quote(FALSE || (1L & 1L)),
    quote(TRUE && 1L),
    quote(FALSE || 1L),
    quote(TRUE && ((1L & 1L) == FALSE))
  )
  errors <- c(
    rep("", 7L),
    rep("requires length-1 operands", 2L),
    rep("requires logical operands", 5L)
  )
  for (i in seq_along(expressions)) {
    fn <- function() NULL
    body(fn) <- call("{", expressions[[i]])
    if (!nzchar(errors[[i]])) {
      expect_quick_identical(fn, list())
    } else {
      expect_error(quick(fn)(), errors[[i]])
    }
  }
})

test_that("short-circuiting does not read absent optional arguments", {
  fn <- function(fallback) {
    declare(type(fallback = double(1)))
    f <- function(x = NULL) {
      out <- is.null(x) || x > 0
      if (is.null(x)) {
        x <- fallback
      }
      out
    }
    c(f(0), f(), f(1))
  }

  expect_quick_identical(fn, list(0))
})

test_that("short-circuited nested operands defer length errors", {
  skipped_and <- function() {
    FALSE && (logical(2) && TRUE)
  }
  skipped_or <- function() {
    TRUE || (logical(2) || FALSE)
  }
  reached_and <- function() {
    TRUE && (logical(2) && TRUE)
  }

  expect_quick_identical(skipped_and, list())
  expect_quick_identical(skipped_or, list())
  expect_error(quick(reached_and)(), "requires length-1 operands")
})

test_that("short-circuited unresolved names remain deferred", {
  expressions <- list(
    quote(FALSE && missing_name),
    quote(TRUE || ((missing_name) + 1 > 0)),
    quote(FALSE || missing_name),
    quote(TRUE && (missing_name + 1 > 0))
  )
  for (i in seq_along(expressions)) {
    fn <- function() NULL
    body(fn) <- call("{", expressions[[i]])
    if (i <= 2L) {
      expect_quick_identical(fn, list())
    } else {
      expect_error(quick(fn)(), "missing_name", fixed = TRUE)
    }
  }
})
test_that("short-circuited division is not evaluated eagerly", {
  skipped_and <- function(x) {
    declare(type(x = double(1)))
    FALSE && (1 / x > 0)
  }
  skipped_or <- function(x) {
    declare(type(x = double(1)))
    TRUE || (1 / x > 0)
  }

  expect_translation_snapshots(skipped_and)
  expect_quick_identical(skipped_and, list(0))
  expect_quick_identical(skipped_or, list(0))
})

test_that("short-circuited calls defer arity errors", {
  unary_intrinsics <- c(
    "sin",
    "cos",
    "tan",
    "tanh",
    "asin",
    "acos",
    "atan",
    "sqrt",
    "exp",
    "log",
    "floor",
    "ceiling",
    "trunc",
    "log10",
    "abs",
    "Re",
    "Im",
    "Mod",
    "Arg",
    "Conj",
    "as.double",
    "as.integer",
    "cat",
    "dim",
    "drop",
    "is.null",
    "length",
    "ncol",
    "nrow",
    "print",
    "rev",
    "seq_along",
    "seq_len",
    "t",
    "which.max",
    "which.min"
  )
  bad_calls <- lapply(unary_intrinsics, \(op) as.call(list(as.name(op))))
  skipped <- function() NULL
  body(skipped) <- as.call(c(
    quote(`{`),
    list(as.call(c(quote(c), lapply(bad_calls, \(x) call("&&", FALSE, x)))))
  ))
  reached <- function() {
    TRUE && sqrt()
  }

  expect_quick_identical(skipped, list())
  expect_error(quick(reached)(), "sqrt")
})

test_that("short-circuited local closure diagnostics are deferred", {
  skipped_missing <- function() {
    predicate <- function(x) x > 0
    FALSE && predicate()
  }
  reached_missing <- function() {
    predicate <- function(x) x > 0
    TRUE && predicate()
  }
  skipped_body <- function() {
    predicate <- function(x) is.null(1)
    FALSE && predicate(1)
  }
  reached_body <- function() {
    predicate <- function(x) is.null(1)
    TRUE && predicate(1)
  }

  expect_quick_identical(skipped_missing, list())
  expect_quick_identical(skipped_body, list())

  qmissing <- quick(reached_missing)
  expect_error(qmissing(), "missing required argument")
  qbody <- quick(reached_body)
  expect_error(qbody(), "is only supported on symbols")
})

test_that("short-circuited fixed-arity handlers defer arity errors", {
  arities <- c(
    `$` = "exactly two",
    array = "two or three",
    chol2inv = "exactly one",
    ifelse = "exactly three",
    rep.int = "exactly two"
  )
  bad_calls <- lapply(names(arities), \(op) as.call(list(as.name(op))))

  for (i in seq_along(bad_calls)) {
    bad_call <- bad_calls[[i]]
    skipped <- function() NULL
    body(skipped) <- call("{", call("&&", FALSE, bad_call))
    reached <- function() NULL
    body(reached) <- call("{", call("&&", TRUE, bad_call))

    expect_identical(skipped(), FALSE)
    expect_identical(quick(skipped)(), FALSE)
    expect_error(
      quick(reached)(),
      paste0("requires ", arities[[i]], " argument")
    )
  }

  explicit_null_array <- function() {
    sum(array(1L, dim = 1L, dimnames = NULL))
  }
  expect_quick_identical(explicit_null_array, list())
})

test_that("short-circuited matrix calls defer missing-argument errors", {
  skipped <- function() {
    out <- FALSE && (sum(cbind()) > 0)
    out <- FALSE && (sum(rbind()) > 0)
    out <- FALSE && (sum(crossprod()) > 0)
    out <- FALSE && (sum(tcrossprod()) > 0)
    out <- FALSE && (sum(outer()) > 0)
    out <- FALSE && (sum(solve()) > 0)
    out <- FALSE && (sum(qr.solve()) > 0)
    out <- FALSE && (sum(chol()) > 0)
    out <- FALSE && (sum(forwardsolve()) > 0)
    out <- FALSE && (sum(backsolve()) > 0)
    out
  }
  reached_one <- function() {
    TRUE && (sum(solve()) > 0)
  }
  reached_two <- function() {
    TRUE && (sum(outer()) > 0)
  }

  expect_quick_identical(skipped, list())
  expect_error(quick(reached_one)(), "requires at least one argument")
  expect_error(quick(reached_two)(), "requires at least two arguments")

  braced <- function(flag) {
    declare(type(flag = logical(1)))
    flag &&
      {
        sum(matrix(1, 1, 1, byrow = TRUE)) > 0
      }
  }
  qbraced <- quick(braced)
  expect_false(qbraced(FALSE))
  expect_error(qbraced(TRUE), "byrow=TRUE", fixed = TRUE)
})

test_that("short-circuited operators defer arity errors", {
  bad_calls <- lapply(
    c(
      "(",
      "!",
      "&&",
      "||",
      "&",
      "|",
      "<",
      "<=",
      ">",
      ">=",
      "==",
      "!=",
      "+",
      "-",
      "*",
      "/",
      "^",
      "%%",
      "%/%",
      "%*%",
      "%o%",
      ":"
    ),
    \(op) as.call(list(as.name(op)))
  )
  bad_calls[[length(bad_calls) + 1L]] <- NA

  for (bad_call in bad_calls) {
    skipped <- function() NULL
    body(skipped) <- call("{", call("&&", FALSE, bad_call))
    reached <- function() NULL
    body(reached) <- call("{", call("&&", TRUE, bad_call))

    expect_identical(skipped(), FALSE)
    expect_identical(quick(skipped)(), FALSE)
    qfn <- quick(reached)
    expect_error(qfn())
  }

  bad_comparison <- as.call(list(as.name("==")))
  skipped_nested <- function() NULL
  body(skipped_nested) <- call(
    "{",
    call("&&", FALSE, call("&&", TRUE, bad_comparison))
  )
  reached_nested <- function() NULL
  body(reached_nested) <- call(
    "{",
    call("&&", TRUE, call("&&", TRUE, bad_comparison))
  )
  expect_identical(quick(skipped_nested)(), FALSE)
  expect_error(quick(reached_nested)(), "requires exactly two arguments")

  reached_with_effects <- function() {
    TRUE && ((runif(1) > 0) & abs())
  }
  qfn <- quick(reached_with_effects)
  set.seed(817)
  expect_error(qfn(), "abs")
  actual_seed <- .Random.seed

  set.seed(817)
  runif(1)
  expect_identical(actual_seed, .Random.seed)

  body(skipped) <- quote({
    TRUE || NA
  })
  expect_quick_identical(skipped, list())
  body(reached) <- quote({
    FALSE && (x <- TRUE)
  })
  expect_error(quick(reached), "assignment expressions")
})

test_that("nested short-circuits own their right-operand diagnostics", {
  outer_skipped_and <- function() {
    FALSE && (TRUE && abs())
  }
  outer_skipped_or <- function() {
    TRUE || (FALSE || abs())
  }
  skipped <- function() {
    TRUE && (FALSE && abs())
  }
  reached <- function() {
    TRUE && (TRUE && abs())
  }

  expect_quick_identical(outer_skipped_and, list())
  expect_quick_identical(outer_skipped_or, list())
  expect_quick_identical(skipped, list())
  qfn <- quick(reached)
  expect_error(qfn(), "abs")
})

test_that("short-circuited elementwise shape errors are deferred", {
  skipped <- function() {
    FALSE && (logical(2) & logical(3))
  }
  reached <- function() {
    TRUE && (logical(2) & logical(3))
  }

  expect_quick_identical(skipped, list())
  qfn <- quick(reached)
  expect_error(qfn(), "elementwise vector operations")

  reached_with_effects <- function() {
    TRUE && ((runif(2) > 0) & (runif(3) > 0))
  }
  qfn <- quick(reached_with_effects)
  set.seed(816)
  expect_error(qfn(), "elementwise vector operations")
  actual_seed <- .Random.seed

  set.seed(816)
  runif(2)
  runif(3)
  expect_identical(actual_seed, .Random.seed)
})

test_that("short-circuited complex refusals are deferred", {
  skipped_modulo <- function(z) {
    declare(type(z = complex(1)))
    FALSE && ((z %% z) == z)
  }
  reached_modulo <- function(z) {
    declare(type(z = complex(1)))
    TRUE && ((z %% z) == z)
  }
  skipped_integer_division <- function(z) {
    declare(type(z = complex(1)))
    FALSE && ((z %/% z) == z)
  }
  reached_integer_division <- function(z) {
    declare(type(z = complex(1)))
    TRUE && ((z %/% z) == z)
  }
  skipped_matmul <- function(z) {
    declare(type(z = complex(1, 1)))
    FALSE && ((z %*% z) == 0i)
  }
  reached_matmul <- function(z) {
    declare(type(z = complex(1, 1)))
    TRUE && ((z %*% z) == 0i)
  }

  expect_quick_identical(skipped_modulo, list(1 + 1i))
  expect_error(quick(reached_modulo)(1 + 1i), "unimplemented complex operation")
  expect_quick_identical(skipped_integer_division, list(1 + 1i))
  expect_error(
    quick(reached_integer_division)(1 + 1i),
    "%/% only implemented for numeric types",
    fixed = TRUE
  )
  z <- matrix(1 + 1i, 1L, 1L)
  expect_quick_identical(skipped_matmul, list(z))
  expect_error(
    quick(reached_matmul)(z),
    "linear algebra in quickr is double-only"
  )
})

test_that("short-circuited real-only math calls defer mode errors", {
  for (op in c("floor", "ceiling", "trunc")) {
    skipped <- function() NULL
    body(skipped) <- call("{", call("&&", FALSE, call(">", call(op, 1i), 0)))
    reached <- function() NULL
    body(reached) <- call("{", call("&&", TRUE, call(">", call(op, 1i), 0)))

    expect_identical(skipped(), FALSE)
    expect_identical(quick(skipped)(), FALSE)
    expect_error(
      quick(reached)(),
      paste0(op, "() only implemented"),
      fixed = TRUE
    )
  }
})

test_that("short-circuited coercions defer mode errors", {
  messages <- c(
    as.double = "as.double() does not support complex input",
    as.integer = "as.integer() only implemented"
  )
  for (op in names(messages)) {
    skipped_and <- function() NULL
    body(skipped_and) <- call(
      "{",
      call("&&", FALSE, call(">", call(op, 1i), 0))
    )
    reached_and <- function() NULL
    body(reached_and) <- call(
      "{",
      call("&&", TRUE, call(">", call(op, 1i), 0))
    )
    skipped_ifelse <- function() NULL
    body(skipped_ifelse) <- call("{", call("ifelse", FALSE, call(op, 1i), 0))
    reached_ifelse <- function() NULL
    body(reached_ifelse) <- call("{", call("ifelse", TRUE, call(op, 1i), 0))

    expect_identical(quick(skipped_and)(), FALSE)
    expect_error(quick(reached_and)(), messages[[op]], fixed = TRUE)
    expect_identical(quick(skipped_ifelse)(), 0)
    expect_error(quick(reached_ifelse)(), messages[[op]], fixed = TRUE)
  }
})

test_that("invalid literal arithmetic remains inside lazy branches", {
  skipped_and <- function() {
    FALSE && (1 + "a" > 0)
  }
  reached_and <- function() {
    TRUE && (1 + "a" > 0)
  }
  skipped_ifelse <- function() {
    ifelse(FALSE, 1 + "a" > 0, TRUE)
  }
  reached_ifelse <- function() {
    ifelse(TRUE, 1 + "a" > 0, TRUE)
  }

  expect_quick_identical(skipped_and, list())
  expect_error(quick(reached_and)(), "Unsupported object type")
  expect_quick_identical(skipped_ifelse, list())
  expect_error(quick(reached_ifelse)(), "Unsupported object type")
})

test_that("unsupported comparisons remain inside lazy branches", {
  skipped_and <- function() {
    FALSE && ("a" == "b")
  }
  reached_and <- function() {
    TRUE && ("a" == "b")
  }
  skipped_ifelse <- function() {
    ifelse(FALSE, "a" == "b", TRUE)
  }
  reached_ifelse <- function() {
    ifelse(TRUE, "a" == "b", TRUE)
  }

  expect_quick_identical(skipped_and, list())
  expect_error(quick(reached_and)(), "Unsupported object type")
  expect_quick_identical(skipped_ifelse, list())
  expect_error(quick(reached_ifelse)(), "Unsupported object type")
})

test_that("lazy multi-argument reductions defer operand errors", {
  skipped_and <- function() {
    FALSE && (sum(logical(2) & logical(3), 1L) > 0)
  }
  reached_and <- function() {
    TRUE && (sum(logical(2) & logical(3), 1L) > 0)
  }
  skipped_ifelse <- function() {
    ifelse(FALSE, sum(logical(2) & logical(3), 1L) > 0, TRUE)
  }
  reached_ifelse <- function() {
    ifelse(TRUE, sum(logical(2) & logical(3), 1L) > 0, TRUE)
  }

  expect_quick_identical(skipped_and, list())
  expect_error(quick(reached_and)(), "elementwise vector operations")
  expect_quick_identical(skipped_ifelse, list())
  expect_error(quick(reached_ifelse)(), "elementwise vector operations")
})

test_that("short-circuited BLAS shape errors are deferred", {
  skipped <- function(a, b) {
    declare(type(a = double(2, 3)), type(b = double(4, 2)))
    FALSE && (sum(a %*% b) > 0)
  }
  reached <- function(a, b) {
    declare(type(a = double(2, 3)), type(b = double(4, 2)))
    TRUE && (sum(a %*% b) > 0)
  }

  a <- matrix(as.double(1:6), 2, 3)
  b <- matrix(as.double(1:8), 4, 2)
  expect_quick_identical(skipped, list(a, b))
  qfn <- quick(reached)
  expect_error(qfn(a, b), "non-conformable arguments in %*%", fixed = TRUE)
})

test_that("&& and || accept one-element matrices", {
  matrix_and <- function(x, y) {
    declare(type(x = logical(1, 1)), type(y = logical(1, 1)))
    x && y
  }
  matrix_or <- function(x, y) {
    declare(type(x = logical(1, 1)), type(y = logical(1, 1)))
    x || y
  }

  true <- matrix(TRUE, 1, 1)
  false <- matrix(FALSE, 1, 1)
  expect_quick_identical(matrix_and, list(true, true), list(true, false))
  expect_quick_identical(matrix_or, list(false, false), list(false, true))
})

test_that("&& and || short-circuit like R's scalar operators", {
  # The right operand indexes past the end of x whenever the left side
  # already decides; R never evaluates it.
  guarded_index <- function(i, x) {
    declare(type(i = integer(1)), type(x = double(3)))
    out <- 0
    if (i <= 3L && x[i] > 0) {
      out <- 1
    }
    out
  }
  expect_translation_snapshots(guarded_index)
  expect_quick_identical(guarded_index, list(5L, c(1, 2, 3)))
  expect_quick_identical(guarded_index, list(2L, c(1, 2, 3)))
  expect_quick_identical(guarded_index, list(2L, c(1, -2, 3)))

  or_guarded <- function(a, x, i) {
    declare(type(a = logical(1)), type(x = double(2)), type(i = integer(1)))
    out <- a || x[i] > 0
    out
  }
  expect_quick_identical(or_guarded, list(TRUE, c(1, 2), 9L))
  expect_quick_identical(or_guarded, list(FALSE, c(-1, 2), 2L))

  shadowed_abs <- function() {
    calls <- 0L
    abs <- function() {
      calls <<- calls + 1L
      TRUE
    }
    and_result <- FALSE && abs()
    or_result <- TRUE || abs()
    list(and_result = and_result, or_result = or_result, calls = calls)
  }
  expect_quick_identical(shadowed_abs, list())
})

test_that("short-circuit results are private to parallel iterations", {
  parallel_and <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    n <- length(x)
    out <- logical(n)
    declare(parallel())
    for (i in seq_along(x)) {
      out[i] <- x[i] > 0 && y[i] > 0
    }
    out
  }

  x <- rep(c(-1, 1), 500)
  y <- rep(1, length(x))
  expected <- parallel_and(x, y)
  expect_translation_snapshots(parallel_and)
  qfn <- quick(parallel_and)
  expect_identical(qfn(x, y), expected)
})

test_that("while re-evaluates hoisted condition code every iteration", {
  # The canonical scan idiom: the && lowering hoists statements, which
  # must re-run per iteration, not once before the loop.
  scan_positive <- function(x) {
    declare(type(x = double(n)))
    i <- 1L
    n <- length(x)
    while (i <= n && x[i] > 0) {
      i <- i + 1L
    }
    i
  }
  expect_translation_snapshots(scan_positive)
  expect_quick_identical(scan_positive, list(c(1, 2, -1, 5)))
  expect_quick_identical(scan_positive, list(c(1, 2, 3)))
  expect_quick_identical(scan_positive, list(c(-1, 2)))
})
