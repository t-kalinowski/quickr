skip_on_cran()

test_that("parallel loops reject scalar accumulators before compilation", {
  unsafe_sum <- function(x) {
    declare(type(x = double(NA)))
    total <- 0.0
    declare(parallel())
    for (i in seq_along(x)) {
      total <- total + x[i]
    }
    total
  }

  expect_error(r2f(unsafe_sum), "accumulator.*total")
  expect_error(quick(unsafe_sum), "accumulator.*total")
})

test_that("parallel accumulator checks cover arithmetic and reductions", {
  updates <- list(
    quote(total + x[i]),
    quote(x[i] + total),
    quote(total - x[i]),
    quote(total * x[i]),
    quote(min(total, x[i])),
    quote(max(x[i], total)),
    quote((total + x[i]) / 2)
  )

  for (update in updates) {
    fn <- eval(bquote(function(x) {
      declare(type(x = double(NA)))
      total <- 1.0
      declare(omp())
      for (i in seq_along(x)) {
        total <- .(update)
      }
      total
    }))
    expect_error(r2f(fn), "accumulator.*total")
  }

  for (update in list(quote(total && x[i]), quote(x[i] || total))) {
    fn <- eval(bquote(function(x) {
      declare(type(x = logical(NA)))
      total <- TRUE
      declare(parallel())
      for (i in seq_along(x)) {
        total <- .(update)
      }
      total
    }))
    expect_error(r2f(fn), "accumulator.*total")
  }
})

test_that("accumulator checks include branches and nested serial loops", {
  conditional_sum <- function(x) {
    declare(type(x = double(NA)))
    total <- 0.0
    declare(parallel())
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        total <- total + x[i]
      }
    }
    total
  }

  nested_sum <- function(x) {
    declare(type(x = double(NA, NA)))
    total <- 0.0
    declare(parallel())
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        total <- total + x[i, j]
      }
    }
    total
  }

  expect_error(r2f(conditional_sum), "accumulator.*total")
  expect_error(r2f(nested_sum), "accumulator.*total")
})

test_that("arguments and value loops cannot supply shared accumulators", {
  argument_sum <- function(x, total) {
    declare(type(x = double(NA)), type(total = double(1)))
    declare(parallel())
    for (i in seq_along(x)) {
      total <- total + x[i]
    }
    total
  }

  value_sum <- function(x) {
    declare(type(x = double(NA)))
    total <- 0.0
    declare(parallel())
    for (value in x) {
      total <- total + value
    }
    total
  }

  expect_error(r2f(argument_sum), "accumulator.*total")
  expect_error(r2f(value_sum), "accumulator.*total")
})

test_that("assignment aliases and resets do not make shared accumulators safe", {
  reset_sum <- function(x) {
    declare(type(x = double(NA)))
    total <- 0.0
    declare(parallel())
    for (i in seq_along(x)) {
      total <- 0.0
      # fmt: skip
      total = total + x[i]
    }
    total
  }

  expect_error(r2f(reset_sum), "accumulator.*total")
})

test_that("accumulator checks normalize grouped assignment calls and empty indices", {
  grouped_sum <- function(x) {
    declare(type(x = double(NA, NA)))
    total <- 0.0
    declare(parallel())
    for (i in seq_len(nrow(x))) {
      (`<-`)(total, total + sum(x[i, ]))
    }
    total
  }

  expect_error(r2f(grouped_sum), "accumulator.*total")
})

test_that("deferred accumulator errors do not emit an unsafe parallel loop", {
  conditional_sum <- function(x, go) {
    declare(type(x = double(NA)), type(go = logical(1)))
    calc <- function() {
      total <- 0.0
      declare(parallel())
      for (i in seq_along(x)) {
        total <- total + x[i]
      }
      total > 0
    }
    go && calc()
  }

  code <- as.character(r2f(conditional_sum))
  expect_match(code, "shared scalar accumulator `total`", fixed = TRUE)
  expect_false(grepl("!$omp parallel do", code, fixed = TRUE))
})

test_that("parallel loops reject called closures that write captured scalars", {
  for (prior_call in list(quote(NULL), quote(add(1.0)))) {
    captured_sum <- eval(bquote(function(x) {
      declare(type(x = double(NA)))
      total <- 0.0
      add <- function(value) {
        total <<- total + value
        NULL
      }
      .(prior_call)
      declare(parallel())
      for (i in seq_along(x)) {
        add(1.0)
      }
      total
    }))

    expect_error(r2f(captured_sum), "captured scalar.*total")
  }
})

test_that("parallel sapply rejects direct and nested captured scalar writes", {
  captured_sum <- function(x) {
    declare(type(x = double(NA)))
    total <- 0.0
    declare(parallel())
    out <- sapply(x, function(value) {
      total <<- total + value
      total
    })
    out
  }

  nested_sum <- function(x) {
    declare(type(x = double(NA)))
    total <- 0.0
    add <- function(value) {
      total <<- total + value
      total
    }
    add(0.0)
    declare(parallel())
    out <- sapply(x, function(value) add(value))
    out
  }

  expect_error(r2f(captured_sum), "captured scalar.*total")
  expect_error(r2f(nested_sum), "captured scalar.*total")
})

test_that("uncalled captures and local shadowing remain valid in parallel loops", {
  skip_if_no_openmp()

  local_sum <- function(x) {
    declare(type(x = double(NA)))
    total <- 10.0
    add <- function(value) {
      total <- 1.0
      total <- total + value
      total
    }
    out <- double(length(x))
    declare(parallel())
    for (i in seq_along(x)) {
      unused <- function(value) {
        total <<- total + value
        total
      }
      out[i] <- x[i] + add(i)
    }
    out + total
  }

  expect_quick_identical(local_sum, double(), as.double(seq_len(1000L)))
})

test_that("serial scalar accumulation retains R semantics", {
  serial_sum <- function(x) {
    declare(type(x = double(NA)))
    total <- 0.0
    for (i in seq_along(x)) {
      total <- total + x[i]
    }
    total
  }

  expect_quick_identical(serial_sum, double(), c(1, -2, 3))
})

test_that("parallel loops retain independent indexed accumulation", {
  skip_if_no_openmp()

  indexed_sum <- function(x) {
    declare(type(x = integer(NA, NA)))
    out <- integer(nrow(x))
    declare(parallel())
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i] <- out[i] + x[i, j]
      }
    }
    out
  }

  expect_translation_snapshots(indexed_sum)
  expect_quick_identical(
    indexed_sum,
    matrix(integer(), nrow = 0L, ncol = 3L),
    matrix(seq_len(24L), nrow = 8L)
  )
})

test_that("parallel sapply callbacks can accumulate in local variables", {
  skip_if_no_openmp()

  repeated_sum <- function(x) {
    declare(type(x = integer(NA)))
    declare(parallel())
    out <- sapply(x, function(value) {
      total <- 0L
      for (j in seq_len(3L)) {
        total <- total + value
      }
      total
    })
    out
  }

  expect_translation_snapshots(repeated_sum)
  expect_quick_identical(repeated_sum, seq_len(1000L))
})
