test_that("openmp_makevars_lines uses explicit env flags", {
  withr::local_envvar(c(
    QUICKR_OPENMP_FFLAGS = "-fopenmp",
    QUICKR_OPENMP_LIBS = "-fopenmp"
  ))

  expect_equal(
    quickr:::openmp_makevars_lines(),
    c("PKG_FFLAGS += -fopenmp", "PKG_LIBS += -fopenmp")
  )
})

test_that("openmp_makevars_lines errors when flags are missing", {
  local_mocked_bindings(
    openmp_fflags = function() "",
    .package = "quickr"
  )

  expect_error(
    quickr:::openmp_makevars_lines(),
    "OpenMP was requested but no OpenMP flags",
    class = "quickr_openmp_unavailable"
  )
})

test_that("openmp_makevars_lines errors when linker flags are missing", {
  local_mocked_bindings(
    openmp_fflags = function() "-fopenmp",
    openmp_link_flags = function(...) "",
    .package = "quickr"
  )

  expect_error(
    quickr:::openmp_makevars_lines(),
    "OpenMP was requested but no OpenMP linker flags",
    class = "quickr_openmp_unavailable"
  )
})

test_that("openmp_config_value caches toolchain lookups", {
  cache_env <- environment(quickr:::openmp_config_value)
  old_cache <- cache_env$cached
  old_config <- cache_env$quickr_r_cmd_config_value
  withr::defer(cache_env$cached <- old_cache)
  withr::defer({
    if (is.null(old_config)) {
      rm(quickr_r_cmd_config_value, envir = cache_env)
    } else {
      cache_env$quickr_r_cmd_config_value <- old_config
    }
  })
  cache_env$cached <- NULL

  calls <- 0
  cache_env$quickr_r_cmd_config_value <- function(...) {
    calls <<- calls + 1
    "value"
  }

  expect_equal(quickr:::openmp_config_value("QUICKR_TEST_CACHE"), "value")
  expect_equal(quickr:::openmp_config_value("QUICKR_TEST_CACHE"), "value")
  expect_equal(calls, 1)
})

test_that("get_pending_parallel returns NULL for NULL or non-scope", {
  expect_null(quickr:::get_pending_parallel(NULL))
  expect_null(quickr:::get_pending_parallel(list()))
})

test_that("take_pending_parallel returns NULL for NULL or non-scope", {
  expect_null(quickr:::take_pending_parallel(NULL))
  expect_null(quickr:::take_pending_parallel(list()))
})

test_that("parse_parallel_decl errors on unknown arguments", {
  e <- quote(parallel(foo))
  expect_error(
    quickr:::parse_parallel_decl(e),
    "unknown argument to parallel"
  )
})

test_that("parse_parallel_decl parses private argument", {
  e <- quote(parallel(private = c(x, y)))
  result <- quickr:::parse_parallel_decl(e)
  expect_equal(result$backend, "omp")
  expect_equal(result$private, c("x", "y"))

  # Single symbol
  e2 <- quote(parallel(private = z))
  result2 <- quickr:::parse_parallel_decl(e2)
  expect_equal(result2$private, "z")
})

test_that("parse_parallel_decl errors on invalid private values", {
  expect_error(
    quickr:::parse_parallel_decl(quote(parallel(private = 1))),
    "private must be a symbol or c\\(\\) of symbols"
  )

  expect_error(
    quickr:::parse_parallel_decl(quote(parallel(private = c(x, 1)))),
    "private must be a symbol or c\\(\\) of symbols"
  )
})

test_that("parallel private validates declared symbols (rolling mean)", {
  fn <- function(x, window) {
    declare(
      type(x = double(n)),
      type(window = double(j))
    )

    out <- double(length(x) - length(window) + 1)

    window_size <- as.double(length(window))

    declare(parallel(private = c(acc, j)))
    for (i in seq_along(out)) {
      acc <- 0
      for (j in seq_along(window)) {
        acc <- acc + x[(i + j - 1)] * window[j]
      }
      out[i] <- acc * window_size
    }
    out
  }

  skip_if_no_openmp()
  expect_quick_identical(
    fn,
    list(x = as.double(1:10), window = as.double(1:3))
  )
})

test_that("parallel private errors on undeclared symbols (rolling mean)", {
  fn <- function(x, window) {
    declare(
      type(x = double(n)),
      type(window = double(j))
    )

    out <- double(length(x) - length(window) + 1)

    window_size <- as.double(length(window))

    declare(parallel(private = c(qwerty, j)))
    for (i in seq_along(out)) {
      acc <- 0
      for (j in seq_along(window)) {
        acc <- acc + x[(i + j - 1)] * window[j]
      }
      out[i] <- acc * window_size
    }
    out
  }

  expect_error(
    r2f(fn),
    "could not resolve private symbol"
  )
})

test_that("parallel private ignores scalars assigned inside loop", {
  fn <- function(x, window) {
    declare(
      type(x = double(n)),
      type(window = double(j))
    )

    out <- double(length(x) - length(window) + 1)

    window_size <- as.double(length(window))

    declare(parallel(private = j))
    for (i in seq_along(out)) {
      acc <- 0
      for (j in seq_along(window)) {
        acc <- acc + x[(i + j - 1)] * window[j]
      }
      out[i] <- acc * window_size
    }
    out
  }

  expect_no_error(r2f(fn))
})

test_that("is_parallel_target_stmt handles edge cases", {
  # non-call

  expect_false(quickr:::is_parallel_target_stmt(1L))

  # assignment with less than 3 elements
  bad_assign <- quote(`<-`(x))
  expect_false(quickr:::is_parallel_target_stmt(bad_assign))

  # other call types
  expect_false(quickr:::is_parallel_target_stmt(quote(foo(x))))
})

test_that("openmp_directives errors for unsupported backend", {
  parallel <- list(backend = "cuda")
  expect_error(
    quickr:::openmp_directives(parallel),
    "unsupported parallel backend"
  )
})

test_that("openmp_fflags uses env variable first", {
  withr::local_envvar(QUICKR_OPENMP_FFLAGS = "-custom-flags")
  result <- quickr:::openmp_fflags()
  expect_equal(result, "-custom-flags")
})

test_that("openmp_link_flags uses env variable first", {
  withr::local_envvar(QUICKR_OPENMP_LIBS = "-lcustom")
  result <- quickr:::openmp_link_flags()
  expect_equal(result, "-lcustom")
})

test_that("openmp_fflags returns empty when no config found", {
  local_mocked_bindings(
    openmp_config_value = function(...) "",
    .package = "quickr"
  )
  withr::local_envvar(QUICKR_OPENMP_FFLAGS = "")

  result <- quickr:::openmp_fflags()
  expect_equal(result, "")
})

test_that("openmp_link_flags returns fflags for non-clang CC", {
  local_mocked_bindings(
    openmp_config_value = function(name, ...) {
      switch(name, SHLIB_OPENMP_CFLAGS = "", CC = "gcc", "")
    },
    .package = "quickr"
  )
  withr::local_envvar(QUICKR_OPENMP_LIBS = "")

  result <- quickr:::openmp_link_flags(fflags = "-fopenmp")
  expect_equal(result, "-fopenmp")
})

test_that("openmp_link_flags returns empty for clang CC", {
  local_mocked_bindings(
    openmp_config_value = function(name, ...) {
      switch(name, SHLIB_OPENMP_CFLAGS = "", CC = "clang", FC = "flang", "")
    },
    .package = "quickr"
  )
  withr::local_envvar(QUICKR_OPENMP_LIBS = "")

  result <- quickr:::openmp_link_flags(fflags = "-fopenmp")
  expect_equal(result, "")
})
