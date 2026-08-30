# Unit tests for C bridge hoisting of size expressions

skip_on_cran()

test_that("size check blocks redeclare hoisted size temps", {
  fn <- function(n, m, a, b) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(a = double(min(n, m))),
      type(b = double(min(n, m))),
      type(out = double(min(n, m)))
    )
    out <- double(min(n, m))
    for (i in seq_len(length(out))) {
      out[i] <- a[i] + b[i]
    }
    out
  }

  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(3L, 5L, c(1, 2, 3), c(10, 20, 30)),
    list(5L, 3L, c(1, 2, 3), c(4, 5, 6))
  )
})

test_that("C bridge casts completed real size expressions", {
  fn <- function(x, y) {
    declare(type(x = double(1)), type(y = double(1)))
    out <- diag(x + y)
    out
  }

  dll_paths_before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(dll_paths_before), add = TRUE)
  qfn := quick(fn)

  dll_paths <- setdiff(loaded_dll_paths(), dll_paths_before)
  expect_length(dll_paths, 1L)
  c_path <- list.files(
    dirname(dll_paths[[1L]]),
    pattern = "_c_wrapper[.]c$",
    full.names = TRUE
  )
  expect_length(c_path, 1L)
  c_code <- paste(readLines(c_path, warn = FALSE), collapse = "\n")
  expect_match(
    c_code,
    "((R_xlen_t)((Rf_asReal(x) + Rf_asReal(y))))",
    fixed = TRUE
  )

  x <- c(1.7)
  y <- c(1.7)
  expect_identical(qfn(x, y), fn(x, y))
})
