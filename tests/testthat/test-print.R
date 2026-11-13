skip_on_cran()

test_that("scalar printers accept integer, double, and both logical storage forms", {
  fn <- function(i, x, flag) {
    declare(type(i = integer(1)), type(x = double(1)), type(flag = logical(1)))
    local_flag <- x > 0
    print(i)
    print(x)
    print(flag)
    print(local_flag)
    x
  }
  before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(before), add = TRUE)
  qfn <- quick(fn)
  output <- capture.output(value <- qfn(2L, -3.5, TRUE))
  expect_identical(output, c("[1] 2", "[1] -3.5", "[1] 1", "[1] 0"))
  expect_identical(value, -3.5)
  expect_translation_snapshots(fn)
})

test_that("array printers accept integer, double, and both logical storage forms", {
  fn <- function(i, x, flag) {
    declare(type(i = integer(n)), type(x = double(n)), type(flag = logical(n)))
    local_flag <- x > 0
    print(i)
    print(x)
    print(flag)
    print(local_flag)
    x
  }
  before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(before), add = TRUE)
  qfn <- quick(fn)
  x <- c(-3.5, 4.5)
  flag <- c(TRUE, FALSE)
  output <- capture.output(value <- qfn(1:2, x, flag))
  expected <- c(
    capture.output(print(1:2)),
    capture.output(print(x)),
    capture.output(print(as.integer(flag))),
    capture.output(print(as.integer(x > 0)))
  )
  expect_identical(output, expected)
  expect_identical(value, x)
  expect_translation_snapshots(fn)
})

test_that("cat prints literal labels with embedded quotes", {
  fn <- function(x) {
    declare(type(x = double(1)))
    cat("ready\n")
    cat("say \"hi\"\n")
    x
  }
  before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(before), add = TRUE)
  qfn <- quick(fn)
  output <- capture.output(value <- qfn(2))
  expect_identical(output, capture.output(invisible(fn(2))))
  expect_identical(value, 2)
  expect_translation_snapshots(fn)
})
