# Unit tests for compile-package helpers

test_that("dump_collected writes src stubs for collected quick functions", {
  temp <- withr::local_tempdir()
  withr::local_dir(temp)

  writeLines("useDynLib(pkg, .registration = TRUE)", "NAMESPACE")
  dir.create("src")

  closure_env <- new.env(parent = asNamespace("quickr"))
  fn <- evalq(
    function(x) {
      declare(type(x = double(1)))
      x + 1
    },
    closure_env
  )

  quick_closure <- quickr:::create_quick_closure("fn", fn)

  quickr:::collector$activate("test")
  quickr:::collector$add(
    name = "fn",
    closure = fn,
    quick_closure = quick_closure
  )

  testthat::capture_output({
    result <- suppressMessages(withVisible(quickr:::dump_collected()))
  })
  expect_false(result$visible)

  expect_true(file.exists("src/quickr_entrypoints.c"))
  expect_true(file.exists("src/quickr_sub_routines.f90"))
  expect_true(file.info("src/quickr_entrypoints.c")$size > 0)
  expect_true(file.info("src/quickr_sub_routines.f90")$size > 0)
})
