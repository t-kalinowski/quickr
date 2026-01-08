# Unit tests for compile-package helpers

create_test_package <- function(.local_envir = parent.frame()) {
  pkgname <- "quickr.test.pkg"
  pkgdir <- withr::local_tempdir(
    pattern = "quickr-test-pkg-",
    .local_envir = .local_envir
  )
  pkgpath <- file.path(pkgdir, pkgname)
  dir.create(pkgpath)
  dir.create(file.path(pkgpath, "R"))

  writeLines(
    c(
      paste0("Package: ", pkgname),
      "Title: quickr integration test package",
      "Version: 0.0.0.9000",
      "Description: Temporary package for quickr integration tests.",
      "License: MIT",
      "Encoding: UTF-8",
      "Imports: quickr"
    ),
    file.path(pkgpath, "DESCRIPTION")
  )

  writeLines(
    c(
      "export(add_ab)",
      "importFrom(quickr,quick)",
      sprintf("useDynLib(%s, .registration = TRUE)", pkgname)
    ),
    file.path(pkgpath, "NAMESPACE")
  )

  writeLines(
    c(
      "declare <- function(...) invisible()",
      "",
      "add_ab <- quickr::quick(\"add_ab\", function(a, b) {",
      "  declare({",
      "    type(a = double(n))",
      "    type(b = double(n))",
      "  })",
      "  out <- a + b",
      "  out",
      "})"
    ),
    file.path(pkgpath, "R", "package.R")
  )

  pkgpath
}

test_that("compile_package errors when path is not an R package", {
  temp <- withr::local_tempdir()
  expect_error(
    quickr:::compile_package(temp),
    "does not appear to be an R package"
  )
})

test_that("compile_package changes directory when path is not '.'", {
  temp <- withr::local_tempdir()
  dir.create(file.path(temp, "R"))
  writeLines("Package: testpkg\n", file.path(temp, "DESCRIPTION"))

  skip_on_cran()

  # Mock system2 to avoid actually running R
  local_mocked_bindings(
    system2 = function(...) invisible(NULL),
    .package = "base"
  )

  expect_silent(quickr:::compile_package(temp))
})

test_that("compile_package runs pkgload::load_all and writes src outputs", {
  skip_on_cran()
  skip_if_not_installed("pkgload")

  pkgpath <- create_test_package()
  entrypoints <- file.path(pkgpath, "src", "quickr_entrypoints.c")
  fsubs <- file.path(pkgpath, "src", "quickr_sub_routines.f90")

  expect_false(file.exists(entrypoints))
  expect_false(file.exists(fsubs))

  expect_silent(quickr:::compile_package(pkgpath))

  expect_true(file.exists(entrypoints))
  expect_true(file.exists(fsubs))
  expect_true(any(grepl("add_ab_", readLines(entrypoints), fixed = TRUE)))
})

test_that("pkgload::load_all triggers quickr collection and writes src outputs", {
  skip_on_cran()
  skip_if_not_installed("pkgload")

  pkgpath <- create_test_package()
  entrypoints <- file.path(pkgpath, "src", "quickr_entrypoints.c")
  fsubs <- file.path(pkgpath, "src", "quickr_sub_routines.f90")

  expect_false(file.exists(entrypoints))
  expect_false(file.exists(fsubs))

  code <- paste(
    sprintf("setwd(%s)", shQuote(pkgpath)),
    "suppressPackageStartupMessages(library(pkgload))",
    "pkgload::load_all('.', quiet = TRUE)",
    sep = "; "
  )
  out <- suppressWarnings(system2(
    R.home("bin/R"),
    c("--vanilla", "--slave", "-e", shQuote(code)),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!is.null(status <- attr(out, "status"))) {
    writeLines(out)
    stop("load_all subprocess failed with status: ", status)
  }

  expect_true(file.exists(entrypoints))
  expect_true(file.exists(fsubs))
  expect_true(any(grepl("add_ab_", readLines(entrypoints), fixed = TRUE)))
})

test_that("compile_package works from a clean R session", {
  skip_on_cran()
  skip_if_not_installed("pkgload")

  pkgpath <- normalizePath(
    create_test_package(),
    winslash = "/",
    mustWork = TRUE
  )
  quickr_root <- normalizePath(
    testthat::test_path(".."),
    winslash = "/",
    mustWork = TRUE
  )
  entrypoints <- file.path(pkgpath, "src", "quickr_entrypoints.c")
  fsubs <- file.path(pkgpath, "src", "quickr_sub_routines.f90")

  expect_false(file.exists(entrypoints))
  expect_false(file.exists(fsubs))

  code <- paste(
    c(
      "if (!requireNamespace('quickr', quietly = TRUE)) {",
      "  suppressPackageStartupMessages(library(pkgload))",
      sprintf("  pkgload::load_all(%s, quiet = TRUE)", shQuote(quickr_root)),
      "} else {",
      "  suppressPackageStartupMessages(library(quickr))",
      "}",
      sprintf("quickr:::compile_package(%s)", shQuote(pkgpath))
    ),
    collapse = "\n"
  )
  out <- suppressWarnings(system2(
    R.home("bin/R"),
    c("--vanilla", "--slave", "-e", shQuote(code)),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!is.null(status <- attr(out, "status"))) {
    writeLines(out)
    stop("compile_package subprocess failed with status: ", status)
  }

  expect_true(file.exists(entrypoints))
  expect_true(file.exists(fsubs))
  expect_true(any(grepl("add_ab_", readLines(entrypoints), fixed = TRUE)))
})

test_that("compile_package errors when DESCRIPTION is empty", {
  temp <- withr::local_tempdir()
  withr::local_dir(temp)
  dir.create("R")
  # Create an empty DESCRIPTION file which causes read.dcf to return 0-row matrix
  writeLines("", "DESCRIPTION")

  # Mock system2 to prevent actual R call
  local_mocked_bindings(
    system2 = function(...) invisible(NULL),
    .package = "base"
  )

  expect_error(
    quickr:::compile_package("."),
    "does not point to an R package"
  )
})

test_that("dump_collected resolves anonymous function names", {
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

  quick_closure <- quickr:::create_quick_closure(
    "anonymous_quick_function_1",
    fn
  )
  closure_env$my_func <- quick_closure

  quickr:::collector$activate("test")
  quickr:::collector$add(
    name = "anonymous_quick_function_1",
    closure = fn,
    quick_closure = quick_closure
  )

  testthat::capture_output({
    suppressMessages(quickr:::dump_collected())
  })

  expect_true(file.exists("src/quickr_entrypoints.c"))
})

test_that("dump_collected messages when useDynLib missing in NAMESPACE", {
  temp <- withr::local_tempdir()
  withr::local_dir(temp)

  # NAMESPACE without useDynLib(.registration = TRUE)
  writeLines("export(foo)", "NAMESPACE")
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
    expect_message(
      quickr:::dump_collected(),
      "@useDynLib"
    )
  })
})

test_that("dump_collected creates src directory if missing", {
  temp <- withr::local_tempdir()
  withr::local_dir(temp)

  writeLines("useDynLib(pkg, .registration = TRUE)", "NAMESPACE")
  # Note: src/ not created

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
    suppressMessages(quickr:::dump_collected())
  })

  expect_true(dir.exists("src"))
  expect_true(file.exists("src/quickr_entrypoints.c"))
})

test_that("dump_collected generates alternative init when pkg has R_init", {
  # Create a temp dir with a specific name
  temp <- withr::local_tempdir(pattern = "testpkg")
  withr::local_dir(temp)
  pkgname <- basename(temp)

  writeLines("useDynLib(pkg, .registration = TRUE)", "NAMESPACE")
  dir.create("src")
  # Create a C file with existing R_init function for this pkgname
  writeLines(
    paste0("void R_init_", pkgname, "(DllInfo *dll) {}"),
    "src/init.c"
  )

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
    suppressMessages(quickr:::dump_collected())
  })

  c_content <- readLines("src/quickr_entrypoints.c")
  expect_true(any(grepl("_quick_functions", c_content)))
})

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
