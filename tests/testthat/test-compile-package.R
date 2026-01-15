# Integration tests for compile_package() and the pkgload::load_all() hook

create_test_package <- function(
  .local_envir = parent.frame(),
  use_dynlib = TRUE,
  named_quick = TRUE
) {
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

  namespace_lines <- c(
    "export(add_ab)",
    "importFrom(quickr,quick)"
  )
  if (isTRUE(use_dynlib)) {
    namespace_lines <- c(
      namespace_lines,
      sprintf("useDynLib(%s, .registration = TRUE)", pkgname)
    )
  }
  writeLines(namespace_lines, file.path(pkgpath, "NAMESPACE"))

  quick_call <- if (isTRUE(named_quick)) {
    "add_ab <- quickr::quick(\"add_ab\", function(a, b) {"
  } else {
    "add_ab <- quickr::quick(function(a, b) {"
  }

  writeLines(
    c(
      "declare <- function(...) invisible()",
      "",
      quick_call,
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

run_r <- function(code) {
  out <- suppressWarnings(system2(
    R.home("bin/R"),
    c("--vanilla", "--slave", "-e", shQuote(code)),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!is.null(status <- attr(out, "status"))) {
    writeLines(out)
    stop("R subprocess failed with status: ", status)
  }
  out
}

test_that("compile_package errors when path is not an R package", {
  temp <- withr::local_tempdir()
  expect_error(
    quickr::compile_package(temp),
    "does not appear to be an R package"
  )
})

test_that("compile_package errors when DESCRIPTION is empty", {
  temp <- withr::local_tempdir()
  withr::local_dir(temp)
  dir.create("R")
  writeLines("", "DESCRIPTION")

  expect_error(
    quickr::compile_package("."),
    "does not point to an R package"
  )
})

test_that("compile_package runs pkgload::load_all and writes src outputs", {
  skip_on_cran()
  skip_if_not_installed("pkgload")

  pkgpath <- create_test_package(named_quick = TRUE, use_dynlib = TRUE)
  entrypoints <- file.path(pkgpath, "src", "quickr_entrypoints.c")
  fsubs <- file.path(pkgpath, "src", "quickr_sub_routines.f90")

  expect_false(file.exists(entrypoints))
  expect_false(file.exists(fsubs))

  expect_silent(quickr::compile_package(pkgpath))

  expect_true(file.exists(entrypoints))
  expect_true(file.exists(fsubs))
  expect_true(file.info(entrypoints)$size > 0)
  expect_true(file.info(fsubs)$size > 0)
  expect_true(any(grepl("add_ab_", readLines(entrypoints), fixed = TRUE)))
})

test_that("pkgload::load_all writes outputs and resolves anonymous quick() names", {
  skip_on_cran()
  skip_if_not_installed("pkgload")

  pkgpath <- create_test_package(named_quick = FALSE, use_dynlib = TRUE)
  pkgpath <- normalizePath(pkgpath, winslash = "/", mustWork = TRUE)
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
  run_r(code)

  expect_true(file.exists(entrypoints))
  expect_true(file.exists(fsubs))
  expect_true(any(grepl("add_ab_", readLines(entrypoints), fixed = TRUE)))
})

test_that("pkgload::load_all messages when NAMESPACE lacks useDynLib", {
  skip_on_cran()
  skip_if_not_installed("pkgload")

  pkgpath <- create_test_package(named_quick = TRUE, use_dynlib = FALSE)
  pkgpath <- normalizePath(pkgpath, winslash = "/", mustWork = TRUE)
  entrypoints <- file.path(pkgpath, "src", "quickr_entrypoints.c")

  code <- paste(
    sprintf("setwd(%s)", shQuote(pkgpath)),
    "suppressPackageStartupMessages(library(pkgload))",
    "pkgload::load_all('.', quiet = TRUE)",
    sep = "; "
  )
  out <- run_r(code)

  expect_true(file.exists(entrypoints))
  expect_true(any(grepl("@useDynLib", out)))
})
