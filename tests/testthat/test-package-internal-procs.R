test_that("package compilation: local closures don't collide across quick functions", {
  pkgdir <- withr::local_tempdir(pattern = "quickr-contains-pkg-")
  pkgpath <- file.path(pkgdir, "quickr.contains.pkg")
  dir.create(pkgpath)
  dir.create(file.path(pkgpath, "R"))
  dir.create(file.path(pkgpath, "src"))

  writeLines(
    c(
      "Package: quickr.contains.pkg",
      "Title: quickr internal-proc test package",
      "Version: 0.0.0.9000",
      "Description: Internal tests for quickr package compilation.",
      "License: MIT",
      "Encoding: UTF-8",
      "RoxygenNote: 7.3.2",
      "Imports: quickr"
    ),
    file.path(pkgpath, "DESCRIPTION")
  )

  writeLines(
    c(
      "useDynLib(quickr.contains.pkg, .registration = TRUE)",
      "export(fn1)",
      "export(fn2)"
    ),
    file.path(pkgpath, "NAMESPACE")
  )

  writeLines(
    c(
      "declare <- function(...) invisible()",
      "",
      "# Two different quick functions defining the same local closure name `f`.",
      "# If local closures were emitted as global procedures, this would fail to",
      "# compile due to duplicate symbol definitions in a single compilation unit.",
      "",
      "fn1 <- quickr::quick(\"fn1\", function(x) {",
      "  declare(type(x = double(NA)))",
      "  f <- function(i) x[i] + 1.0",
      "  out <- double(length(x))",
      "  out <- sapply(seq_along(out), f)",
      "  out",
      "})",
      "",
      "fn2 <- quickr::quick(\"fn2\", function(x) {",
      "  declare(type(x = double(NA)))",
      "  f <- function(i) x[i] * 2.0",
      "  out <- double(length(x))",
      "  out <- sapply(seq_along(out), f)",
      "  out",
      "})"
    ),
    file.path(pkgpath, "R", "pkg.R")
  )

  # Compile the package using the *development* quickr currently under test,
  # without relying on pkgload/pkgbuild (which may not have build tools in test
  # environments).
  pkg_env <- new.env(parent = baseenv())
  quickr:::collector$activate("quickr.contains.pkg:quick_funcs")
  sys.source(file.path(pkgpath, "R", "pkg.R"), envir = pkg_env)
  withr::with_dir(pkgpath, quickr:::dump_collected())

  build_dir <- withr::local_tempdir(pattern = "quickr-contains-build-")
  file.copy(
    file.path(pkgpath, "src", "quickr_sub_routines.f90"),
    file.path(build_dir, "quickr_sub_routines.f90"),
    overwrite = TRUE
  )
  file.copy(
    file.path(pkgpath, "src", "quickr_entrypoints.c"),
    file.path(build_dir, "quickr_entrypoints.c"),
    overwrite = TRUE
  )

  owd <- setwd(build_dir)
  on.exit(setwd(owd), add = TRUE)

  res <- suppressWarnings(system2(
    R.home("bin/R"),
    c(
      "CMD",
      "SHLIB",
      "-o",
      paste0("quickr.contains.pkg", .Platform$dynlib.ext),
      "quickr_sub_routines.f90",
      "quickr_entrypoints.c"
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!is.null(status <- attr(res, "status"))) {
    writeLines(res)
    stop("Compilation failed with status: ", status)
  }

  expect_true(file.exists(paste0("quickr.contains.pkg", .Platform$dynlib.ext)))
})
