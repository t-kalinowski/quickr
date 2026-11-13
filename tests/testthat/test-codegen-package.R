skip_on_cran()

# Generate with the development namespace currently under test. A subprocess
# loading quickr from an installed library could snapshot an older compiler.
generate_codegen_package <- function(
  pkgname,
  source,
  init = NULL,
  .local_envir = parent.frame()
) {
  root <- withr::local_tempdir(
    pattern = "quickr-package-snapshot-",
    .local_envir = .local_envir
  )
  path <- file.path(root, pkgname)
  dir.create(path)
  dir.create(file.path(path, "R"))
  dir.create(file.path(path, "src"))
  writeLines(
    c(
      paste0("Package: ", pkgname),
      "Title: Generated package source snapshots",
      "Version: 0.0.0.9000",
      "Description: Temporary package for quickr source snapshots.",
      "License: MIT",
      "Encoding: UTF-8",
      "Imports: quickr"
    ),
    file.path(path, "DESCRIPTION")
  )
  writeLines(
    sprintf("useDynLib(%s, .registration = TRUE)", pkgname),
    file.path(path, "NAMESPACE")
  )
  writeLines(source, file.path(path, "R", "package.R"))
  if (!is.null(init)) {
    writeLines(init, file.path(path, "src", "init.c"))
  }

  pkg_env <- new.env(parent = baseenv())
  quickr:::collector$activate(paste0(pkgname, ":quick_funcs"))
  on.exit(invisible(quickr:::collector$get_collected()), add = TRUE)
  sys.source(file.path(path, "R", "package.R"), envir = pkg_env)
  withr::with_dir(path, suppressMessages(quickr:::dump_collected()))

  list(
    name = pkgname,
    path = path,
    source = source,
    fortran = readLines(file.path(path, "src", "quickr_sub_routines.f90")),
    entrypoints = readLines(file.path(path, "src", "quickr_entrypoints.c"))
  )
}

expect_codegen_package_snapshot <- function(pkg) {
  expect_snapshot({
    writeLines(pkg$source)
    writeLines(pkg$fortran)
    writeLines(pkg$entrypoints)
  })
}

load_codegen_package <- function(pkg, .local_envir = parent.frame()) {
  src <- file.path(pkg$path, "src")
  withr::local_dir(src)
  sources <- c("quickr_sub_routines.f90", "quickr_entrypoints.c")
  if (file.exists("init.c")) {
    sources <- c(sources, "init.c")
  }
  dll_name <- paste0(pkg$name, .Platform$dynlib.ext)
  output <- suppressWarnings(system2(
    R.home("bin/R"),
    c("CMD", "SHLIB", "-o", dll_name, sources),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!is.null(status <- attr(output, "status"))) {
    stop(
      "Compilation failed with status ",
      status,
      ":\n",
      paste(output, collapse = "\n"),
      call. = FALSE
    )
  }
  dll <- dyn.load(file.path(src, dll_name))
  withr::defer(dyn.unload(dll[["path"]]), envir = .local_envir)
  dll
}

test_that("package headers support later RNG and power-shaped entrypoints", {
  pkg <- generate_codegen_package(
    "quickr.snapshot.headers",
    c(
      "declare <- function(...) invisible()",
      "",
      "shift <- quickr::quick(\"shift\", function(x) {",
      "  declare(type(x = double(1)))",
      "  x + 1",
      "})",
      "",
      "draw_square <- quickr::quick(\"draw_square\", function(n) {",
      "  declare(type(n = integer(1)))",
      "  runif(n^2L)",
      "})"
    )
  )
  expect_codegen_package_snapshot(pkg)
  dll <- load_codegen_package(pkg)
  expect_named(
    getDLLRegisteredRoutines(dll)$.External,
    c("shift_", "draw_square_")
  )
  expect_false(dll[["dynamicLookup"]])
  shift <- getNativeSymbolInfo("shift_", dll)$address
  draw_square <- getNativeSymbolInfo("draw_square_", dll)$address
  expect_identical(.External(shift, 2), 3)
  expect_identical(
    withr::with_seed(42, .External(draw_square, 2L)),
    withr::with_seed(42, runif(4))
  )
})

test_that("package entrypoints retain separate procedures with the same local name", {
  pkg <- generate_codegen_package(
    "quickrsnapshotclosures",
    c(
      "declare <- function(...) invisible()",
      "",
      "first <- quickr::quick(\"first\", function(x) {",
      "  declare(type(x = double(1)))",
      "  f <- function(value) value + 1",
      "  f(x)",
      "})",
      "",
      "second <- quickr::quick(\"second\", function(x) {",
      "  declare(type(x = double(1)))",
      "  f <- function(value) value * 2",
      "  f(x)",
      "})"
    )
  )
  expect_codegen_package_snapshot(pkg)
  dll <- load_codegen_package(pkg)
  first <- getNativeSymbolInfo("first_", dll)$address
  second <- getNativeSymbolInfo("second_", dll)$address
  expect_identical(.External(first, 3), 4)
  expect_identical(.External(second, 3), 6)
})

test_that("packages with their own init receive a registration helper", {
  pkg <- generate_codegen_package(
    "quickrsnapshotinit",
    c(
      "declare <- function(...) invisible()",
      "",
      "shift <- quickr::quick(\"shift\", function(x) {",
      "  declare(type(x = double(1)))",
      "  x + 1",
      "})"
    ),
    init = c(
      "#include <R_ext/Rdynload.h>",
      "void R_init_quickrsnapshotinit_quick_functions(DllInfo *dll);",
      "void R_init_quickrsnapshotinit(DllInfo *dll) {",
      "  R_init_quickrsnapshotinit_quick_functions(dll);",
      "  R_useDynamicSymbols(dll, FALSE);",
      "}"
    )
  )
  expect_codegen_package_snapshot(pkg)
  dll <- load_codegen_package(pkg)
  expect_named(getDLLRegisteredRoutines(dll)$.External, "shift_")
  expect_false(dll[["dynamicLookup"]])
  shift <- getNativeSymbolInfo("shift_", dll)$address
  expect_identical(.External(shift, 2), 3)
})
