# include("file.R") splices a file's code in place of the call, both when a
# function is compiled and when it runs as plain R.

skip_on_cran()

write_r_file <- function(dir, name, lines) {
  writeLines(lines, file.path(dir, name))
}

local_helper_dir <- function(.local_envir = parent.frame()) {
  dir <- withr::local_tempdir(
    pattern = "quickr-include-",
    .local_envir = .local_envir
  )
  normalizePath(dir, winslash = "/", mustWork = TRUE)
}

test_that("an included file behaves as if its code were written in place", {
  dir <- local_helper_dir()
  write_r_file(
    dir,
    "helpers.R",
    c(
      "gain <- 2.5",
      "clamp <- function(v, lo, hi) {",
      "  if (v < lo) return(lo)",
      "  if (v > hi) return(hi)",
      "  v",
      "}"
    )
  )
  withr::local_dir(dir)

  fn <- function(x) {
    declare(type(x = double(NA)))
    include("helpers.R")
    out <- double(length(x))
    for (i in seq_along(x)) {
      out[i] <- clamp(x[i] * gain, 0, 1)
    }
    out
  }
  expect_quick_identical(fn, list(c(-1, 0.1, 0.3, 2)))
})

test_that("included files can include other files", {
  dir <- local_helper_dir()
  write_r_file(dir, "constants.R", "half <- 0.5")
  write_r_file(
    dir,
    "math.R",
    c(
      "include(\"constants.R\")",
      "lerp <- function(a, b, t) a + (b - a) * t",
      "midpoint <- function(a, b) lerp(a, b, half)"
    )
  )
  withr::local_dir(dir)

  fn <- function(a, b) {
    declare(type(a = double(1)), type(b = double(1)))
    include("math.R")
    midpoint(a, b) + half
  }
  expect_quick_identical(fn, list(1, 3), list(-2, 5))
})

test_that("included helpers can use the function's variables and <<-", {
  dir <- local_helper_dir()
  write_r_file(
    dir,
    "counting.R",
    c(
      "count <- 0L",
      "bump <- function() {",
      "  count <<- count + 1L",
      "  0L",
      "}",
      "scaled <- function(i) x[i] * k"
    )
  )
  withr::local_dir(dir)

  fn <- function(x) {
    declare(type(x = double(NA)))
    k <- 3
    include("counting.R")
    total <- 0
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        bump()
      }
      total <- total + scaled(i)
    }
    total + count
  }
  expect_quick_identical(fn, list(c(1, -2, 3)), list(c(-1, -2)))
})

test_that("include() works in a local function body", {
  dir <- local_helper_dir()
  write_r_file(dir, "kernels.R", "sq <- function(v) v * v")
  withr::local_dir(dir)

  fn <- function(x) {
    declare(type(x = double(1)))
    smooth <- function(v) {
      include("kernels.R")
      sq(v) + 1
    }
    smooth(x)
  }
  expect_quick_identical(fn, list(2), list(-3))
})

test_that("include() paths can be computed and qualified as quickr::include", {
  dir <- local_helper_dir()
  write_r_file(dir, "helpers.R", c("offset <- 10", "add_offset <- function(v) v + offset"))
  helper_dir <- dir

  computed <- function(x) {
    declare(type(x = double(NA)))
    include(file.path(helper_dir, "helpers.R"))
    add_offset(x)
  }
  expect_quick_identical(computed, list(c(1, 2, 3)))

  qualified <- function(x) {
    declare(type(x = double(NA)))
    quickr::include(file.path(helper_dir, "helpers.R"))
    add_offset(x) * 2
  }
  expect_quick_identical(qualified, list(c(1, 2, 3)))
})

test_that("include() must be a top-level statement", {
  dir <- local_helper_dir()
  write_r_file(dir, "helpers.R", "gain <- 2")
  withr::local_dir(dir)
  message <- "include() must be a top-level statement of a function body"

  in_if <- function(x, flag) {
    declare(type(x = double(1)), type(flag = logical(1)))
    if (flag) {
      include("helpers.R")
    }
    x
  }
  expect_error(quick(in_if), message, fixed = TRUE)

  as_value <- function(x) {
    declare(type(x = double(1)))
    y <- include("helpers.R")
    x
  }
  expect_error(quick(as_value), message, fixed = TRUE)

  in_loop <- function(x) {
    declare(type(x = double(1)))
    for (i in 1:2) {
      include("helpers.R")
    }
    x
  }
  expect_error(quick(in_loop), message, fixed = TRUE)
})

test_that("include() paths must be known when the function is compiled", {
  local_only <- function(x) {
    declare(type(x = double(1)))
    helpers_path_local <- "helpers.R"
    include(helpers_path_local)
    x
  }
  expect_error(
    quick(local_only),
    "include() paths are resolved when the function is compiled, so `helpers_path_local` must not depend on the function's arguments or variables",
    fixed = TRUE
  )

  not_a_path <- function(x) {
    declare(type(x = double(1)))
    include(42)
    x
  }
  expect_error(
    quick(not_a_path),
    "include() path must be a single string, not 42",
    fixed = TRUE
  )
})

test_that("missing, unparsable, and cyclic includes are errors", {
  dir <- local_helper_dir()
  write_r_file(dir, "a.R", "include(\"b.R\")")
  write_r_file(dir, "b.R", "include(\"a.R\")")
  write_r_file(dir, "broken.R", "f <- function(")
  withr::local_dir(dir)

  missing_file <- function(x) {
    declare(type(x = double(1)))
    include("nope.R")
    x
  }
  expect_error(
    quick(missing_file),
    "include() file not found: \"nope.R\"",
    fixed = TRUE
  )
  # The uncompiled function reports the same problem.
  expect_error(missing_file(1), "include() file not found: \"nope.R\"", fixed = TRUE)

  cyclic <- function(x) {
    declare(type(x = double(1)))
    include("a.R")
    x
  }
  cycle_message <- "include() cycle: \"a.R\" -> \"b.R\" -> \"a.R\""
  expect_error(quick(cyclic), cycle_message, fixed = TRUE)
  expect_error(cyclic(1), cycle_message, fixed = TRUE)

  unparsable <- function(x) {
    declare(type(x = double(1)))
    include("broken.R")
    x
  }
  expect_error(
    quick(unparsable),
    "could not parse included file \"broken.R\"",
    fixed = TRUE
  )
})

test_that("an undefined variable names the function it is used in", {
  dir <- local_helper_dir()
  write_r_file(dir, "helpers.R", "clamp_hi <- function(v) min(v, limit)")
  withr::local_dir(dir)

  no_gain <- function(x) {
    declare(type(x = double(NA)))
    sum(x) * gain
  }
  expect_error(
    quick(no_gain),
    "`gain` is not defined in this function (`no_gain`)",
    fixed = TRUE
  )

  in_closure <- function(x) {
    declare(type(x = double(1)))
    scale_it <- function(v) v * factor_k
    scale_it(x)
  }
  expect_error(
    quick(in_closure),
    "`factor_k` is not defined in this function (`scale_it`)",
    fixed = TRUE
  )

  in_anonymous <- function(x) {
    declare(type(x = double(NA)))
    out <- sapply(seq_along(x), function(i) x[i] * factor_k)
    out
  }
  expect_error(
    quick(in_anonymous),
    "`factor_k` is not defined in this function (an anonymous function in `in_anonymous`)",
    fixed = TRUE
  )

  # Included code gets the same check.
  included <- function(x) {
    declare(type(x = double(1)))
    include("helpers.R")
    clamp_hi(x)
  }
  expect_error(
    quick(included),
    "`limit` is not defined in this function (`clamp_hi`)",
    fixed = TRUE
  )
})

test_that("include() works in a package compiled by pkgload::load_all()", {
  skip_if_not_installed("pkgload")

  pkgname <- "quickr.include.pkg"
  pkgdir <- local_helper_dir()
  pkgpath <- file.path(pkgdir, pkgname)
  dir.create(file.path(pkgpath, "R"), recursive = TRUE)
  dir.create(file.path(pkgpath, "inst", "quickr"), recursive = TRUE)

  writeLines(
    c(
      paste0("Package: ", pkgname),
      "Title: quickr include() integration test package",
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
      "export(scale_clamp)",
      "importFrom(quickr,quick)",
      sprintf("useDynLib(%s, .registration = TRUE)", pkgname)
    ),
    file.path(pkgpath, "NAMESPACE")
  )
  write_r_file(
    file.path(pkgpath, "inst", "quickr"),
    "helpers.R",
    c(
      "gain <- 2.5",
      "clamp <- function(v, lo, hi) {",
      "  if (v < lo) return(lo)",
      "  if (v > hi) return(hi)",
      "  v",
      "}"
    )
  )
  write_r_file(
    file.path(pkgpath, "R"),
    "package.R",
    c(
      "scale_clamp <- quickr::quick(\"scale_clamp\", function(x) {",
      "  declare(type(x = double(NA)))",
      "  quickr::include(\"inst/quickr/helpers.R\")",
      "  out <- double(length(x))",
      "  for (i in seq_along(x)) {",
      "    out[i] <- clamp(x[i] * gain, 0, 1)",
      "  }",
      "  out",
      "})"
    )
  )

  withr::defer(try(pkgload::unload(pkgname), silent = TRUE))
  expect_no_error(withr::with_dir(pkgpath, {
    pkgload::load_all(".", quiet = TRUE)
  }))

  fsubs <- readLines(file.path(pkgpath, "src", "quickr_sub_routines.f90"))
  expect_true(any(grepl("subroutine clamp", fsubs, fixed = TRUE)))

  scale_clamp <- get("scale_clamp", envir = asNamespace(pkgname))
  x <- c(-1, 0.1, 0.3, 2)
  expect_identical(scale_clamp(x), pmin(pmax(x * 2.5, 0), 1))
})
