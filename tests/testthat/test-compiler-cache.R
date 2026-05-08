skip_on_cran()

test_that("quickr_cached_r_cmd_config_value reuses successful lookups", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0(name, "-value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "FC-value-1"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "FC-value-1"
  )
  expect_equal(calls, 1L)
})

test_that("quickr_cached_r_cmd_config_value retries failed lookups", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(list(value = "", ok = FALSE))
      }
      list(value = "gfortran", ok = TRUE)
    },
    .package = "quickr"
  )

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    ""
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "gfortran"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "gfortran"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on toolchain env", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )

  withr::local_envvar(FC = "gfortran")
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  withr::local_envvar(FC = "flang")
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on explicit Makevars content", {
  cache <- new.env(parent = emptyenv())
  makevars <- withr::local_tempfile()
  writeLines("FC=gfortran", makevars)
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(R_MAKEVARS_USER = makevars)

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  writeLines("FC=flang", makevars)
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on default Makevars with missing explicit path", {
  cache <- new.env(parent = emptyenv())
  home <- withr::local_tempdir()
  dir.create(file.path(home, ".R"))
  makevars <- file.path(home, ".R", "Makevars")
  writeLines("FC=gfortran", makevars)
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(c(
    HOME = home,
    R_USER = NA,
    R_MAKEVARS_USER = file.path(home, "missing-Makevars")
  ))

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  writeLines("FC=flang", makevars)
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on Makevars-referenced env", {
  cache <- new.env(parent = emptyenv())
  makevars <- withr::local_tempfile()
  writeLines("FC=$(QUICKR_TEST_FC)", makevars)
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(c(
    QUICKR_TEST_FC = "gfortran",
    R_MAKEVARS_USER = makevars
  ))

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  withr::local_envvar(QUICKR_TEST_FC = "flang")
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on Makevars CURDIR", {
  cache <- new.env(parent = emptyenv())
  makevars <- withr::local_tempfile()
  build_a <- withr::local_tempdir()
  build_b <- withr::local_tempdir()
  writeLines("FC=$(CURDIR)", makevars)
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(R_MAKEVARS_USER = makevars)

  withr::with_dir(build_a, {
    expect_identical(
      quickr_cached_r_cmd_config_value("FC", cache = cache),
      "value-1"
    )
  })
  withr::with_dir(build_a, {
    expect_identical(
      quickr_cached_r_cmd_config_value("FC", cache = cache),
      "value-1"
    )
  })

  withr::with_dir(build_b, {
    expect_identical(
      quickr_cached_r_cmd_config_value("FC", cache = cache),
      "value-2"
    )
  })
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on included Makevars content", {
  cache <- new.env(parent = emptyenv())
  makevars <- withr::local_tempfile()
  included <- withr::local_tempfile()
  writeLines(paste("include", included), makevars)
  writeLines("FC=gfortran", included)
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(R_MAKEVARS_USER = makevars)

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  writeLines("FC=flang", included)
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value propagates variables through includes", {
  cache <- new.env(parent = emptyenv())
  root <- withr::local_tempdir()
  makevars <- file.path(root, "Makevars")
  included <- file.path(root, "included.mk")
  nested <- file.path(root, "nested.mk")
  writeLines(
    c(
      paste("DIR =", root),
      "include $(DIR)/included.mk"
    ),
    makevars
  )
  writeLines("include $(DIR)/nested.mk", included)
  writeLines("FC=gfortran", nested)
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(R_MAKEVARS_USER = makevars)

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  writeLines("FC=flang", nested)
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on default HOME Makevars", {
  cache <- new.env(parent = emptyenv())
  home_a <- withr::local_tempdir()
  home_b <- withr::local_tempdir()
  dir.create(file.path(home_a, ".R"))
  dir.create(file.path(home_b, ".R"))
  writeLines("FC=gfortran", file.path(home_a, ".R", "Makevars"))
  writeLines("FC=flang", file.path(home_b, ".R", "Makevars"))
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(c(
    HOME = home_a,
    R_USER = NA,
    R_MAKEVARS_USER = NA
  ))

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  withr::local_envvar(HOME = home_b)
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value uses HOME Makevars with R_USER set", {
  cache <- new.env(parent = emptyenv())
  home <- withr::local_tempdir()
  r_user <- withr::local_tempdir()
  dir.create(file.path(home, ".R"))
  dir.create(file.path(r_user, ".R"))
  makevars <- file.path(home, ".R", "Makevars")
  writeLines("FC=gfortran", makevars)
  writeLines("FC=ignored", file.path(r_user, ".R", "Makevars"))
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(c(
    HOME = home,
    R_USER = r_user,
    R_MAKEVARS_USER = NA
  ))

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  writeLines("FC=flang", makevars)
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on platform Makevars", {
  cache <- new.env(parent = emptyenv())
  home <- withr::local_tempdir()
  dir.create(file.path(home, ".R"))
  makevars <- file.path(home, ".R", paste0("Makevars-", R.version$platform))
  writeLines("FC=gfortran", makevars)
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(c(
    HOME = home,
    R_USER = NA,
    R_MAKEVARS_USER = NA
  ))

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  writeLines("FC=flang", makevars)
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on default site Makevars", {
  cache <- new.env(parent = emptyenv())
  r_home <- withr::local_tempdir()
  site_makevars <- file.path(r_home, "etc", "Makevars.site")
  dir.create(dirname(site_makevars), recursive = TRUE)
  writeLines("FC=gfortran", site_makevars)
  calls <- 0L
  local_mocked_bindings(
    R.home = function(...) r_home,
    .package = "base"
  )
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )
  withr::local_envvar(R_MAKEVARS_SITE = NA)

  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  writeLines("FC=flang", site_makevars)
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_r_cmd_config_value keys on R_ARCH", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name) {
      calls <<- calls + 1L
      list(value = paste0("value-", calls), ok = TRUE)
    },
    .package = "quickr"
  )

  withr::local_envvar(R_ARCH = "/x64")
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  withr::local_envvar(R_ARCH = "/i386")
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_flang_available reuses successful probes", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  local_mocked_bindings(
    quickr_flang_path = function() "/tmp/flang-new",
    quickr_flang_available_at_path = function(flang) {
      calls <<- calls + 1L
      list(path = flang, available = TRUE)
    },
    .package = "quickr"
  )

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_true(result$available)

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_true(result$available)
  expect_equal(calls, 1L)
})

test_that("quickr_cached_flang_available retries failed probes", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  local_mocked_bindings(
    quickr_flang_path = function() "/tmp/flang-new",
    quickr_flang_available_at_path = function(flang) {
      calls <<- calls + 1L
      list(path = flang, available = calls != 1L)
    },
    .package = "quickr"
  )

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_false(result$available)

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_true(result$available)
  expect_equal(calls, 2L)
})

test_that("quickr_cached_flang_available keys on flang path", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  flang <- "/tmp/flang-new"
  local_mocked_bindings(
    quickr_flang_path = function() flang,
    quickr_flang_available_at_path = function(path) {
      calls <<- calls + 1L
      list(path = path, available = TRUE)
    },
    .package = "quickr"
  )

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")

  flang <- "/tmp/flang"
  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang")

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang")
  expect_equal(calls, 2L)
})

test_that("quickr_r_cmd_config_probe reports command failures", {
  calls <- 0L
  system2_stub <- function(command, args, stdout = "", stderr = "", ...) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(structure(" value ", status = 1L))
    }
    "gfortran"
  }

  expect_identical(
    quickr_r_cmd_config_probe(
      "FC",
      r_cmd = "R",
      system2 = system2_stub
    ),
    list(value = "", ok = FALSE)
  )
  expect_identical(
    quickr_r_cmd_config_probe(
      "FC",
      r_cmd = "R",
      system2 = system2_stub
    ),
    list(value = "gfortran", ok = TRUE)
  )
  expect_equal(calls, 2L)
})
