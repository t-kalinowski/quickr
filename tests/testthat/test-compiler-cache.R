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
    quickr:::quickr_cached_r_cmd_config_value("FC", cache = cache),
    "FC-value-1"
  )
  expect_identical(
    quickr:::quickr_cached_r_cmd_config_value("FC", cache = cache),
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
    quickr:::quickr_cached_r_cmd_config_value("FC", cache = cache),
    ""
  )
  expect_identical(
    quickr:::quickr_cached_r_cmd_config_value("FC", cache = cache),
    "gfortran"
  )
  expect_identical(
    quickr:::quickr_cached_r_cmd_config_value("FC", cache = cache),
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
    quickr:::quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )
  expect_identical(
    quickr:::quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-1"
  )

  withr::local_envvar(FC = "flang")
  expect_identical(
    quickr:::quickr_cached_r_cmd_config_value("FC", cache = cache),
    "value-2"
  )
  expect_identical(
    quickr:::quickr_cached_r_cmd_config_value("FC", cache = cache),
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

  result <- quickr:::quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_true(result$available)

  result <- quickr:::quickr_cached_flang_available(cache = cache)
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

  result <- quickr:::quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_false(result$available)

  result <- quickr:::quickr_cached_flang_available(cache = cache)
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

  result <- quickr:::quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")

  flang <- "/tmp/flang"
  result <- quickr:::quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang")

  result <- quickr:::quickr_cached_flang_available(cache = cache)
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
    quickr:::quickr_r_cmd_config_probe(
      "FC",
      r_cmd = "R",
      system2 = system2_stub
    ),
    list(value = "", ok = FALSE)
  )
  expect_identical(
    quickr:::quickr_r_cmd_config_probe(
      "FC",
      r_cmd = "R",
      system2 = system2_stub
    ),
    list(value = "gfortran", ok = TRUE)
  )
  expect_equal(calls, 2L)
})
