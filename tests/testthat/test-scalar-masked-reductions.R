skip_on_cran()

test_that("masked scalar sums and products retain empty identities and modes", {
  inputs <- list(integer = 3L, logical = FALSE, double = 3.5, complex = 2 + 3i)
  for (mode in names(inputs)) {
    fn <- eval(substitute(
      function(x, mask) {
        declare(type(x = MODE(1)), type(mask = logical(1)))
        total <- sum(x[mask])
        product <- prod(x[mask])
        list(total = total, product = product)
      },
      list(MODE = as.name(mode))
    ))
    expect_quick_identical(
      fn,
      list(inputs[[mode]], TRUE),
      list(inputs[[mode]], FALSE)
    )
    expect_translation_snapshots(fn, note = paste("Masked", mode, "scalar"))
  }
})

test_that("masked scalar extrema reject an empty selection", {
  inputs <- list(integer = 3L, logical = TRUE, double = 3.5)
  for (mode in names(inputs)) {
    for (reducer in c("min", "max")) {
      fn <- eval(substitute(
        function(x, mask) {
          declare(type(x = MODE(1)), type(mask = logical(1)))
          REDUCE(x[mask])
        },
        list(MODE = as.name(mode), REDUCE = as.name(reducer))
      ))
      compiled <- quick(fn)
      expect_identical(compiled(inputs[[mode]], TRUE), fn(inputs[[mode]], TRUE))
      expect_error(
        compiled(inputs[[mode]], FALSE),
        "min()/max() of empty inputs are not supported",
        fixed = TRUE
      )
      if (mode == "integer" && reducer == "min") {
        expect_translation_snapshots(fn)
      }
    }
  }
})

test_that("multi-argument extrema ignore empty scalar selections", {
  fn <- function(x, y, mask_x, mask_y) {
    declare(
      type(x = integer(1)),
      type(y = double(1)),
      type(mask_x = logical(1)),
      type(mask_y = logical(1))
    )
    low <- min(x[mask_x], y[mask_y])
    high <- max(x[mask_x], y[mask_y])
    list(low = low, high = high)
  }
  expect_quick_identical(
    fn,
    list(3L, 4.5, TRUE, TRUE),
    list(3L, 4.5, TRUE, FALSE),
    list(3L, 4.5, FALSE, TRUE)
  )
  expect_error(
    quick(fn)(3L, 4.5, FALSE, FALSE),
    "min()/max() of empty inputs are not supported",
    fixed = TRUE
  )
  expect_translation_snapshots(fn)
})

test_that("scalar mask and source expressions are evaluated once", {
  fn <- function(x, mask) {
    declare(type(x = integer(1)), type(mask = logical(1)))
    calls <- 0L
    value <- function() {
      calls <<- calls + 1L
      x
    }
    select <- function() {
      calls <<- calls + 10L
      mask
    }
    total <- sum(c(value())[c(select())] + 2L)
    low <- min(c(value())[c(select())], 10L)
    c(total, low, calls)
  }
  expect_quick_identical(fn, list(3L, TRUE), list(3L, FALSE))
})

test_that("masked scalar reductions run before later argument effects", {
  for (reducer in c("sum", "prod", "min", "max")) {
    fn <- eval(substitute(
      function(x, mask) {
        declare(type(x = integer(1)), type(mask = logical(1)))
        change <- function() {
          x <<- 9L
          mask <<- !mask
          4L
        }
        result <- REDUCE(x[mask], change())
        result
      },
      list(REDUCE = as.name(reducer))
    ))
    expect_quick_identical(fn, list(3L, TRUE), list(3L, FALSE))
    if (reducer %in% c("sum", "min")) {
      expect_translation_snapshots(fn, note = paste(reducer, "before effects"))
    }
  }
})
