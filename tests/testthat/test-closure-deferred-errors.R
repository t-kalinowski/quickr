closure_error_cases <- list(
  "nonreturn prefix before a complex alternate" = list(
    fn = function(flag) {
      declare(type(flag = logical(1)))
      predicate <- function() {
        draw <- runif(1)
        invalid <- abs()
        later <- runif(2)
        TRUE
      }
      ifelse(flag, predicate(), 1i)
    },
    diagnostic = "abs"
  ),
  "nested closure assignment" = list(
    fn = function(flag) {
      declare(type(flag = logical(1)))
      predicate <- function() {
        draw <- runif(1)
        nested <- function() {
          other <- runif(1)
          abs()
        }
        value <- nested()
        later <- runif(2)
        TRUE
      }
      flag && predicate()
    },
    diagnostic = "abs"
  ),
  "sapply initializer" = list(
    fn = function(flag) {
      declare(type(flag = logical(1)))
      predicate <- function() {
        draw <- runif(1)
        result <- sapply(seq_len(2L), function(i) {
          other <- runif(1)
          abs()
        })
        later <- runif(2)
        TRUE
      }
      flag && predicate()
    },
    diagnostic = "abs"
  ),
  "final stop after a draw" = list(
    fn = function(flag) {
      declare(type(flag = logical(1)))
      predicate <- function() {
        draw <- runif(1)
        stop("sentinel")
      }
      flag && predicate()
    },
    diagnostic = "sentinel"
  )
)

for (case in names(closure_error_cases)) {
  test_that(paste("lazy closure errors retain effects in", case), {
    fn <- closure_error_cases[[case]]$fn
    diagnostic <- closure_error_cases[[case]]$diagnostic
    qfn <- quick(fn)
    withr::local_seed(153)

    set.seed(153)
    expected <- fn(FALSE)
    expected_seed <- .Random.seed
    set.seed(153)
    expect_identical(qfn(FALSE), expected)
    expect_identical(.Random.seed, expected_seed)

    set.seed(153)
    expect_error(fn(TRUE), diagnostic, fixed = TRUE)
    expected_seed <- .Random.seed
    set.seed(153)
    expect_error(qfn(TRUE), diagnostic, fixed = TRUE)
    expect_identical(.Random.seed, expected_seed)
  })
}
