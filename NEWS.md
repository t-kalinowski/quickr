# quickr (development version)

- Added support for `while`, `repeat`, `break`, `next`.

- Added support for `%%` and `%/%`.

- Added support for expression return values.

- Fix passing a scalar (rank-0) arg to reduction intrinsics
  (min, max, prod, sum).

- Fixed an issue with dll symbol registration when
  `quick()` is used in a package (#19).

- Added support for `nrow()`, `ncol()` and `dim()` (#21, @mikmart).

# quickr 0.1.0

* Initial CRAN submission.
