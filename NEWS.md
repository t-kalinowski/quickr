# quickr (development version)

- Functions can now return multiple arrays in a `list()`, optionally
  named (#49, @mns-nordicals).

# quickr 0.2.0

- Internal utility `r2f()` print method now shows the generated `c_bridge`
  for translated subroutines.

- Added support for `nrow()`, `ncol()` and `dim()` (#21, @mikmart).

- Added support for `runif()` with integration to R's RNG (#22, #45).

- Added support for `while`, `repeat`, `break`, `next`.

- Added support for `%%` and `%/%`.

- Added support for expression return values.

- Fix passing a scalar (rank-0) arg to reduction intrinsics
  (min, max, prod, sum).

- Fixed an issue where `/` might perform integer division if one of the operands
  is an integer type (#33, #41).

- Fixed an issue with dll symbol registration when
  `quick()` is used in a package (#19).

- Fixed segfault encountered on Windows with variable sized arrays.

- Added workaround for cases where the compiler error message might not
  display correctly in RStudio.

- Improved error message when using case-sensitive variable names (#18, #36, #39)

- Added `AGENTS.md` and `scripts/setup_codex.sh` to enable the ChatGPT/Codex agent
  to run tests in a docker container configured without internet access.


# quickr 0.1.0

* Initial CRAN submission.
