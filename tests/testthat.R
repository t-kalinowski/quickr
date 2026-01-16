# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

# covr doesn't merge coverage across testthat worker processes, so parallel test
# execution can undercount coverage substantially.
if (identical(Sys.getenv("R_COVR"), "true")) {
  Sys.setenv(TESTTHAT_PARALLEL = "false")
}

library(testthat)
if (identical(Sys.getenv("R_COVR"), "true")) {
  # covr can emit harmless warnings while instrumenting quickr's namespace; keep
  # coverage runs clean.
  suppressWarnings(library(quickr))
} else {
  library(quickr)
}

test_check("quickr")
