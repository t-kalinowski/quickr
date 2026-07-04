# Unit test for package-load side effects

skip_on_cran()

test_that("OMP_CANCELLATION is set on load when unset, respected when preset", {
  withr::with_envvar(c(OMP_CANCELLATION = NA), {
    quickr:::.onLoad()
    expect_identical(Sys.getenv("OMP_CANCELLATION"), "true")
  })
  withr::with_envvar(c(OMP_CANCELLATION = "false"), {
    quickr:::.onLoad()
    expect_identical(Sys.getenv("OMP_CANCELLATION"), "false")
  })
})
