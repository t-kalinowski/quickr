test_that("CRAN smoke budget stays small", {
  smoke_files <- list.files(
    test_path(),
    pattern = "^test-cran-smoke-.*\\.R$",
    full.names = TRUE
  )
  lines <- unlist(lapply(smoke_files, readLines), use.names = FALSE)
  quick_entrypoints <- sum(grepl("expect_quick_(equal|identical)\\(", lines))
  quick_entrypoints <- quick_entrypoints + sum(grepl("\\bquick\\(", lines))

  expect_lte(quick_entrypoints, 1L)
})
