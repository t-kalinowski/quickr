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

test_that("non-smoke test files skip on CRAN", {
  test_files <- list.files(
    test_path(),
    pattern = "^test-.*\\.R$",
    full.names = TRUE
  )
  non_smoke_files <- test_files[
    !grepl("^test-cran-(budget|smoke-.*)\\.R$", basename(test_files))
  ]
  has_top_level_skip <- vapply(
    non_smoke_files,
    function(path) {
      lines <- readLines(path, warn = FALSE)
      head_text <- paste(
        lines[seq_len(min(10, length(lines)))],
        collapse = "\n"
      )
      grepl("skip_on_cran()", head_text, fixed = TRUE)
    },
    logical(1)
  )

  expect_true(
    all(has_top_level_skip),
    info = paste(
      basename(non_smoke_files[!has_top_level_skip]),
      collapse = ", "
    )
  )
})
