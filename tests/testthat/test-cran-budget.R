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
  always_run_files <- c(
    "test-bind.R",
    "test-blas-guards.R",
    "test-block-scopes.R",
    "test-conformability-grid.R",
    "test-declare-type.R",
    "test-errors.R",
    "test-ifelse.R",
    "test-local-closure-optional-args-more.R",
    "test-logical.R",
    "test-loops.R",
    "test-matrix-inference.R",
    "test-matrix-lapack.R",
    "test-matrix-mul.R",
    "test-parallel-declare.R",
    "test-reduction-scalars.R",
    "test-recycling.R",
    "test-r2f-registry.R",
    "test-sapply-closures.R",
    "test-subset-reduction.R"
  )
  test_files <- list.files(
    test_path(),
    pattern = "^test-.*\\.R$",
    full.names = TRUE
  )
  has_top_level_skip <- setNames(
    vapply(
      test_files,
      function(path) {
        lines <- readLines(path, warn = FALSE)
        head_text <- paste(
          lines[seq_len(min(10, length(lines)))],
          collapse = "\n"
        )
        grepl("skip_on_cran()", head_text, fixed = TRUE)
      },
      logical(1)
    ),
    basename(test_files)
  )

  always_run_with_skip <- always_run_files[has_top_level_skip[always_run_files]]
  expect_true(
    length(always_run_with_skip) == 0L,
    info = paste(always_run_with_skip, collapse = ", ")
  )

  non_smoke_files <- test_files[
    !grepl("^test-cran-(budget|smoke-.*)\\.R$", basename(test_files)) &
      !basename(test_files) %in% always_run_files
  ]

  expect_true(
    all(has_top_level_skip[basename(non_smoke_files)]),
    info = paste(
      basename(non_smoke_files)[
        !has_top_level_skip[basename(non_smoke_files)]
      ],
      collapse = ", "
    )
  )
})
