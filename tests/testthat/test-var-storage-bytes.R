test_that("var_storage_bytes() fails fast for unsupported Variable@mode", {
  v <- Variable(mode = "character", dims = list(10L))

  # character is an allowed Variable@mode, but storage sizing must not silently
  # return NULL (which can lead to length-0 logicals in heap-allocation code).
  expect_error(
    quickr:::var_storage_bytes(v),
    "character|unsupported|var_storage_bytes",
    ignore.case = TRUE
  )
})
