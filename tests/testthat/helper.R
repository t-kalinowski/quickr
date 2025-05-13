

scrub_environment <- function(x) {
  gsub("environment: 0x[0-9a-f]+", "environment: 0x0", x)
}

expect_translation_snapshots <- function(fn, name = deparse(substitute(fn))) {
  fn
  fsub <- r2f(fn)
  cwrapper <- make_c_bridge(fsub)

  expect_snapshot({
    fn
    cat(fsub)
    cat(cwrapper)
  }, transform = scrub_environment)

}

expect_quick_identical <- function(fn, ...) {
  qfn := quick(fn)
  args_list <- list(...)
  args_list <- lapply(args_list, function(x) if(!is.list(x)) list(x) else x)

  for (args in args_list)
    expect_identical(do.call(fn, args),
                     do.call(qfn, args))
}

assign_in_global <- function(...) {
  x <- rlang::dots_list(..., .named = TRUE)
  list2env(x, envir = globalenv())
}
