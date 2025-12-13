scrub_environment <- function(x) {
  gsub("environment: 0x[0-9a-f]+", "environment: 0x0", x)
}

expect_translation_snapshots <- function(fn, name = deparse(substitute(fn)), note = NULL) {
  fn
  fsub <- r2f(fn)
  cwrapper <- make_c_bridge(fsub)

  if (is.null(note)) {
    expect_snapshot(
      {
        fn
        cat(fsub)
        cat(cwrapper)
      },
      transform = scrub_environment
    )
  } else {
    expect_snapshot(
      {
        cat("# Snapshot note: ", note, "\n", sep = "")
        fn
        cat(fsub)
        cat(cwrapper)
      },
      transform = scrub_environment
    )
  }
}

expect_quick_identical <- function(fn, ...) {
  qfn := quick(fn)
  args_list <- rlang::list2(...)
  args_list <- lapply(args_list, function(x) if (!is.list(x)) list(x) else x)

  for (args in args_list) {
    fn_res <- do.call(fn, args)
    qfn_res <- do.call(qfn, args)
    expect_identical(fn_res, qfn_res)
  }
}


expect_quick_equal <- function(fn, ...) {
  qfn := quick(fn)
  args_list <- rlang::list2(...)
  args_list <- lapply(args_list, function(x) if (!is.list(x)) list(x) else x)

  for (args in args_list) {
    fn_res <- do.call(fn, args)
    qfn_res <- do.call(qfn, args)
    expect_equal(fn_res, qfn_res)
    expect_identical(typeof(fn_res), typeof(qfn_res))
  }
}

assign_in_global <- function(...) {
  x <- rlang::dots_list(..., .named = TRUE)
  list2env(x, envir = globalenv())
}

set_seed_and_call <- function(fun, ...) {
  set.seed(1234)
  fun(...)
}
