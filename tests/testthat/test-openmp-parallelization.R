timed_run <- function(fn, ..., reps = 1L) {
  reps <- as.integer(reps)
  if (reps < 1L) {
    stop("reps must be >= 1")
  }

  runs <- vector("list", reps)
  for (i in seq_len(reps)) {
    start_wall <- Sys.time()
    timing <- system.time(fn(...))
    end_wall <- Sys.time()
    if (all(c("user.self", "sys.self") %in% names(timing))) {
      cpu_fields <- intersect(
        names(timing),
        c("user.self", "sys.self", "user.child", "sys.child")
      )
    } else {
      cpu_fields <- intersect(names(timing), c("user", "system"))
    }
    runs[[i]] <- list(
      elapsed = as.numeric(difftime(end_wall, start_wall, units = "secs")),
      elapsed_system = as.numeric(timing[["elapsed"]] %||% NA_real_),
      cpu = sum(timing[cpu_fields])
    )
  }

  median_or_na <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    median(x, na.rm = TRUE)
  }
  list(
    elapsed = median_or_na(vapply(runs, `[[`, 0, "elapsed")),
    elapsed_system = median_or_na(vapply(runs, `[[`, 0, "elapsed_system")),
    cpu = median_or_na(vapply(runs, `[[`, 0, "cpu"))
  )
}

check_thread_scaling_subprocess <- function(label, n, iters) {
  pkg_path <- normalizePath(
    testthat::test_path("..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
  r_dir <- file.path(pkg_path, "R")
  r_sources <- if (dir.exists(r_dir)) {
    list.files(r_dir, pattern = "\\.[Rr]$", all.files = FALSE)
  } else {
    character()
  }
  load_snippet <- if (
    file.exists(file.path(pkg_path, "DESCRIPTION")) &&
      length(r_sources) > 0
  ) {
    paste(
      "suppressPackageStartupMessages(library(pkgload))",
      sprintf("pkgload::load_all(%s, quiet = TRUE)", shQuote(pkg_path)),
      sep = "; "
    )
  } else {
    "suppressPackageStartupMessages(library(quickr))"
  }
  iter_parallel_line <- paste(
    "iter_parallel <- quick(function(x, n, iters) {",
    "declare(type(x = double(n)), type(n = integer(1)),",
    "type(iters = integer(1)), type(out = double(n)));",
    "out <- double(n); declare(parallel());",
    "for (i in seq_len(n)) {",
    "v <- x[i];",
    "for (k in seq_len(iters)) { v <- v * 1.000001 + sin(v) };",
    "out[i] <- v",
    "}; out })",
    collapse = " "
  )
  make_code <- function(iters) {
    paste(
      load_snippet,
      iter_parallel_line,
      sprintf("n <- %dL", as.integer(n)),
      sprintf("iters <- %dL", as.integer(iters)),
      "set.seed(1)",
      "x <- runif(n)",
      "invisible(iter_parallel(x, n, iters))",
      "timing <- system.time(iter_parallel(x, n, iters))",
      "cpu_fields <- intersect(names(timing), c('user.self', 'sys.self', 'user.child', 'sys.child', 'user', 'system'))",
      "cpu <- sum(timing[cpu_fields])",
      "cat(sprintf('elapsed=%.6f cpu=%.6f\\n', timing[['elapsed']], cpu))",
      sep = "; "
    )
  }

  run_one <- function(threads, iters) {
    code <- make_code(iters)
    out <- system2(
      R.home("bin/R"),
      c("--vanilla", "--slave", "-e", shQuote(code)),
      env = c(
        paste0("OMP_NUM_THREADS=", threads),
        paste0("OMP_THREAD_LIMIT=", threads),
        "OMP_DYNAMIC=false"
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    line <- out[grepl("^elapsed=", out)][1L]
    if (is.na(line)) {
      stop(paste(out, collapse = "\n"))
    }
    parsed <- strcapture(
      "elapsed=([0-9.]+) cpu=([0-9.]+|NA)",
      line,
      data.frame(elapsed = 0, cpu = NA_real_)
    )
    list(elapsed = parsed$elapsed, cpu = parsed$cpu)
  }

  scale_iters <- as.integer(iters)
  for (attempt in seq_len(5L)) {
    two_threads <- run_one(2, scale_iters)
    four_threads <- run_one(4, scale_iters)
    eight_threads <- run_one(8, scale_iters)
    min_elapsed <- min(
      c(two_threads$elapsed, four_threads$elapsed, eight_threads$elapsed)
    )
    if (min_elapsed >= 0.1) {
      break
    }
    scale_iters <- as.integer(scale_iters * 4L)
  }

  thread_info <- paste0(
    label,
    " (iters=",
    scale_iters,
    ")",
    ": threads=2 elapsed=",
    signif(two_threads$elapsed, 3),
    " cpu=",
    signif(two_threads$cpu, 3),
    "; threads=4 elapsed=",
    signif(four_threads$elapsed, 3),
    " cpu=",
    signif(four_threads$cpu, 3),
    "; threads=8 elapsed=",
    signif(eight_threads$elapsed, 3),
    " cpu=",
    signif(eight_threads$cpu, 3)
  )
  expect_lt(
    min(four_threads$elapsed, eight_threads$elapsed),
    two_threads$elapsed * 2,
    label = thread_info
  )

  if (!anyNA(c(two_threads$cpu, four_threads$cpu, eight_threads$cpu))) {
    ratio_two <- two_threads$cpu / two_threads$elapsed
    ratio_four <- four_threads$cpu / four_threads$elapsed
    ratio_eight <- eight_threads$cpu / eight_threads$elapsed

    max_ratio <- max(ratio_two, ratio_four, ratio_eight)
    expect_gt(max_ratio, 1.2, label = thread_info)
  }
}

test_that("parallel loops show parallelism without large slowdowns", {
  skip_if_no_openmp()

  serial <- function(x, n, iters) {
    declare(
      type(x = double(n)),
      type(n = integer(1)),
      type(iters = integer(1)),
      type(out = double(n))
    )
    out <- double(n)
    for (i in seq_len(n)) {
      v <- x[i]
      for (k in seq_len(iters)) {
        v <- sin(v) + cos(v)
        v <- sqrt(v + 1)
      }
      out[i] <- v
    }
    out
  }

  parallel <- function(x, n, iters) {
    declare(
      type(x = double(n)),
      type(n = integer(1)),
      type(iters = integer(1)),
      type(out = double(n))
    )
    out <- double(n)
    declare(parallel())
    for (i in seq_len(n)) {
      v <- x[i]
      for (k in seq_len(iters)) {
        v <- sin(v) + cos(v)
        v <- sqrt(v + 1)
      }
      out[i] <- v
    }
    out
  }

  n <- 500000L
  set.seed(1)
  x <- runif(n)
  serial_q <- quick(serial)
  parallel_q <- quick(parallel)

  iters <- 12L
  serial_q(x, n, iters)
  gc()

  reps <- 1L
  for (attempt in seq_len(5L)) {
    serial_time <- timed_run(serial_q, x, n, iters, reps = reps)
    if (serial_time$elapsed >= 0.1) {
      break
    }
    iters <- as.integer(iters * 4L)
  }
  parallel_time <- withr::with_envvar(
    c(
      OMP_NUM_THREADS = "2",
      OMP_THREAD_LIMIT = "2",
      OMP_DYNAMIC = "false"
    ),
    {
      parallel_q(x, n, iters)
      gc()
      timed_run(parallel_q, x, n, iters, reps = reps)
    }
  )

  info <- paste0(
    "iters=",
    iters,
    "; ",
    "serial elapsed=",
    signif(serial_time$elapsed, 3),
    " (sys=",
    signif(serial_time$elapsed_system, 3),
    ")",
    " cpu=",
    signif(serial_time$cpu, 3),
    "; parallel elapsed=",
    signif(parallel_time$elapsed, 3),
    " (sys=",
    signif(parallel_time$elapsed_system, 3),
    ")",
    " cpu=",
    signif(parallel_time$cpu, 3)
  )

  slowdown_factor <- if (identical(Sys.info()[["sysname"]], "Darwin")) {
    2.5
  } else {
    1.5
  }

  if (!anyNA(c(parallel_time$cpu, serial_time$cpu))) {
    cpu_increase <- parallel_time$cpu > serial_time$cpu * 1.1
    elapsed_improve <- parallel_time$elapsed < serial_time$elapsed * 0.95
    expect_true(cpu_increase || elapsed_improve, label = info)
    expect_lt(
      parallel_time$elapsed,
      serial_time$elapsed * slowdown_factor,
      label = info
    )
  } else {
    expect_lt(
      parallel_time$elapsed,
      serial_time$elapsed * slowdown_factor,
      label = info
    )
  }
})

test_that("openmp responds to OMP_NUM_THREADS across sessions", {
  skip_if_no_openmp()

  check_thread_scaling_subprocess(
    label = "iter-map",
    n = 400000L,
    iters = 80L
  )
})
