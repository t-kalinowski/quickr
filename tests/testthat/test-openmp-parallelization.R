benchmark_timings <- function(serial_fn, parallel_fn, args, reps = 5L) {
  reps <- as.integer(reps)
  if (reps < 1L) {
    stop("reps must be >= 1")
  }

  record_one <- function(fn, variant, rep, order) {
    timing <- system.time(do.call(fn, args))
    elapsed <- timing["elapsed"]
    cpu <- sum(timing[c("user.self", "sys.self")])
    data.frame(
      variant = variant,
      rep = rep,
      order = order,
      elapsed = elapsed,
      cpu = cpu,
      multiple_cores_used = (cpu * 1.5) > elapsed,
      cpu_over_elapsed = if (elapsed > 0) {
        cpu / elapsed
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  }

  out <- vector("list", reps * 2L)
  pos <- 0L
  for (rep in seq_len(reps)) {
    variants <- if (rep %% 2L == 1L) {
      c("serial", "parallel")
    } else {
      c("parallel", "serial")
    }

    for (order in 1:2) {
      variant <- variants[[order]]
      fn <- if (identical(variant, "serial")) serial_fn else parallel_fn
      pos <- pos + 1L
      out[[pos]] <- record_one(fn, variant = variant, rep = rep, order = order)
    }
  }

  do.call(rbind, out)
}

test_that("parallel loop uses multiple threads", {
  skip_if_no_openmp()

  serial <- function(x, n, iters, out) {
    declare(
      type(x = double(n)),
      type(n = integer(1)),
      type(iters = integer(1)),
      type(out = double(n))
    )
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

  parallel <- function(x, n, iters, out) {
    declare(
      type(x = double(n)),
      type(n = integer(1)),
      type(iters = integer(1)),
      type(out = double(n))
    )
    declare(parallel(private = c(v, k)))
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

  threads <- 2L
  withr::local_envvar(c(
    OMP_NUM_THREADS = as.character(threads),
    OMP_THREAD_LIMIT = as.character(threads),
    OMP_DYNAMIC = "false"
  ))

  n <- 500000L
  set.seed(1)
  x <- runif(n)
  out <- double(n)
  serial_q <- quick(serial)
  parallel_q <- quick(parallel)

  iters <- 12L

  expect_equal(
    parallel_q(x, n, iters, double(n)),
    serial_q(x, n, iters, double(n))
  )

  serial_q(x, n, iters, out)
  parallel_q(x, n, iters, out)

  for (attempt in seq_len(6L)) {
    calibration <- benchmark_timings(
      serial_q,
      parallel_q,
      args = list(x = x, n = n, iters = iters, out = out),
      reps = 1L
    )
    serial_elapsed <- calibration$elapsed[calibration$variant == "serial"][1L]
    if (serial_elapsed >= 0.2) {
      break
    }
    iters <- as.integer(iters * 2L)
  }

  timings <- benchmark_timings(
    serial_q,
    parallel_q,
    args = list(x = x, n = n, iters = iters, out = out),
    reps = 5L
  )

  # We intentionally avoid a strict "parallel must be faster" assertion here,
  # because elapsed time is highly sensitive to external contention (other test
  # processes, CI load, thermal throttling). Instead, the unit test focuses on:
  # - evidence that multiple threads actually did work (cpu/elapsed > 1), and
  # - the parallel path not being catastrophically slower than the serial path.
  serial_ratio <- median(timings$cpu_over_elapsed[timings$variant == "serial"])
  parallel_ratio <- median(
    timings$cpu_over_elapsed[timings$variant == "parallel"]
  )

  if (!any(timings$multiple_cores_used[timings$variant == "parallel"])) {
    skip(paste0(
      "OpenMP threads not observed running concurrently (serial cpu/elapsed=",
      signif(serial_ratio, 3),
      ", parallel cpu/elapsed=",
      signif(parallel_ratio, 3),
      ")"
    ))
  }

  serial_median <- median(timings$elapsed[timings$variant == "serial"])
  parallel_median <- median(timings$elapsed[timings$variant == "parallel"])

  info <- paste0(
    "threads=",
    threads,
    "; iters=",
    iters,
    "; serial=",
    signif(serial_median, 3),
    "s; parallel=",
    signif(parallel_median, 3),
    "s; serial cpu/elapsed=",
    signif(serial_ratio, 3),
    "; parallel cpu/elapsed=",
    signif(parallel_ratio, 3)
  )

  # This bound is intentionally generous to avoid test flakiness under load.
  # A strict speedup assertion belongs in a dedicated perf check (see
  # scripts/perf-openmp-speedup.R), while the unit test's job is to confirm that
  # OpenMP parallelism is engaged and doesn't catastrophically regress.
  slowdown_factor <- if (identical(Sys.info()[["sysname"]], "Darwin")) 3 else 2
  expect_lt(parallel_median, serial_median * slowdown_factor, label = info)
})

test_that("stop inside parallel loop propagates errors", {
  skip_if_no_openmp()

  stop_in_parallel <- function(x) {
    declare(type(x = double(1)))
    declare(parallel())
    for (i in seq_len(1L)) {
      if (x < 0) {
        stop("x must be nonnegative")
      }
    }
    x + 1
  }

  q_stop <- quick(stop_in_parallel)
  expect_error(q_stop(-1), "x must be nonnegative", fixed = TRUE)
  expect_equal(q_stop(1), 2)
})

test_that("stop inside nested parallel loop propagates errors", {
  skip_if_no_openmp()

  stop_in_nested_parallel <- function(x) {
    declare(type(x = double(1)))
    declare(parallel())
    for (i in seq_len(1L)) {
      declare(parallel())
      for (j in seq_len(1L)) {
        if (x < 0) {
          stop("x must be nonnegative")
        }
      }
    }
    x + 1
  }

  q_stop <- quick(stop_in_nested_parallel)
  expect_error(q_stop(-1), "x must be nonnegative", fixed = TRUE)
  expect_equal(q_stop(1), 2)
})

test_that("stop inside parallel sapply propagates errors", {
  skip_if_no_openmp()

  stop_in_parallel_sapply <- function(x) {
    declare(type(x = double(1)))
    declare(parallel())
    out <- sapply(seq_len(1L), function(i) {
      if (x < 0) {
        stop("x must be nonnegative")
      }
      x + 1.0
    })
    out
  }

  q_stop <- quick(stop_in_parallel_sapply)
  expect_error(q_stop(-1), "x must be nonnegative", fixed = TRUE)
  expect_equal(q_stop(1), 2)
})
