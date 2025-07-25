


test_that("add1", {

  slow_add1 <- function(x) {
    declare(type(x = double(NA)))
    x <- x + 1
    x
  }
  r2f(slow_add1)

  fsub <- new_fortran_subroutine("slow_add1", slow_add1)
  cwrapper <- make_c_bridge(fsub)


  expect_snapshot({ slow_add1; cat(fsub); cat(cwrapper) },
                  transform = scrub_environment)

  quick_add1 <- quick(name = "slow_add1", slow_add1)
  expect_equal(quick_add1(as.double(1:3)), slow_add1(as.double(1:3)))
  expect_equal(quick_add1(c(1, 2, 3)), slow_add1(c(1, 2, 3)))
  expect_equal(quick_add1(1), slow_add1(1))
})


test_that("add2", {

  slow_add2 <- function(x, y) {
    declare(type(x = integer(n)),
            type(y = integer(n)))
    out <- x + y
    out
  }
  # r2f(slow_add2)

  fsub <- new_fortran_subroutine("slow_add2", slow_add2)
  cwrapper <- make_c_bridge(fsub)

  expect_snapshot({ slow_add2; cat(fsub); cat(cwrapper) },
                  transform = scrub_environment)

  x <- 1:3; y <- 4:6
  quick_add2 <- quick(name = "slow_add2", slow_add2)
  expect_equal(quick_add2(x, y), slow_add2(x, y))
})


test_that("convolve", {

  # options("quickr.r2f.debug" = TRUE)
  slow_convolve <- function(a, b) {
    declare(type(a = double(NA)))
    declare(type(b = double(NA)))

    ab <- double(length(a) + length(b) - 1)
    for (i in seq_along(a)) {
      for (j in seq_along(b)) {
        ab[i + j - 1] = ab[i + j - 1] + a[i] * b[j]
      }
    }
    ab
  }

  (fsub <- new_fortran_subroutine("slow_convolve", slow_convolve))
  cwrapper <- make_c_bridge(fsub)


  expect_snapshot({ slow_convolve; cat(fsub); cat(cwrapper) },
                  transform = scrub_environment)

  quick_convolve <- quick(name = "quick_convolve", slow_convolve)
  a <- 1:3; b <- 10:15
  expect_error(quick_convolve(a, b), "must be 'double', not 'integer'", fixed = TRUE)
  a <- as.double(1:3); b <- as.double(10:15)
  expect_equal(quick_convolve(a, b), slow_convolve(a, b))
  a <- as.double(0:3); b <- 1
  expect_equal(quick_convolve(a, b), slow_convolve(a, b))


})


test_that("which.max", {

  fn <- function(a) {
    declare(type(a = double(NA)))

    out <- which.max(a)
    out

  }


  expect_snapshot(r2f(fn))

  qfn <- quick(fn)

  x = sample.int(1000) + runif(1000)

  expect_identical(fn(x), qfn(x))

  # bench::mark(fn(x), {anyNA(x); qfn(x)}) -> r; print(r); plot(r);

  # qfn(-c(1, 2, 3, 2, 1))
  # fn(-c(1, 2, 3, 2, 1))


  # ---------------------

  # now with logical
  fn <- function(a) {
    declare(type(a = logical(NA)))

    out <- which.max(a)
    out
  }

  expect_snapshot(r2f(fn))


  x <- logical(1000)
  x[500] <- TRUE

  qfn <- quick(fn)
  expect_identical(fn(x), qfn(x))

})

#   qfn_find_loc_int <- quick("fn", fn)
#   qfn_find_loc_lgl <- quick("fn", fn)
#
#
#   bench::mark(fn(x), {anyNA(x); qfn(x)}) -> r; print(r); plot(r);
#   bench::mark(fn(x), qfn(x)) -> r; print(r); plot(r);
#   bench::mark(qfn_find_loc_int(x), qfn_find_loc_lgl(x)) -> r; print(r); plot(r);

# })


test_that("matrix", {

  fn <- function(a, b) {
    declare(type(a = integer(1)))
    declare(type(b = integer(1)))

    out <- matrix(0, a, b)
    out
  }

  fsub <- r2f(fn)
  make_c_bridge(fsub)

  (r2f(fn))
  expect_snapshot(r2f(fn))

  qfn <- quick(fn)

  # expect_identical(qfn(3, 4), fn(3, 4))  ## strict = TRUE by default
  # expect_identical(qfn(3L, 4), fn(3L, 4))
  expect_identical(qfn(3L, 4L), fn(3L, 4L))



  fn <- function(val, nc, nr) {
    # declare({
    #   type(val = double(1))
    #   type(a = integer(1))
    #   type(b = integer(1))
    # })
    declare(
      type(val = double(1)),
      type(nc = integer(1)),
      type(nr = integer(1))
    )

    out <- matrix(val, nc, nr)
    out
  }
  qfn <- quick(fn)


  qfn(1.1, 3L, 3L)

  expect_identical(qfn(2.3, 3L, 4L), fn(2.3, 3L, 4L))
  expect_identical(qfn(2.3, 3L, 4L), matrix(2.3, 3L, 4L))
  # bench::mark(fn(2.3, 3, 4), matrix(2.3, 3, 4), qfn(2.3, 3, 4)) -> r; print(r); plot(r)


})


test_that("reuse implicit size", {
  fn <- function(a1, a2) {
    declare(type(a1 = double(n)))
    declare(type(a2 = double(n, n)))
    out <- a1 + a2[1,]
    out
  }

  fsub <- r2f(fn)
  c_wrapper <- make_c_bridge(fsub)
  qfn <- quick(fn)

  expect_snapshot({print(fsub); cat(c_wrapper)})

  n <- 1000
  a1 <- as.double(1:n)
  a2 <- matrix(runif(n), n, n)

  expect_identical(fn(a1, a2), qfn(a1, a2))

  # bench::mark(fn(a1, a2), qfn(a1, a2)) |> print() |> plot()
})





test_that("viterbi", {
  viterbi <- function(observations, states, initial_probs, transition_probs, emission_probs) {
    declare({
      type(observations = integer(num_steps))
      type(states = integer(num_states))
      type(initial_probs = double(num_states))
      type(transition_probs = double(num_states, num_states))
      type(emission_probs = double(num_states, num_obs))
    })

    num_states <- length(states)
    num_steps <- length(observations)

    # Trellis matrices for probabilities and backtracking
    trellis <- matrix(0, nrow = length(states), ncol = length(observations))
    backpointer <- matrix(0L, nrow = length(states), ncol = length(observations))

    # print(backpointer)
    # print(trellis)
    # Fortran("write (*,*) backpointer")

    # Initialization step
    trellis[, 1] <- initial_probs * emission_probs[, observations[1]]

    # print(backpointer)
    # print(trellis)

    # Recursion step
    for (step in 2:num_steps) {
      for (current_state in 1:num_states) {
        probabilities <- trellis[, step - 1] * transition_probs[, current_state]
        trellis[current_state, step] <- max(probabilities) * emission_probs[current_state, observations[step]]
        backpointer[current_state, step] <- which.max(probabilities)
      }
    }

    # print(backpointer)
    # print(trellis)


    # Backtracking to find the most likely path
    path <- integer(length(observations))
    # print(path)
    path[num_steps] <- which.max(trellis[, num_steps])
    # print(path)
    for (step in seq((num_steps - 1), 1)) {
      # print(backpointer[path[step + 1], step + 1])
      path[step] <- backpointer[path[step + 1], step + 1]
      # print(step)
    }

    # print(path)

    # print(states)
    # Return the most likely path
    out <- states[path]
    out
  };

  fsub <- r2f(viterbi)
  cwrapper <- make_c_bridge(fsub)
  expect_snapshot({ viterbi; cat(fsub); cat(cwrapper) },
                  transform = scrub_environment)

  set.seed(42)

  # Parameters
  num_steps <- 15L
  num_states <- 4L
  num_obs <- 15L

  observations <- sample(1:num_obs, num_steps, replace = TRUE)
  states <- 1:num_states
  initial_probs <- runif(num_states)
  initial_probs <- initial_probs / sum(initial_probs)  # Normalize to sum to 1
  transition_probs <- matrix(runif(num_states * num_states), nrow = num_states)
  transition_probs <- transition_probs / rowSums(transition_probs)  # Row-normalize
  emission_probs <- matrix(runif(num_states * num_obs), nrow = num_states)
  emission_probs <- emission_probs / rowSums(emission_probs)  # Row-normalize

  qviterbi <- quick(viterbi)
  expect_equal(
    viterbi(observations, states, initial_probs, transition_probs, emission_probs),
    qviterbi(observations, states, initial_probs, transition_probs, emission_probs)
  )
})



#
test_that("viterbi2", {
  viterbi <- function(observations, states, initial_probs, transition_probs, emission_probs) {
    declare(
      type(observations = integer(num_steps)),
      type(states = integer(num_states)),
      type(initial_probs = double(num_states)),
      type(transition_probs = double(num_states, num_states)),
      type(emission_probs = double(num_states, num_obs)),
    )

    trellis <- matrix(0, nrow = length(states), ncol = length(observations))
    backpointer <- matrix(0L, nrow = length(states), ncol = length(observations))
    trellis[, 1] <- initial_probs * emission_probs[, observations[1]]

    for (step in 2:length(observations)) {
      for (current_state in 1:length(states)) {
        probabilities <- trellis[, step - 1] * transition_probs[, current_state]
        trellis[current_state, step] <- max(probabilities) * emission_probs[current_state, observations[step]]
        backpointer[current_state, step] <- which.max(probabilities)
      }
    }

    path <- integer(length(observations))
    path[length(observations)] <- which.max(trellis[, length(observations)])
    for (step in seq(length(observations) - 1, 1)) {
      path[step] <- backpointer[path[step + 1], step + 1]
    }

    out <- states[path]
    out
  }

  expect_snapshot({ viterbi; cat(fsub <- r2f(viterbi)); cat(make_c_bridge(fsub)) },
                  transform = scrub_environment)


  set.seed(1234)

  n <- 2L
  # Parameters
  num_steps <- 16
  num_states <- 8
  num_obs <- 16

  observations <- sample(1:num_obs, num_steps, replace = TRUE)
  states <- 1:num_states
  initial_probs <- runif(num_states)
  initial_probs <- initial_probs / sum(initial_probs)  # Normalize to sum to 1
  transition_probs <- matrix(runif(num_states * num_states), nrow = num_states)
  transition_probs <- transition_probs / rowSums(transition_probs)  # Row-normalize
  emission_probs <- matrix(runif(num_states * num_obs), nrow = num_states)
  emission_probs <- emission_probs / rowSums(emission_probs)  # Row-normalize

  qviterbi <- quick(viterbi)
  expect_equal(
    viterbi(observations, states, initial_probs, transition_probs, emission_probs),
    qviterbi(observations, states, initial_probs, transition_probs, emission_probs)
  )
  if (FALSE)
  {
    bench::mark(
      viterbi(observations, states, initial_probs, transition_probs, emission_probs),
      qviterbi(observations, states, initial_probs, transition_probs, emission_probs)
    ) -> res; res |> print() |> plot(); summary(res, relative = TRUE) |> print()
    # viterbi(observations, states, initial_probs, transition_probs, emission_probs)
    # qviterbi(observations, states, initial_probs, transition_probs, emission_probs)
  }



})

test_that("heat diffusion", {
  # Heat Diffusion Simulation in R
  # 2D grid, explicit time-stepping, fixed boundaries

  # Parameters
  nx <- 100L       # Grid size in x
  ny <- 100L       # Grid size in y
  dx <- 1L         # Grid spacing
  dy <- 1L
  dt <- 0.01      # Time step
  k <- 0.1        # Thermal diffusivity
  steps <- 50L    # Number of time steps


  diffuse_heat <- function(nx, ny, dx, dy, dt, k, steps) {
    declare(
      type(nx = integer(1)),
      type(ny = integer(1)),
      type(dx = integer(1)),
      type(dy = integer(1)),
      type(dt = double(1)),
      type(k = double(1)),
      type(steps = integer(1))
    )

    # Initialize temperature grid
    temp <- matrix(0, nx, ny)
    temp[nx / 2, ny / 2] <- 100  # Initial heat source in the center


    # Boundary conditions
    # apply_boundary_conditions <- function(temp) {
    #   temp[1, ] <- 0
    #   temp[nx, ] <- 0
    #   temp[, 1] <- 0
    #   temp[, ny] <- 0
    #   temp
    # }
    #
    # # Update step using finite differences
    # update_temperature <- function(temp, k, dx, dy, dt) {
    #   temp_new <- temp
    #   for (i in 2:(nx - 1)) {
    #     for (j in 2:(ny - 1)) {
    #       temp_new[i, j] <- temp[i, j] + k * dt * ((temp[i + 1, j] - 2 * temp[i, j] + temp[i - 1, j]) / dx ^
    #                                                  2 +
    #                                                  (temp[i, j + 1] - 2 * temp[i, j] + temp[i, j - 1]) / dy ^ 2)
    #     }
    #   }
    #   temp_new
    # }

    # Time stepping
    for (step in seq_len(steps)) {
      # temp <- apply_boundary_conditions(temp)
      # temp <- update_temperature(temp, k, dx, dy, dt)

      temp[1, ] <- 0
      temp[nx, ] <- 0
      temp[, 1] <- 0
      temp[, ny] <- 0

      temp_new <- temp
      for (i in 2:(nx - 1)) {
        for (j in 2:(ny - 1)) {
          temp_new[i, j] <- temp[i, j] + k * dt *
            ((temp[i + 1, j] - 2 * temp[i, j] + temp[i - 1, j]) /
               dx ^ 2 + (temp[i, j + 1] - 2 * temp[i, j] + temp[i, j - 1]) / dy ^ 2)
        }
      }
      temp <- temp_new

    }

    temp

    # Plot the final temperature distribution
    # image(temp, col = heat.colors(100), main = "Heat Diffusion")
  }


  expect_snapshot({ diffuse_heat; cat(fsub <- r2f(diffuse_heat)); cat(make_c_bridge(fsub)) },
                  transform = scrub_environment)


  qdiffuse_heat <- quick(diffuse_heat)

  expect_equal(qdiffuse_heat(nx, ny, dx, dy, dt, k, steps),
               diffuse_heat(nx, ny, dx, dy, dt, k, steps))


  if (FALSE) {

  bench::mark(qdiffuse_heat(nx, ny, dx, dy, dt, k, steps),
              diffuse_heat(nx, ny, dx, dy, dt, k, steps)) -> res;
  res |> print() |> plot(); summary(res, relative = TRUE) |> print()

  }

})


test_that("hoist mask", {

  # no mask to hoist
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- max(x)
    out
  }

  fsub <- r2f(fn)
  cwrapper <- make_c_bridge(fsub)


  expect_snapshot({ fn; cat(fsub); cat(cwrapper) },
                  transform = scrub_environment)

  x <- runif(100, -10, 10)
  qfn := quick(fn)
  expect_equal(qfn(x), fn(x))


  # mask hoists
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- max(x[x>=0])
    out
  }

  fsub <- r2f(fn)
  cwrapper <- make_c_bridge(fsub)


  expect_snapshot({ fn; cat(fsub); cat(cwrapper) },
                  transform = scrub_environment)

  x <- runif(100, -10, 10)
  qfn := quick(fn)
  expect_equal(qfn(x), fn(x))
  # bench::mark(qfn(x), fn(x), relative = T)

})


test_that("which.max/which.min", {

  fn <- function(lgl1, int1, dbl1) {
    declare(type(lgl1 = logical(NA)))
    declare(type(int1 = integer(NA)))
    declare(type(dbl1 = double(NA)))
    out <- c(
      which.min(lgl1),
      which.min(int1),
      which.min(dbl1),
      which.max(lgl1),
      which.max(int1),
      which.max(dbl1),
      which.max(dbl1[dbl1<0])
    )
    out
  }

  r2f(fn)
  # qfn := quick(fn)
  #
  # lgl1 = sample(c(TRUE, FALSE), 111, TRUE)
  # int1 = sample.int(222)
  # dbl1 = runif (333, -1, 1)
  #
  # # expect_equal(
  # bench::mark(relative=T,
  #   qfn(lgl1, int1, dbl1),
  #   fn(lgl1, int1, dbl1)
  # )

  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(
    lgl1 = sample(c(TRUE, FALSE), 10, TRUE),
    int1 = sample.int(100),
    dbl1 = runif(100, -1, 1)
  ))

})

test_that("roll_mean", {
  slow_roll_mean <- function(x, weights, normalize = TRUE) {
    declare(
      type(x = double(NA)),
      type(weights = double(NA)),
      type(normalize = logical(1))
    )
    out <- double(length(x) - length(weights) + 1)
    n <- length(weights)
    if (normalize)
      weights <- weights/sum(weights)*length(weights)

    for(i in seq_along(out)) {
      out[i] <- sum(x[i:(i+n-1)] * weights) / length(weights)
    }
    out
  }

  x <- dnorm(seq(-3, 3, len = 2000))
  weights <- runif (30)

  expect_translation_snapshots(slow_roll_mean)
  expect_quick_equal(slow_roll_mean, list(x, weights))


})




test_that("between", {
  between <- function(x, left, right) {
    declare({
      type(x = double(n))
      type(left = double(1))
      type(right = double(1))
    })
    out <- x >= left & x <= right
    out
  }

  expect_translation_snapshots(between)
  expect_quick_identical(between, list(x = runif(100), left = .4, right = .6))

})



test_that("size constraint", {
  fn <- function(a, b) {
    declare(type(a = double(n)),
            type(b = double(n+1)))
    a = sum(b)
    a
  }

  fsub <- new_fortran_subroutine("fn", fn)
  cbridge <- make_c_bridge(fsub, strict = TRUE)

  qfn := quick(fn)

  expect_error(
    qfn(1, 2),
    regexp = "length(b) must equal (length(a) + 1), but are 1 and 2",
    fixed = TRUE
  )
  expect_translation_snapshots(fn, "call_size_constraint")
  expect_equal(qfn(1, c(2, 3)), 5)

})


test_that("size constraint", {
  fn <- function(x) {
    declare(type(x = double(1)))
    x <- -0.1 + x
    x
  }

  qfn := quick(fn)

  expect_error(
    qfn(c(1, 2)),
    regexp = "length(x) must be 1, not 2",
    fixed = TRUE
  )

  expect_identical(qfn(3), fn(3))
  expect_identical(qfn(-3), fn(-3))
  expect_identical(qfn(0), fn(0))
  expect_identical(qfn(-0), fn(-0))
})


test_that("logical ops", {
  test_args <- list(
    list(1, 2),
    list(2, 1),
    list(-2, 2),
    list(-2, -2),
    list(3, 3),
    list(4, 1),
    list(1, 4)
  )

  fn <- function(a, b) {
    declare(
      type(a = double(1)),
      type(b = double(1))
    )

    delta <- a - b
    if (delta < 0) {
      delta <- (-1) * delta
    }

    a_gt_b <- a > b
    b_gt_a <- b > a
    delta_lt_3 <- delta <= 3

    out <- (a_gt_b || b_gt_a) && delta_lt_3
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # simpler version of above
  fn <- function(a, b) {
    declare({
      type(a = double(1))
      type(b = double(1))
    })

    delta <- abs(a - b)
    out <- (a != b) & (delta <= 3)
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # even simpler version
  fn <- function(a, b) {
    declare(type(a = double(1)), type(b = double(1)))
    out <- (a != b) && abs(a - b) <= 3
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # vectorized version
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    out <- (a != b) & abs(a - b) <= 3
    out
  }
  expect_translation_snapshots(fn)
  .[a, b] <- .mapply(c, test_args, NULL)
  expect_quick_identical(fn, list(a, b))
})


test_that("double unary intrinsics", {

  double_intrinsics <- c(
    "sin", "cos", "tan",
    "asin", "acos", "atan",
    "sqrt", "exp", "log", "log10",
    "floor", "ceiling",
    "abs"
  )

  for (intr in double_intrinsics) {
    fn <- eval(str2lang(sprintf(
      "function(x) {
         declare(type(x = double(NA)))
         out <- %s(x)
         out
       }",
      intr
    )))

    expect_translation_snapshots(fn)

    x <- switch(
      intr,
      asin  = seq(-1, 1, length.out = 20),
      acos  = seq(-1, 1, length.out = 20),
      sqrt  = seq(0, 10, length.out = 20),
      log   = seq(.1, 10, length.out = 20),
      log10 = seq(.1, 10, length.out = 20),
      exp   = seq(-2, 2, length.out = 20),
      seq(-5, 5, length.out = 20)
    )
    expect_quick_equal(fn, x)
  }
})

test_that("integer unary intrinsics", {

  integer_intrinsics <- c("abs")

  for (intr in integer_intrinsics) {
    fn <- eval(str2lang(sprintf(
      "function(x) {
         declare(type(x = integer(NA)))
         out <- %s(x)
         out
       }", intr)))

    expect_translation_snapshots(fn)

    x <- as.integer(seq(-5, 5, length.out = 20))
    expect_quick_identical(fn, x)
  }
})

test_that("complex unary intrinsics", {

  set.seed(123)
  x <- seq(-5, 5, length.out = 30)
  z <- complex(real = x, imaginary = sample(x))

  complex_intrinsics <- c(
    "sin", "cos", "tan",
    "asin", "acos", "atan",
    "sqrt", "exp", "log", "log10",
    "Re", "Im", "Mod", "Arg", "Conj"
  )

  for (intr in complex_intrinsics) {
    fn <- eval(str2lang(sprintf(
      "function(z) {
         declare(type(z = complex(NA)))
         out <- %s(z)
         out
       }", intr)))

    expect_translation_snapshots(fn)
    expect_quick_equal(fn, z)
  }

})


test_that("ifelse", {
  fn <- function(a, b) {
    declare(
      type(a = integer(n)),
      type(b = integer(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_identical(fn, list(-10:10, integer(21)))

  # double version of above
  fn <- function(a, b) {
    declare(
      type(a = double(n)),
      type(b = double(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_equal(fn, list(seq(-5, 5, length.out = 20), double(20)))

})

test_that("seq", {

  fn <- function(a, b) {
    declare(
      type(a = integer(1)),
      type(b = integer(1))
    )

    out <- sum(seq(a, b))
    out
  }

  expect_quick_identical(fn, list(-1L, 10L), list(10L, -5L))
})


test_that("declare(type()) variants", {

  # exprs in {
  quick_seq <- (function(start, end) {
    declare({
      type(start = integer(1))
      type(end = integer(1))
      type(out = integer(end - start + 1))
    })
    out <- seq(start, end)
    out
  })
  expect_quick_identical(quick_seq, list(1L, 5L))

  # args, with missing trailing arg
  quick_seq <- (function(start, end) {
    declare(
      type(start = integer(1)),
      type(end = integer(1)),
      type(out = integer(end - start + 1)),
    )
    out <- seq(start, end)
    out
  })
  expect_quick_identical(quick_seq, list(1L, 5L))

  # args, without missing trailing arg
  quick_seq <- (function(start, end) {
    declare(
      type(start = integer(1)),
      type(end = integer(1)),
      type(out = integer(end - start + 1))
    )
    out <- seq(start, end)
    out
  })
  expect_quick_identical(quick_seq, list(1L, 5L))

  # seperate declare() calls
  quick_seq <- (function(start, end) {
    declare(type(start = integer(1)))
    declare(type(end = integer(1)))
    declare(type(out = integer(end - start + 1)))
    out <- seq(start, end)
    out
  })
  expect_quick_identical(quick_seq, list(1L, 5L))
})


test_that("repeat/break", {
  inc_to_5 <- function(x) {
    declare(type(x = integer(1)))
    repeat {
      if (x >= 5L) {
        break
      }
      x <- x + 1L
    }
    x
  }

  expect_translation_snapshots(inc_to_5)
  expect_quick_identical(inc_to_5, -1L, 0L, 4L, 5L)
})

test_that("repeat + next", {
  inc_to_5_skip_neg <- function(x) {
    declare(type(x = integer(1)))
    repeat {
      x <- x + 1L
      if (x < 0L) next
      if (x >= 5L) break
    }
    x
  }

  expect_translation_snapshots(inc_to_5_skip_neg)
  expect_quick_identical(inc_to_5_skip_neg, -3L, -1L, 0L, 4L, 5L)
})

test_that("break/for", {
  fn <- function(x) {
    declare(type(x = integer(1)))
    for (i in 1:10) {
      x = x + 1L
      if (x >= 5L) {
        break
      }
    }
    x
  }

  expect_translation_snapshots(fn)
  expect_quick_identical(fn, -1L, 0L, 4L, 5L)
})



test_that("while", {
  fn <- function(x) {
    declare(type(x = integer(1)))
    while(x < 5L) {
      x = x+1L
    }
    x
  }

  expect_translation_snapshots(fn)
  expect_quick_identical(fn, -1L, 0L, 4L, 5L)
})

test_that("while + next", {

  inc_to_5_skip_neg_while <- function(x) {
    declare(type(x = integer(1)))
    while (x < 5L) {
      x <- x + 1L
      if (x < 0L) next
    }
    x
  }

  expect_translation_snapshots(inc_to_5_skip_neg_while)
  expect_quick_identical(inc_to_5_skip_neg_while, -3L, -1L, 0L, 4L, 5L)
})


test_that("while + break", {

  inc_to_5_break_while <- function(x) {
    declare(type(x = integer(1)))
    while (TRUE) {
      if (x >= 5L) break
      x <- x + 1L
    }
    x
  }

  expect_translation_snapshots(inc_to_5_break_while)
  expect_quick_identical(inc_to_5_break_while, -1L, 0L, 4L, 5L)
})

test_that("expr return value", {
  fn <- function(x) {
    declare(type(x = integer(NA)))
    x + 1L
  }

  expect_translation_snapshots(fn)
  expect_quick_identical(fn, 1:10)
})


# -------------------------------------------------------------------------
# Remainder (%%) and integer division (%/%) operators
# -------------------------------------------------------------------------

test_that("%% and %/%", {
  x <- -7:7
  x <- expand.grid(a = x, b = x)
  x <- x[x$b != 0, ]                  # avoid divisor == 0
  x <- as.list(x)

  rem_double <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    a %% b
  }

  expect_translation_snapshots(rem_double)
  expect_quick_equal(rem_double, lapply(x, function(v) v + 0.2))

  rem_int <- function(a, b) {
    declare(type(a = integer(n)), type(b = integer(n)))
    a %% b
  }
  expect_quick_identical(rem_int, x)


  div_double <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    a %/% b
  }

  expect_translation_snapshots(div_double)
  expect_quick_equal(div_double, lapply(x, function(v) v + 0.2))

  div_int <- function(a, b) {
    declare(type(a = integer(n)), type(b = integer(n)))
    a %/% b
  }
  expect_quick_identical(div_int, x)
})

