# Example: Viterbi algorithm

test_that("viterbi", {
  viterbi <- function(
    observations,
    states,
    initial_probs,
    transition_probs,
    emission_probs
  ) {
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
    backpointer <- matrix(
      0L,
      nrow = length(states),
      ncol = length(observations)
    )

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
        trellis[current_state, step] <- max(probabilities) *
          emission_probs[current_state, observations[step]]
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
  }

  fsub <- r2f(viterbi)
  cwrapper <- make_c_bridge(fsub)
  expect_snapshot(
    {
      viterbi
      cat(fsub)
      cat(cwrapper)
    },
    transform = scrub_environment
  )

  set.seed(42)

  # Parameters
  num_steps <- 15L
  num_states <- 4L
  num_obs <- 15L

  observations <- sample(1:num_obs, num_steps, replace = TRUE)
  states <- 1:num_states
  initial_probs <- runif(num_states)
  initial_probs <- initial_probs / sum(initial_probs) # Normalize to sum to 1
  transition_probs <- matrix(runif(num_states * num_states), nrow = num_states)
  transition_probs <- transition_probs / rowSums(transition_probs) # Row-normalize
  emission_probs <- matrix(runif(num_states * num_obs), nrow = num_states)
  emission_probs <- emission_probs / rowSums(emission_probs) # Row-normalize

  qviterbi <- quick(viterbi)
  expect_equal(
    viterbi(
      observations,
      states,
      initial_probs,
      transition_probs,
      emission_probs
    ),
    qviterbi(
      observations,
      states,
      initial_probs,
      transition_probs,
      emission_probs
    )
  )
})


#

test_that("viterbi2", {
  viterbi <- function(
    observations,
    states,
    initial_probs,
    transition_probs,
    emission_probs
  ) {
    declare(
      type(observations = integer(num_steps)),
      type(states = integer(num_states)),
      type(initial_probs = double(num_states)),
      type(transition_probs = double(num_states, num_states)),
      type(emission_probs = double(num_states, num_obs)),
    )

    trellis <- matrix(0, nrow = length(states), ncol = length(observations))
    backpointer <- matrix(
      0L,
      nrow = length(states),
      ncol = length(observations)
    )
    trellis[, 1] <- initial_probs * emission_probs[, observations[1]]

    for (step in 2:length(observations)) {
      for (current_state in 1:length(states)) {
        probabilities <- trellis[, step - 1] * transition_probs[, current_state]
        trellis[current_state, step] <- max(probabilities) *
          emission_probs[current_state, observations[step]]
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

  expect_snapshot(
    {
      viterbi
      cat(fsub <- r2f(viterbi))
      cat(make_c_bridge(fsub))
    },
    transform = scrub_environment
  )

  set.seed(1234)

  n <- 2L
  # Parameters
  num_steps <- 16
  num_states <- 8
  num_obs <- 16

  observations <- sample(1:num_obs, num_steps, replace = TRUE)
  states <- 1:num_states
  initial_probs <- runif(num_states)
  initial_probs <- initial_probs / sum(initial_probs) # Normalize to sum to 1
  transition_probs <- matrix(runif(num_states * num_states), nrow = num_states)
  transition_probs <- transition_probs / rowSums(transition_probs) # Row-normalize
  emission_probs <- matrix(runif(num_states * num_obs), nrow = num_states)
  emission_probs <- emission_probs / rowSums(emission_probs) # Row-normalize

  qviterbi <- quick(viterbi)
  expect_equal(
    viterbi(
      observations,
      states,
      initial_probs,
      transition_probs,
      emission_probs
    ),
    qviterbi(
      observations,
      states,
      initial_probs,
      transition_probs,
      emission_probs
    )
  )
  if (FALSE) {
    bench::mark(
      viterbi(
        observations,
        states,
        initial_probs,
        transition_probs,
        emission_probs
      ),
      qviterbi(
        observations,
        states,
        initial_probs,
        transition_probs,
        emission_probs
      )
    ) -> res
    res |> print() |> plot()
    summary(res, relative = TRUE) |> print()
    # viterbi(observations, states, initial_probs, transition_probs, emission_probs)
    # qviterbi(observations, states, initial_probs, transition_probs, emission_probs)
  }
})
