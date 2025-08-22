# Example: Heat diffusion simulation

test_that("heat diffusion", {
  # Heat Diffusion Simulation in R
  # 2D grid, explicit time-stepping, fixed boundaries

  # Parameters
  nx <- 100L # Grid size in x
  ny <- 100L # Grid size in y
  dx <- 1L # Grid spacing
  dy <- 1L
  dt <- 0.01 # Time step
  k <- 0.1 # Thermal diffusivity
  steps <- 50L # Number of time steps

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
    temp[nx / 2, ny / 2] <- 100 # Initial heat source in the center

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
          temp_new[i, j] <- temp[i, j] +
            k *
              dt *
              ((temp[i + 1, j] - 2 * temp[i, j] + temp[i - 1, j]) /
                dx^2 +
                (temp[i, j + 1] - 2 * temp[i, j] + temp[i, j - 1]) / dy^2)
        }
      }
      temp <- temp_new
    }

    temp

    # Plot the final temperature distribution
    # image(temp, col = heat.colors(100), main = "Heat Diffusion")
  }

  expect_snapshot(
    {
      diffuse_heat
      cat(fsub <- r2f(diffuse_heat))
      cat(make_c_bridge(fsub))
    },
    transform = scrub_environment
  )

  qdiffuse_heat <- quick(diffuse_heat)

  expect_equal(
    qdiffuse_heat(nx, ny, dx, dy, dt, k, steps),
    diffuse_heat(nx, ny, dx, dy, dt, k, steps)
  )

  if (FALSE) {
    bench::mark(
      qdiffuse_heat(nx, ny, dx, dy, dt, k, steps),
      diffuse_heat(nx, ny, dx, dy, dt, k, steps)
    ) -> res
    res |> print() |> plot()
    summary(res, relative = TRUE) |> print()
  }
})
