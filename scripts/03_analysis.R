library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

source("scripts/01_setup.R")
source("scripts/02_model.R")

# -------------------------------------------------------------------------

N <- 100

conditions <- tribble(
  ~epsilon_mean , ~epsilon_sd ,
              0 ,           0 ,
              0 ,          20 ,
              0 ,          40 ,
            -15 ,          20 ,
             15 ,          20
) |>
  mutate(
    theta_mean = 0.25,
    theta_sd = 0.122,
    .before = 1
  )

distributions <- conditions |>
  mutate(
    theta = map2(
      theta_mean,
      theta_sd,
      ~ make_true_thresholds(.x, .y)
    ),
    epsilon = map2(
      epsilon_mean,
      epsilon_sd,
      ~ make_perception_errors(N, .x, .y)
    ),
    hat_theta = map2(
      theta,
      epsilon,
      ~ make_operational_thresholds(N, .x, .y)
    )
  )

# Operational-threshold distributions ----

operational_thresholds <- distributions |>
  rowwise() |>
  reframe(
    epsilon_mean,
    epsilon_sd,
    threshold = seq(1e-6, 1, length.out = 1000),
    density = hat_theta$density(threshold)
  )


# Participation dynamics ----

participation_dynamics <- distributions |>
  rowwise() |>
  reframe(
    epsilon_mean,
    epsilon_sd,
    participation = seq(0, 1, length.out = 1001),
    next_participation = hat_theta$cdf(participation)
  )


# Equilibrium outcomes & Joint effects ----

joint_effects <- crossing(
  theta_mean = c(0.20, 0.25, 0.30),
  theta_sd = c(0.072, 0.122, 0.172),
  epsilon_mean = -50:50,
  epsilon_sd = 0:100
) |>
  group_by(theta_mean, theta_sd) |>
  reframe(
    epsilon_mean,
    epsilon_sd,
    equilibrium = {
      theta <- make_true_thresholds(first(theta_mean), first(theta_sd))

      map2_dbl(
        epsilon_mean,
        epsilon_sd,
        ~ {
          epsilon <- make_perception_errors(N, .x, .y)
          hat_theta <- make_operational_thresholds(N, theta, epsilon)
          find_equilibrium(hat_theta)
        }
      )
    }
  )


# -------------------------------------------------------------------------

analysis_results <- list(
  operational_thresholds = operational_thresholds,
  participation_dynamics = participation_dynamics,
  joint_effects = joint_effects
)

this_saveRDS(analysis_results)
