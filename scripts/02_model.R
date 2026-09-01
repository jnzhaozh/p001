library(tibble)
library(purrr)

make_true_thresholds <- function(
  theta_mean = 0.25,
  theta_sd = 0.122
) {
  density <- \(x) {
    dnorm(x, theta_mean, theta_sd) *
      (x >= 0 & x <= 1)
  }

  cdf <- \(x) {
    y <- pnorm(x, theta_mean, theta_sd)
    y[x < 0] <- 0
    y[x >= 1] <- 1
    y
  }

  lst(density, cdf)
}


make_perception_errors <- function(
  N = 100,
  epsilon_mean,
  epsilon_sd
) {
  hat_N_mean <- N + epsilon_mean
  sdlog <- sqrt(log1p((epsilon_sd / hat_N_mean)^2))
  meanlog <- log(hat_N_mean) - sdlog^2 / 2

  \(p) exp(meanlog + sdlog * qnorm(p)) - N
}

make_operational_thresholds <- function(
  N = 100,
  theta,
  epsilon
) {
  ratio <- 1 + epsilon(ppoints(1e3, a = 0.5)) / N

  density <- \(x) {
    map_dbl(x, \(z) {
      mean(theta$density(z / ratio) / ratio)
    })
  }

  cdf <- \(x) {
    map_dbl(x, \(z) {
      mean(theta$cdf(z / ratio))
    })
  }

  lst(density, cdf)
}

find_equilibrium <- function(
  hat_theta
) {
  rho <- hat_theta$cdf(0)

  repeat {
    rho_next <- hat_theta$cdf(rho)

    if (rho_next - rho < 1e-6) {
      return(rho_next)
    }

    rho <- rho_next
  }
}

compute_equilibrium <- function(
  N = 100,
  theta_mean,
  theta_sd,
  epsilon_mean,
  epsilon_sd
) {
  theta <- make_true_thresholds(
    theta_mean,
    theta_sd
  )

  epsilon <- make_perception_errors(
    N,
    epsilon_mean,
    epsilon_sd
  )

  hat_theta <- make_operational_thresholds(
    N,
    theta,
    epsilon
  )

  find_equilibrium(hat_theta)
}

# -------------------------------------------------------------------------

# make_operational_thresholds <- function(
#   N,
#   theta,
#   epsilon,
#   n_grid = 1000
# ) {
#   p <- (seq_len(n_grid) - 0.5) / n_grid
#
#   theta_grid <- theta$quantile(p)
#   epsilon_grid <- epsilon$quantile(p)
#
#   hat_theta <- outer(theta_grid, 1 + epsilon_grid / N, "*")
#
#   tibble(
#     hat_theta = as.vector(hat_theta),
#     prob = 1 / n_grid^2
#   )
# }

# -------------------------------------------------------------------------
#
# generate_true_thresholds <- function(
#   N,
#   n_mean,
#   n_sd
# ) {
#   tibble(
#     id = seq_len(N),
#     n = truncnorm::rtruncnorm(
#       N,
#       a = 0,
#       b = N,
#       mean = n_mean,
#       sd = n_sd
#     ),
#     theta = n / N
#   )
# }
#
#
# generate_perception_errors <- function(
#   N,
#   epsilon_mean,
#   epsilon_sd
# ) {
#   epsilon <- if (epsilon_sd == 0) {
#     rep(epsilon_mean, N)
#   } else {
#     truncnorm::rtruncnorm(
#       N,
#       a = -N,
#       mean = epsilon_mean,
#       sd = epsilon_sd
#     )
#   }
#
#   tibble(
#     id = seq_len(N),
#     epsilon = epsilon
#   )
# }
#
# generate_operational_thresholds <- function(
#   theta,
#   epsilon
# ) {
#   N <- nrow(theta)
#
#   left_join(
#     theta,
#     epsilon,
#     by = "id"
#   ) |>
#     mutate(
#       hat_N = N + epsilon,
#       hat_theta = theta * hat_N / N
#     )
# }
#
#
# simulate_participation_dynamics <- function(
#   agents,
#   instigators
# ) {
#   N <- nrow(agents)
#
#   active <- seq_len(N) <= instigators
#   trajectory <- sum(active)
#
#   for (t in seq_len(N)) {
#     n_t <- sum(active)
#
#     active_next <- active | (agents$hat_theta <= n_t / N)
#
#     n_next <- sum(active_next)
#
#     if (n_next == n_t) {
#       break
#     }
#
#     active <- active_next
#     trajectory <- c(trajectory, n_next)
#   }
#
#   tibble(
#     t = seq_along(trajectory) - 1L,
#     n = trajectory
#   )
# }

# # get_*_distribution ------------------------------------------------------
#
# get_threshold_distribution <- function(
#   threshold_mean,
#   threshold_sd,
#   population_size
# ) {
#   threshold_range <- 0:population_size
#
#   if (threshold_sd == 0) {
#     prob <- as.numeric(threshold_range == threshold_mean)
#   } else {
#     prob <- dnorm(
#       threshold_range,
#       mean = threshold_mean,
#       sd = threshold_sd
#     )
#   }
#
#   tibble(
#     absolute_threshold = threshold_range,
#     absolute_threshold_prob = prob / sum(prob)
#   )
# }
#
# get_perception_error_distribution <- function(
#   perception_error_mean,
#   perception_error_sd,
#   population_size
# ) {
#   perception_error_range <- (1 - population_size):population_size
#
#   if (perception_error_sd == 0) {
#     prob <- as.numeric(perception_error_range == perception_error_mean)
#   } else {
#     prob <- dnorm(
#       perception_error_range,
#       mean = perception_error_mean,
#       sd = perception_error_sd
#     )
#   }
#
#   tibble(
#     perception_error = perception_error_range,
#     perception_error_prob = prob / sum(prob)
#   )
# }
#
# get_effective_threshold_distribution <- function(
#   threshold_mean,
#   threshold_sd,
#   perception_error_mean,
#   perception_error_sd,
#   population_size
# ) {
#   threshold_distribution <- get_threshold_distribution(
#     threshold_mean = threshold_mean,
#     threshold_sd = threshold_sd,
#     population_size = population_size
#   )
#
#   perception_error_distribution <- get_perception_error_distribution(
#     perception_error_mean = perception_error_mean,
#     perception_error_sd = perception_error_sd,
#     population_size = population_size
#   )
#
#   expand_grid(
#     threshold_distribution,
#     perception_error_distribution
#   ) %>%
#     mutate(
#       joint_prob = absolute_threshold_prob * perception_error_prob,
#       proportion_threshold = absolute_threshold / population_size,
#       perceived_population_size = population_size + perception_error,
#       effective_threshold = proportion_threshold *
#         perceived_population_size /
#         population_size
#     )
# }
#
#
# # draw_* ------------------------------------------------------------------
#
# draw_threshold <- function(
#   threshold_mean,
#   threshold_sd,
#   population_size
# ) {
#   threshold_distribution <- get_threshold_distribution(
#     threshold_mean = threshold_mean,
#     threshold_sd = threshold_sd,
#     population_size = population_size
#   )
#
#   sample(
#     x = threshold_distribution$absolute_threshold,
#     size = population_size,
#     replace = TRUE,
#     prob = threshold_distribution$absolute_threshold_prob
#   )
# }
#
#
# draw_perception_error <- function(
#   perception_error_mean,
#   perception_error_sd,
#   population_size
# ) {
#   perception_error_distribution <- get_perception_error_distribution(
#     perception_error_mean = perception_error_mean,
#     perception_error_sd = perception_error_sd,
#     population_size = population_size
#   )
#
#   sample(
#     x = perception_error_distribution$perception_error,
#     size = population_size,
#     replace = TRUE,
#     prob = perception_error_distribution$perception_error_prob
#   )
# }
#
#
# draw_effective_threshold <- function(
#   threshold_mean,
#   threshold_sd,
#   perception_error_mean,
#   perception_error_sd,
#   population_size
# ) {
#   absolute_threshold <- draw_threshold(
#     threshold_mean = threshold_mean,
#     threshold_sd = threshold_sd,
#     population_size = population_size
#   )
#
#   perception_error <- draw_perception_error(
#     perception_error_mean = perception_error_mean,
#     perception_error_sd = perception_error_sd,
#     population_size = population_size
#   )
#
#   proportion_threshold <- absolute_threshold / population_size
#   perceived_population_size <- population_size + perception_error
#   effective_threshold <- proportion_threshold *
#     perceived_population_size /
#     population_size
#
#   tibble(
#     agent_id = seq_len(population_size),
#     absolute_threshold = absolute_threshold,
#     proportion_threshold = proportion_threshold,
#     perception_error = perception_error,
#     perceived_population_size = perceived_population_size,
#     effective_threshold = effective_threshold
#   )
# }
#
#
# # find_equilibrium --------------------------------------------------------
#
# find_equilibrium <- function(
#   thresholds,
#   population_size,
#   joint_prob = NULL,
#   active_initial = 1,
#   max_steps = 1000,
#   tolerance = 1e-8
# ) {
#   if (is.null(joint_prob)) {
#     joint_prob <- rep(1 / length(thresholds), length(thresholds))
#   } else {
#     joint_prob <- joint_prob / sum(joint_prob)
#   }
#
#   active_now <- active_initial
#
#   for (step in seq_len(max_steps)) {
#     active_next <- sum(joint_prob[thresholds <= active_now / population_size]) *
#       population_size
#
#     if (abs(active_next - active_now) < tolerance) {
#       return(active_next)
#     }
#
#     active_now <- active_next
#   }
#
#   return(active_now)
# }
