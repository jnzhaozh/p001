# get_*_distribution ------------------------------------------------------

get_threshold_distribution <- function(
  threshold_mean,
  threshold_sd,
  population_size
) {
  threshold_range <- 0:population_size

  if (threshold_sd == 0) {
    prob <- as.numeric(threshold_range == threshold_mean)
  } else {
    prob <- dnorm(
      threshold_range,
      mean = threshold_mean,
      sd = threshold_sd
    )
  }

  tibble(
    absolute_threshold = threshold_range,
    absolute_threshold_prob = prob / sum(prob)
  )
}

get_perception_error_distribution <- function(
  perception_error_mean,
  perception_error_sd,
  population_size
) {
  perception_error_range <- (1 - population_size):population_size

  if (perception_error_sd == 0) {
    prob <- as.numeric(perception_error_range == perception_error_mean)
  } else {
    prob <- dnorm(
      perception_error_range,
      mean = perception_error_mean,
      sd = perception_error_sd
    )
  }

  tibble(
    perception_error = perception_error_range,
    perception_error_prob = prob / sum(prob)
  )
}

get_effective_threshold_distribution <- function(
  threshold_mean,
  threshold_sd,
  perception_error_mean,
  perception_error_sd,
  population_size
) {
  threshold_distribution <- get_threshold_distribution(
    threshold_mean = threshold_mean,
    threshold_sd = threshold_sd,
    population_size = population_size
  )

  perception_error_distribution <- get_perception_error_distribution(
    perception_error_mean = perception_error_mean,
    perception_error_sd = perception_error_sd,
    population_size = population_size
  )

  expand_grid(
    threshold_distribution,
    perception_error_distribution
  ) %>%
    mutate(
      joint_prob = absolute_threshold_prob * perception_error_prob,
      proportion_threshold = absolute_threshold / population_size,
      perceived_population_size = population_size + perception_error,
      effective_threshold = proportion_threshold *
        perceived_population_size /
        population_size
    )
}


# draw_* ------------------------------------------------------------------

draw_threshold <- function(
  threshold_mean,
  threshold_sd,
  population_size
) {
  threshold_distribution <- get_threshold_distribution(
    threshold_mean = threshold_mean,
    threshold_sd = threshold_sd,
    population_size = population_size
  )

  sample(
    x = threshold_distribution$absolute_threshold,
    size = population_size,
    replace = TRUE,
    prob = threshold_distribution$absolute_threshold_prob
  )
}


draw_perception_error <- function(
  perception_error_mean,
  perception_error_sd,
  population_size
) {
  perception_error_distribution <- get_perception_error_distribution(
    perception_error_mean = perception_error_mean,
    perception_error_sd = perception_error_sd,
    population_size = population_size
  )

  sample(
    x = perception_error_distribution$perception_error,
    size = population_size,
    replace = TRUE,
    prob = perception_error_distribution$perception_error_prob
  )
}


draw_effective_threshold <- function(
  threshold_mean,
  threshold_sd,
  perception_error_mean,
  perception_error_sd,
  population_size
) {
  absolute_threshold <- draw_threshold(
    threshold_mean = threshold_mean,
    threshold_sd = threshold_sd,
    population_size = population_size
  )

  perception_error <- draw_perception_error(
    perception_error_mean = perception_error_mean,
    perception_error_sd = perception_error_sd,
    population_size = population_size
  )

  proportion_threshold <- absolute_threshold / population_size
  perceived_population_size <- population_size + perception_error
  effective_threshold <- proportion_threshold *
    perceived_population_size /
    population_size

  tibble(
    agent_id = seq_len(population_size),
    absolute_threshold = absolute_threshold,
    proportion_threshold = proportion_threshold,
    perception_error = perception_error,
    perceived_population_size = perceived_population_size,
    effective_threshold = effective_threshold
  )
}


# find_equilibrium --------------------------------------------------------

find_equilibrium <- function(
  thresholds,
  population_size,
  joint_prob = NULL,
  active_initial = 1,
  max_steps = 1000,
  tolerance = 1e-8
) {
  if (is.null(joint_prob)) {
    joint_prob <- rep(1 / length(thresholds), length(thresholds))
  } else {
    joint_prob <- joint_prob / sum(joint_prob)
  }

  active_now <- active_initial

  for (step in seq_len(max_steps)) {
    active_next <- sum(joint_prob[thresholds <= active_now / population_size]) *
      population_size

    if (abs(active_next - active_now) < tolerance) {
      return(active_next)
    }

    active_now <- active_next
  }

  return(active_now)
}
