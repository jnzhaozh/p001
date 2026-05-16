source(here("scripts/01_setup.R"))

# -------------------------------------------------------------------------

population_size <- 100
iteration <- 1000

params_equilibrium <- expand_grid(
  error_mean = c(-15, 0, 15),
  error_sd = seq(0, 50, by = 1)
) %>%
  mutate(
    error_type = case_when(
      error_mean < 0 ~ "underestimate",
      error_mean == 0 ~ "accurate",
      error_mean > 0 ~ "overestimate"
    ),
    threshold_mean = 25,
    threshold_sd = 12.2
  )


# -------------------------------------------------------------------------

results_equilibrium_theoretical <- params_equilibrium %>%
  pmap_dfr(\(error_mean, error_sd, error_type, threshold_mean, threshold_sd) {
    threshold_distribution <- get_effective_threshold_distribution(
      threshold_mean = threshold_mean,
      threshold_sd = threshold_sd,
      perception_error_mean = error_mean,
      perception_error_sd = error_sd,
      population_size = population_size
    )

    equilibrium <- find_equilibrium(
      thresholds = threshold_distribution$effective_threshold,
      joint_prob = threshold_distribution$joint_prob,
      population_size = population_size,
      active_initial = 1
    )

    tibble(
      error_type = error_type,
      error_mean = error_mean,
      error_sd = error_sd,
      threshold_mean = threshold_mean,
      threshold_sd = threshold_sd,
      equilibrium = equilibrium,
      equilibrium_prop = equilibrium / population_size
    )
  })

this_saveRDS(results_equilibrium_theoretical)


# plot --------------------------------------------------------------------

plot_equilibrium_theoretical <- results_equilibrium_theoretical %>%
  mutate(error_mean = factor(error_mean, levels = c(-15, 0, 15))) %>%
  ggplot(aes(
    x = error_sd,
    y = equilibrium,
    color = error_mean,
    group = error_mean
  )) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "-15" = "#009944",
      "0" = "#000000",
      "15" = "#4477AA"
    ),
    labels = c(
      "-15" = "-15",
      "0" = "0",
      "15" = "15"
    )
  ) +
  scale_y_continuous(
    limits = c(0, population_size),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  labs(
    x = expression(
      "Standard deviation of population-size perception error " * sigma[epsilon]
    ),
    y = "Equilibrium participation",
    color = expression("Mean population-size perception error " * mu[epsilon])
  ) +
  guides(
    color = guide_legend(
      title.position = "left",
      ncol = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.1)
    )
  )

plot_equilibrium_theoretical

this_ggsave(plot_equilibrium_theoretical, width = 16, height = 18)

# -------------------------------------------------------------------------

#
# gc()
# plan(sequential)
# plan(multisession, workers = 2)
#
# results_equilibrium_simulation <- params_equilibrium %>%
#   mutate(param_id = row_number()) %>%
#   future_pmap_dfr(
#     \(
#       error_mean,
#       error_sd,
#       error_type,
#       threshold_mean,
#       threshold_sd,
#       param_id
#     ) {
#       equilibrium <- map_dbl(seq_len(iteration), \(i) {
#         threshold_data <- draw_effective_threshold(
#           threshold_mean = threshold_mean,
#           threshold_sd = threshold_sd,
#           perception_error_mean = error_mean,
#           perception_error_sd = error_sd,
#           population_size = population_size
#         )
#
#         find_equilibrium(
#           thresholds = threshold_data$effective_threshold,
#           population_size = population_size,
#           active_initial = 1
#         )
#       })
#
#       tibble(
#         error_type = error_type,
#         error_mean = error_mean,
#         error_sd = error_sd,
#         threshold_mean = threshold_mean,
#         threshold_sd = threshold_sd,
#         equilibrium_mean = mean(equilibrium),
#         equilibrium_low = quantile(equilibrium, 0.025),
#         equilibrium_high = quantile(equilibrium, 0.975),
#         equilibrium_prop_mean = mean(equilibrium / population_size),
#         cascade_prob = mean(equilibrium >= population_size * 0.5)
#       )
#     },
#     .options = furrr_options(seed = 123),
#     .progress = TRUE
#   )
#
# plan(sequential)
# this_saveRDS(results_equilibrium_simulation)
#
#
# plot_equilibrium_simulation <- results_equilibrium_simulation %>%
#   mutate(error_mean = factor(error_mean, levels = c(-15, 0, 15))) %>%
#   ggplot(aes(
#     x = error_sd,
#     y = equilibrium_mean,
#     color = error_mean,
#     group = error_mean
#   )) +
#   geom_ribbon(
#     aes(ymin = equilibrium_low, ymax = equilibrium_high),
#     alpha = 0.15,
#     linewidth = 0,
#     color = NA
#   ) +
#   geom_line(linewidth = 1) +
#   scale_color_manual(
#     values = c(
#       "-15" = "#009944",
#       "0" = "#000000",
#       "15" = "#4477AA"
#     ),
#     labels = c(
#       "-15" = "-15",
#       "0" = "0",
#       "15" = "15"
#     )
#   ) +
#   scale_y_continuous(
#     limits = c(0, population_size),
#     breaks = c(0, 25, 50, 75, 100)
#   ) +
#   labs(
#     x = expression(
#       "Population-size perception-error standard deviation " * sigma[epsilon]
#     ),
#     y = "Equilibrium participation",
#     color = expression("Mean error " * mu[epsilon])
#   )
#
# plot_equilibrium_simulation
