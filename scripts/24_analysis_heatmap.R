source(here("scripts/01_setup.R"))

# -------------------------------------------------------------------------

population_size <- 100
iteration <- 500

params_heatmap <- expand_grid(
  error_mean = seq(-50, 50, by = 1),
  error_sd = seq(0, 100, by = 1),
  threshold_mean = c(20, 25, 30),
  threshold_sd = c(7.2, 12.2, 17.2)
) %>%
  mutate(
    threshold_mean_prop = threshold_mean / population_size,
    threshold_sd_prop = threshold_sd / population_size
  )


# -------------------------------------------------------------------------
#
# gc()
# plan(sequential)
# plan(multisession, workers = 10)
#
# results_heatmap <- params_heatmap %>%
#   mutate(param_id = row_number()) %>%
#   future_pmap_dfr(
#     \(
#       error_mean,
#       error_sd,
#       threshold_mean,
#       threshold_sd,
#       threshold_mean_prop,
#       threshold_sd_prop,
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
#         error_mean = error_mean,
#         error_sd = error_sd,
#         threshold_mean = threshold_mean,
#         threshold_sd = threshold_sd,
#         threshold_mean_prop = threshold_mean_prop,
#         threshold_sd_prop = threshold_sd_prop,
#         equilibrium_mean = mean(equilibrium),
#         equilibrium_prop_mean = mean(equilibrium / population_size),
#         cascade_prob = mean(equilibrium >= population_size * 0.5)
#       )
#     },
#     .options = furrr_options(seed = 123),
#     .progress = TRUE
#   )
#
# plan(sequential)
# this_saveRDS(results_heatmap)

# plot --------------------------------------------------------------------

plot_heatmap <- results_heatmap %>%
  mutate(
    threshold_mean_label = factor(
      paste0("mu[theta] == ", sprintf("%.2f", threshold_mean_prop)),
      levels = paste0("mu[theta] == ", sprintf("%.2f", c(0.2, 0.25, 0.3)))
    ),
    threshold_sd_label = factor(
      paste0("sigma[theta] == ", sprintf("%.3f", threshold_sd_prop)),
      levels = paste0(
        "sigma[theta] == ",
        sprintf("%.3f", c(0.072, 0.122, 0.172))
      )
    )
  ) %>%
  ggplot(aes(x = error_mean, y = error_sd)) +
  geom_raster(aes(fill = equilibrium_prop_mean)) +
  geom_contour(
    aes(z = equilibrium_prop_mean),
    color = "white",
    linewidth = 0.25,
    breaks = c(0.25, 0.5, 0.75)
  ) +
  facet_grid(
    rows = vars(threshold_sd_label),
    cols = vars(threshold_mean_label),
    labeller = label_parsed
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = expression("Mean population-size perception error " * mu[epsilon]),
    y = expression(
      "Standard deviation of population-size perception error " * sigma[epsilon]
    ),
    fill = "Equilibrium participation"
  )

plot_heatmap

this_ggsave(plot_heatmap, width = 24, height = 26)
