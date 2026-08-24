source(here::here("scripts/01_setup_project.R"))
source(here::here("scripts/02_define_model.R"))

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
