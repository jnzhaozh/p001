source(here::here("scripts/01_setup_project.R"))
source(here::here("scripts/02_define_model.R"))

# -------------------------------------------------------------------------
population_size <- 100
n_grid <- 0:population_size

params_equilibrium_mechanism <- tribble(
  ~error_type    , ~error_mean , ~error_sd , ~threshold_mean , ~threshold_sd ,
  "zero_zero"    ,           0 ,         0 ,              25 , 12.2          ,
  "zero_low"     ,           0 ,        20 ,              25 , 12.2          ,
  "zero_high"    ,           0 ,        40 ,              25 , 12.2          ,
  "negative_low" ,         -15 ,        20 ,              25 , 12.2          ,
  "positive_low" ,          15 ,        20 ,              25 , 12.2
)

results_equilibrium_mechanism <- params_equilibrium_mechanism %>%
  pmap_dfr(\(error_type, error_mean, error_sd, threshold_mean, threshold_sd) {
    threshold_distribution <- get_effective_threshold_distribution(
      threshold_mean = threshold_mean,
      threshold_sd = threshold_sd,
      perception_error_mean = error_mean,
      perception_error_sd = error_sd,
      population_size = population_size
    )

    tibble(
      error_type = error_type,
      error_mean = error_mean,
      error_sd = error_sd,
      threshold_mean = threshold_mean,
      threshold_sd = threshold_sd,
      n_current = n_grid,
      n_next = map_dbl(
        n_grid,
        \(n) {
          sum(
            threshold_distribution$joint_prob[
              threshold_distribution$effective_threshold <= n / population_size
            ]
          ) *
            population_size
        }
      )
    )
  })

# this_saveRDS(results_equilibrium_mechanism)
