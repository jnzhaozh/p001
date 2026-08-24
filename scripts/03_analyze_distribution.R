source(here::here("scripts/01_setup_project.R"))
source(here::here("scripts/02_define_model.R"))

# -------------------------------------------------------------------------

set.seed(123)

iteration <- 1000
population_size <- 100

params_distribution <- tribble(
  ~error_type    , ~error_mean , ~error_sd , ~threshold_mean , ~threshold_sd ,
  "zero_zero"    ,           0 ,         0 ,              25 , 12.2          ,
  "zero_low"     ,           0 ,        20 ,              25 , 12.2          ,
  "zero_high"    ,           0 ,        40 ,              25 , 12.2          ,
  "negative_low" ,         -15 ,        20 ,              25 , 12.2          ,
  "positive_low" ,          15 ,        20 ,              25 , 12.2
)

results_distribution <- params_distribution |>
  pmap_dfr(\(error_type, error_mean, error_sd, threshold_mean, threshold_sd) {
    get_effective_threshold_distribution(
      threshold_mean = threshold_mean,
      threshold_sd = threshold_sd,
      perception_error_mean = error_mean,
      perception_error_sd = error_sd,
      population_size = population_size
    ) %>%
      mutate(
        error_type = error_type,
        error_mean = error_mean,
        error_sd = error_sd,
        threshold_mean = threshold_mean,
        threshold_sd = threshold_sd
      )
  })

# this_saveRDS(results_distribution)
