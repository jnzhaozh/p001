source(here("scripts/01_setup.R"))

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

results_distribution <- params_distribution %>%
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


plot_distribution <- results_distribution %>%
  mutate(
    error_type = factor(error_type, levels = params_distribution$error_type)
  ) %>%
  ggplot(aes(
    x = effective_threshold,
    color = error_type,
    weight = joint_prob
  )) +
  geom_density(
    linewidth = 1,
    key_glyph = "path",
    bounds = c(0, Inf)
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_color_manual(
    values = c(
      "zero_zero" = "#000000",
      "zero_low" = "#E69F00",
      "zero_high" = "#CC3311",
      "negative_low" = "#009944",
      "positive_low" = "#4477AA"
    ),
    breaks = params_distribution$error_type,
    labels = sprintf(
      "(%g, %g)",
      params_distribution$error_mean,
      params_distribution$error_sd
    )
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1.25, by = 0.25)
  ) +
  labs(
    x = expression("Operational threshold " * hat(theta)[i]),
    y = "Density",
    color = expression(
      "Population-size perception error distribution (" *
        mu[epsilon] *
        "," ~ sigma[epsilon] * ")"
    )
  ) +
  guides(
    color = guide_legend(
      title.position = "left",
      ncol = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.1)
    )
  )

plot_distribution

this_ggsave(plot_distribution, width = 16, height = 18)
