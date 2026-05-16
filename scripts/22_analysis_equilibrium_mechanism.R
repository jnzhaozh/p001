source(here("scripts/01_setup.R"))

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


p_main <- results_equilibrium_mechanism %>%
  mutate(
    error_type = factor(
      error_type,
      levels = params_equilibrium_mechanism$error_type
    )
  ) %>%
  ggplot(aes(x = n_current, y = n_next, color = error_type)) +
  annotate(
    "segment",
    x = 0,
    y = 0,
    xend = population_size,
    yend = population_size,
    linewidth = 0.8,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "zero_zero" = "#000000",
      "zero_low" = "#E69F00",
      "zero_high" = "#CC3311",
      "negative_low" = "#009944",
      "positive_low" = "#4477AA"
    ),
    breaks = params_equilibrium_mechanism$error_type,
    labels = sprintf(
      "(%g, %g)",
      params_equilibrium_mechanism$error_mean,
      params_equilibrium_mechanism$error_sd
    )
  ) +
  scale_x_continuous(
    limits = c(0, population_size),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  scale_y_continuous(
    limits = c(0, population_size),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  labs(
    x = expression("Participation at " * t ~ "(" * n[t] * ")"),
    y = expression("Participation at " * t + 1 ~ "(" * n[t + 1] * ")"),
    color = expression(
      "Population-size perception-error distribution (" *
        mu[epsilon] *
        "," ~ sigma[epsilon] *
        ")"
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


p_zoom <- results_equilibrium_mechanism %>%
  ggplot(aes(x = n_current, y = n_next, color = error_type)) +
  annotate(
    "segment",
    x = 0,
    y = 0,
    xend = population_size,
    yend = population_size,
    linewidth = 0.6,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "zero_zero" = "#000000",
      "zero_low" = "#E69F00",
      "zero_high" = "#CC3311",
      "negative_low" = "#009944",
      "positive_low" = "#4477AA"
    ),
    breaks = params_equilibrium_mechanism$error_type
  ) +
  # coord_cartesian(xlim = c(0, 15), ylim = c(0, 15)) +
  coord_fixed(xlim = c(0, 25), ylim = c(0, 25)) +
  scale_x_continuous(breaks = c(0, 25)) +
  scale_y_continuous(breaks = c(0, 25)) +
  labs(x = NULL, y = NULL) +
  theme(
    legend.position = "none",
    plot.background = element_rect(color = "black", linewidth = 0.1),
    plot.margin = margin(4, 4, 4, 4)
  )

inset_size <- 0.45
plot_equilibrium_mechanism <- p_main +
  inset_element(
    p_zoom,
    left = 0.52,
    bottom = 0.07,
    right = 0.52 + inset_size,
    top = 0.07 + inset_size,
    align_to = "panel"
  )

plot_equilibrium_mechanism

this_ggsave(plot_equilibrium_mechanism, width = 16, height = 18)
