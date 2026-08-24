source(here::here("scripts/01_setup_project.R"))
source(here::here("scripts/03_analyze_distribution.R"))
# load data ----

results_distribution <- readRDS(here::here(
  "results/results_distribution.rds"
))

# -------------------------------------------------------------------------
(p_distribution <- results_distribution |>
  mutate(
    error_type = factor(
      error_type,
      levels = params_distribution$error_type
    )
  ) |>
  ggplot(
    aes(
      x = effective_threshold,
      color = error_type,
      weight = joint_prob
    )
  ) +
  geom_density(
    linewidth = 0.8,
    key_glyph = "path",
    bounds = c(0, Inf)
  ) +
  scale_x_continuous(
    name = latex2exp::TeX(
      "Operational threshold $\\hat{\\theta}_i$"
    ),
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1.25, by = 0.25)
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_color_manual(
    name = latex2exp::TeX(
      "Perception error ($\\mu_{\\epsilon}$, $\\sigma_{\\epsilon}$)"
    ),
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
  coord_cartesian(xlim = c(0, 1)) +
  this_theme(
    base_size = 15
  ))

this_ggsave(p_distribution, width = 9, height = 8)
