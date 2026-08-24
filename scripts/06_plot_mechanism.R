source(here::here("scripts/01_setup_project.R"))
source(here::here("scripts/05_analyze_mechanism.R"))
# load data ----

results_equilibrium_mechanism <- readRDS(here::here(
  "results/results_equilibrium_mechanism.rds"
))

# -------------------------------------------------------------------------

p_main <- results_equilibrium_mechanism |>
  mutate(
    error_type = factor(
      error_type,
      levels = params_equilibrium_mechanism$error_type
    )
  ) |>
  ggplot(
    aes(
      x = n_current,
      y = n_next,
      color = error_type
    )
  ) +
  annotate(
    "segment",
    x = 0,
    y = 0,
    xend = population_size,
    yend = population_size,
    linewidth = 1,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    name = latex2exp::TeX(
      "Participation $n_t$"
    ),
    limits = c(0, population_size),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  scale_y_continuous(
    name = latex2exp::TeX(
      "Participation $n_{t+1}$"
    ),
    limits = c(0, population_size),
    breaks = c(0, 25, 50, 75, 100)
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
    breaks = params_equilibrium_mechanism$error_type,
    labels = sprintf(
      "(%g, %g)",
      params_equilibrium_mechanism$error_mean,
      params_equilibrium_mechanism$error_sd
    )
  ) +
  this_theme(
    base_size = 15
  )


p_zoom <- results_equilibrium_mechanism |>
  mutate(
    error_type = factor(
      error_type,
      levels = params_equilibrium_mechanism$error_type
    )
  ) |>
  ggplot(
    aes(
      x = n_current,
      y = n_next,
      color = error_type
    )
  ) +
  annotate(
    "segment",
    x = 0,
    y = 0,
    xend = population_size,
    yend = population_size,
    linewidth = 1,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    name = NULL,
    breaks = c(0, 25)
  ) +
  scale_y_continuous(
    name = NULL,
    breaks = c(0, 25)
  ) +
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
  coord_fixed(
    xlim = c(0, 25),
    ylim = c(0, 25)
  ) +
  this_theme(
    base_size = 15
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(4, 4, 4, 4)
  )

# inset_size <- 0.4
# inset_margin <- 0.075
#
# (p_mechanism <- p_main +
#   patchwork::inset_element(
#     p_zoom,
#     left = 1 - inset_margin - inset_size,
#     bottom = inset_margin,
#     right = 1 - inset_margin,
#     top = inset_margin + inset_size,
#     align_to = "panel"
#   ))

(p_mechanism <- cowplot::ggdraw(p_main) +
  cowplot::draw_plot(
    p_zoom,
    x = 0.525,
    y = 0.15,
    width = 0.4,
    height = 0.4
  ))

this_ggsave(p_mechanism, width = 9, height = 8)
