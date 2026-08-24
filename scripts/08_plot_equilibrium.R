source(here::here("scripts/01_setup_project.R"))
source(here::here("scripts/07_analyze_equilibrium.R"))
# load data ----

results_equilibrium_theoretical <- readRDS(here::here(
  "results/results_equilibrium_theoretical.rds"
))

# plot --------------------------------------------------------------------
(p_equilibrium_theoretical <- results_equilibrium_theoretical |>
  mutate(
    error_mean = factor(
      error_mean,
      levels = c(-15, 0, 15)
    )
  ) |>
  ggplot(
    aes(
      x = error_sd,
      y = equilibrium,
      color = error_mean
    )
  ) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(
    name = latex2exp::TeX(
      "Perception-error standard deviation $\\sigma_{\\epsilon}$"
    )
  ) +
  scale_y_continuous(
    name = "Equilibrium participation",
    limits = c(0, population_size),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  scale_color_manual(
    values = c(
      "-15" = "#009944",
      "0" = "#000000",
      "15" = "#4477AA"
    ),
    name = latex2exp::TeX(
      "Perception-error mean $\\mu_{\\epsilon}$"
    )
  ) +
  this_theme(
    base_size = 15
  ))

this_ggsave(p_equilibrium_theoretical, width = 9, height = 8)
