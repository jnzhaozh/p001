library(ggplot2)
library(latex2exp)
library(dplyr)

source("scripts/01_setup.R")

results <- readRDS("results/analysis_results.rds")

# Plot settings ----

condition_levels <- c(
  "(0, 0)",
  "(0, 20)",
  "(0, 40)",
  "(-15, 20)",
  "(15, 20)"
)


condition_colors <- c(
  "(0, 0)" = "#000000",
  "(0, 20)" = "#E69F00",
  "(0, 40)" = "#CC3311",
  "(-15, 20)" = "#009944",
  "(15, 20)" = "#4477AA"
)


# Operational-threshold distributions ----

operational_thresholds <- results$operational_thresholds |>
  mutate(
    condition = factor(
      sprintf("(%g, %g)", epsilon_mean, epsilon_sd),
      levels = condition_levels
    )
  )

(plot_operational_thresholds <- operational_thresholds |>
  ggplot(
    aes(
      x = threshold,
      y = density,
      color = condition
    )
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    name = latex2exp::TeX(
      "Operational threshold $\\hat{\\theta}_i$"
    ),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = c("0", "0.25", "0.5", "0.75", "1"),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    name = "Density",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = latex2exp::TeX(
      "Perception error ($\\mu_{\\epsilon}$, $\\sigma_{\\epsilon}$)"
    ),
    values = condition_colors
  ) +
  this_theme(
    panel.border = element_blank(),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5)
  ))

this_ggsave(plot_operational_thresholds, width = 9, height = 8)

# Participation dynamics ----

participation_dynamics <- results$participation_dynamics |>
  mutate(
    condition = factor(
      sprintf("(%g, %g)", epsilon_mean, epsilon_sd),
      levels = condition_levels
    )
  )

(plot_participation_dynamics <- participation_dynamics |>
  ggplot(
    aes(
      x = participation,
      y = next_participation,
      color = condition
    )
  ) +
  annotate(
    "segment",
    x = 0,
    y = 0,
    xend = 1,
    yend = 1,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.5
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    name = latex2exp::TeX(
      "Participation $\\rho_t$"
    ),
    breaks = seq(0, 1, 0.25),
    labels = c("0", "0.25", "0.5", "0.75", "1"),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    name = latex2exp::TeX(
      "Participation $\\rho_{t+1}$"
    ),
    breaks = seq(0.25, 1, 0.25),
    labels = c("0.25", "0.5", "0.75", "1"),
    expand = expansion(mult = 0)
  ) +
  scale_color_manual(
    name = latex2exp::TeX(
      "Perception error ($\\mu_{\\epsilon}$, $\\sigma_{\\epsilon}$)"
    ),
    values = condition_colors
  ) +
  coord_equal() +
  this_theme(
    panel.border = element_blank(),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5)
  ))

this_ggsave(plot_participation_dynamics, width = 9, height = 8)

# Equilibrium outcomes ----

equilibrium_outcomes <- results$joint_effects |>
  filter(
    theta_mean == 0.25,
    theta_sd == 0.122,
    epsilon_mean %in% c(-15, 0, 15),
    epsilon_sd <= 50
  ) |>
  mutate(
    epsilon_mean = factor(
      epsilon_mean,
      levels = c(-15, 0, 15)
    )
  )

(plot_equilibrium_outcomes <- equilibrium_outcomes |>
  ggplot(
    aes(
      x = epsilon_sd,
      y = equilibrium,
      color = epsilon_mean
    )
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    name = latex2exp::TeX(
      "Perception-error standard deviation $\\sigma_{\\epsilon}$"
    ),
    breaks = seq(0, 50, 10),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    name = "Equilibrium participation",
    breaks = seq(0.25, 1, 0.25),
    labels = c("0.25", "0.5", "0.75", "1"),
    expand = expansion(mult = 0)
  ) +
  scale_color_manual(
    name = latex2exp::TeX(
      "Perception-error mean $\\mu_{\\epsilon}$"
    ),
    values = c(
      "-15" = "#009944",
      "0" = "#000000",
      "15" = "#4477AA"
    )
  ) +
  coord_cartesian(
    ylim = c(0, 1.002),
    clip = "off"
  ) +
  this_theme(
    panel.border = element_blank(),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5)
  ))

this_ggsave(plot_equilibrium_outcomes, width = 9, height = 8)

# Joint effects ----

joint_effects <- results$joint_effects |>
  mutate(
    theta_mean = factor(
      theta_mean,
      levels = c(0.20, 0.25, 0.30),
      labels = c(
        "mu[theta] == 0.20",
        "mu[theta] == 0.25",
        "mu[theta] == 0.30"
      )
    ),
    theta_sd = factor(
      theta_sd,
      levels = c(0.072, 0.122, 0.172),
      labels = c(
        "sigma[theta] == 0.072",
        "sigma[theta] == 0.122",
        "sigma[theta] == 0.172"
      )
    )
  )

(plot_joint_effects <- joint_effects |>
  ggplot(
    aes(
      x = epsilon_mean,
      y = epsilon_sd,
      fill = equilibrium
    )
  ) +
  geom_raster() +
  facet_grid(
    theta_sd ~ theta_mean,
    labeller = label_parsed
  ) +
  scale_x_continuous(
    name = latex2exp::TeX(
      "Perception-error mean $\\mu_{\\epsilon}$"
    ),
    breaks = c(-50, -25, 0, 25, 50)
  ) +
  scale_y_continuous(
    name = latex2exp::TeX(
      "Perception-error standard deviation $\\sigma_{\\epsilon}$"
    ),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  scale_fill_gradient(
    name = "Equilibrium participation",
    low = "#E8EEF5",
    high = "#4477AA",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.5),
    labels = c("0", "0.5", "1"),
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = grid::unit(150, "pt"),
      barheight = grid::unit(15, "pt")
    )
  ) +
  coord_cartesian(expand = FALSE) +
  this_theme())

this_ggsave(plot_joint_effects, width = 9, height = 9.5)
