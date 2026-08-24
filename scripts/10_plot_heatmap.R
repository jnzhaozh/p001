source(here::here("scripts/01_setup_project.R"))
source(here::here("scripts/09_analyze_heatmap.R"))
# load data ----

results_heatmap <- readRDS(here::here(
  "results/results_heatmap.rds"
))

# plot --------------------------------------------------------------------
(p_heatmap <- results_heatmap |>
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
  ) |>
  ggplot(
    aes(
      x = error_mean,
      y = error_sd
    )
  ) +
  geom_raster(
    aes(fill = equilibrium_prop_mean)
  ) +
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
  scale_x_continuous(
    name = latex2exp::TeX(
      "Perception-error mean $\\mu_{\\epsilon}$"
    )
  ) +
  scale_y_continuous(
    name = latex2exp::TeX(
      "Perception-error standard deviation $\\sigma_{\\epsilon}$"
    )
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    name = "Equilibrium participation",
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = grid::unit(200, "pt"),
      barheight = grid::unit(15, "pt")
    )
  ) +
  coord_cartesian(expand = FALSE) +
  this_theme(base_size = 15))

this_ggsave(p_heatmap, width = 9, height = 9.5)
