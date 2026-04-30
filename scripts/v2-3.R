source("~/P001/scripts/theme_this.R")
librarian::shelf(tidyverse, truncnorm, furrr, ggplot2, latex2exp, scales)

# -------------------------------------------------------------------------

iterations <- 1:1000
N <- 100

environments <- list(
  non_bias_non_hetero = c(mu = 0, sigma = 0),
  non_bias_low_hetero = c(mu = 0, sigma = 20),
  non_bias_high_hetero = c(mu = 0, sigma = 40),
  neg_bias_low_hetero = c(mu = -15, sigma = 20),
  pos_bias_low_hetero = c(mu = 15, sigma = 20)
)

# -------------------------------------------------------------------------

set.seed(777)
plan(multisession, workers = parallel::detectCores() - 1)

data_dist <- imap_dfr(environments, \(params, type) {
  future_map_dfr(iterations, \(i) {
    theta <- rnorm(N, mean = 0.25, sd = 0.122)
    epsilon <- rtruncnorm(N, a = -N, b = Inf, params["mu"], params["sigma"])
    
    tibble(environment_type = type,
           requirement = theta * (N + epsilon))
  }, .options = furrr_options(seed = TRUE))
})

# -------------------------------------------------------------------------

plot_dist <- data_dist %>%
  mutate(environment_type = fct_inorder(environment_type)) %>%
  ggplot(aes(x = requirement, color = environment_type)) +
  geom_density(linewidth = 1, key_glyph = "path") +
  scale_color_manual(
    values = c("#000000", "#CC3311", "#E69F00", "#009944", "#4477AA"),
    labels = \(x) map(environments[x], \(p) TeX(sprintf("$(%g, %g)$", p[1], p[2])))
  ) +
  labs(
    x = expression("Perceived Activation Requirement " * tilde(n)[i]),
    y = "Density",
    color = expression("Information Environment (" * mu[epsilon] * "," ~ sigma[epsilon] * ")")
  ) +
  theme_this


list(plot_dist = plot_dist) %>%
  purrr::iwalk(
    ~ ggsave(
      filename = file.path("~/P001/images/", paste0(.y, ".pdf")),
      plot = .x,
      width = 8,
      height = 6,
      units = "in",
      dpi = 300,
      device = cairo_pdf
    )
  )
