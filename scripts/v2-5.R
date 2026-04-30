source("~/P001/scripts/theme_this.R")
librarian::shelf(tidyverse, truncnorm, furrr, scales)

# -------------------------------------------------------------------------

iterations <- 1:1000
N <- 100
epsilon_mu <- seq(-50, 50, by = 1)
epsilon_sigma <- seq(0, 100, by = 1)
theta_mu <- c(0.2, 0.25, 0.3)
theta_sigma <- c(0.072, 0.122, 0.172)

# -------------------------------------------------------------------------

analysis_fun <- function(theta_mu,
                         theta_sigma,
                         epsilon_mu,
                         epsilon_sigma) {
  theta <- rnorm(N, theta_mu, theta_sigma)
  epsilon <- rtruncnorm(N, a = -N, b = Inf, epsilon_mu, epsilon_sigma)
  n_tilde <- theta * (N + epsilon)
  
  current_n <- sum(theta <= 0)
  repeat {
    new_n <- sum(n_tilde <= current_n)
    if (new_n == current_n)
      break
    current_n <- new_n
  }
  
  return(current_n / N)
}

# -------------------------------------------------------------------------

# set.seed(777)
# plan(multisession, workers = parallel::detectCores() - 1)
#
# data_grid <- expand_grid(theta_mu, theta_sigma, epsilon_mu, epsilon_sigma) %>%
#   mutate(stats = future_pmap(
#     list(
#       tm = theta_mu,
#       ts = theta_sigma,
#       em = epsilon_mu,
#       es = epsilon_sigma
#     ),
#     \(tm, ts, em, es) {
#       results <- map_dbl(iterations, ~ analysis_fun(tm, ts, em, es))
#       list(activation_mean = mean(results))
#     },
#     .options = furrr_options(seed = TRUE)
#   )) %>%
#   unnest_wider(stats)

# -------------------------------------------------------------------------

plot_grid <- data_grid %>%
  mutate(
    mu_label = factor(
      theta_mu,
      levels = sort(unique(theta_mu)),
      labels = paste0("mu[theta] == ", sort(unique(theta_mu)))
    ),
    sigma_label = factor(
      theta_sigma,
      levels = sort(unique(theta_sigma)),
      labels = paste0("sigma[theta] == ", sort(unique(theta_sigma)))
    )
  ) %>%
  ggplot(aes(x = epsilon_mu, y = epsilon_sigma)) +
  # geom_contour_filled(aes(z = activation_mean), bins = 5) +
  # facet_grid(sigma_label ~ mu_label, labeller = label_parsed) +
  # scale_fill_viridis_d(option = "plasma", name = "Participation Range") +
  geom_raster(aes(fill = activation_mean), interpolate = TRUE) +
  geom_contour(
    aes(z = activation_mean),
    # breaks = 0.5,
    bins = 4,
    color = "white",
    linewidth = 0.5,
    alpha = 0.8
  ) +
  facet_grid(sigma_label ~ mu_label, labeller = label_parsed) +
  scale_fill_viridis_c(option = "plasma",
                       labels = label_percent(),
                       name = "Participation") +
  labs(
    # title = "Simulation 5: The Global Outcome Landscape",
    # subtitle = "Success regions shift as population composition (theta) changes",
    x = expression("Systematic Bias " * (mu[epsilon])),
    y = expression("Heterogeneity " * (sigma[epsilon]))
  ) +
  theme_this +
  theme(legend.margin = margin(
    t = 5,
    r = 20,
    b = 5,
    l = 10,
    unit = "pt"
  ))



list(plot_grid = plot_grid) %>%
  purrr::iwalk(
    ~ ggsave(
      filename = file.path("~/P001/images/", paste0(.y, ".pdf")),
      plot = .x,
      width = 8,
      height = 7.5,
      units = "in",
      dpi = 300,
      device = cairo_pdf
    )
  )
