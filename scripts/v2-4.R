source("~/P001/scripts/theme_this.R")
librarian::shelf(tidyverse,
                 truncnorm,
                 furrr,
                 ggplot2,
                 patchwork,
                 latex2exp,
                 scales)

# -------------------------------------------------------------------------

iterations <- 1:1000
N <- 100
theta <- c(rep(0, 10), rep(0.2, 90)) # 10% Instigators, 90% Followers
epsilon_mu <- seq(-50, 50, by = 1)
epsilon_sigma <- seq(0, 100, by = 1)

# -------------------------------------------------------------------------

analysis_fun <- function(epsilon_mu, epsilon_sigma, theta) {
  epsilon <- truncnorm::rtruncnorm(N, a = -N, b = Inf, epsilon_mu, epsilon_sigma)
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

set.seed(777)
plan(multisession, workers = parallel::detectCores() - 1)

data_epsilon <- expand_grid(epsilon_mu, epsilon_sigma) %>%
  mutate(stats = future_pmap(list(epsilon_mu, epsilon_sigma), \(m, s) {
    results <- map_dbl(iterations, ~ analysis_fun(m, s, theta))
    
    list(activation_mean = mean(results),
         activation_sd = sd(results))
  }, .options = furrr_options(seed = TRUE))) %>%
  unnest_wider(stats)

# -------------------------------------------------------------------------

# Panel A: Noise as a Catalyst (Slices of sigma at fixed mu)
p_sigma <- data_epsilon %>%
  filter(epsilon_mu %in% c(-15, 0, 15)) %>%
  # mutate(mu_label = fct_inorder(as.character(epsilon_mu))) %>%
  mutate(mu_label = factor(epsilon_mu, levels = c("0", "-15", "15"))) %>%
  ggplot(aes(
    x = epsilon_sigma,
    y = activation_mean,
    color = mu_label,
    fill = mu_label
  )) +
  geom_ribbon(aes(
    ymin = pmax(0, activation_mean - activation_sd),
    ymax = pmin(1, activation_mean + activation_sd)
  ),
  alpha = 0.1,
  color = NA) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  scale_color_manual(values = c("#000000", "#009944", "#4477AA")) +
  scale_fill_manual(values = c("#000000", "#009944", "#4477AA")) +
  labs(
    x = expression("Heterogeneity " * (sigma[epsilon])),
    y = "Particilation (%)",
    # title = "A: Noise as a Catalyst",
    color = expression("Systematic Bias " * (mu[epsilon])),
    fill = expression("Systematic Bias " * (mu[epsilon])),
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE),
         fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_this

# Panel B: Bias as a Shifter (Slices of mu at fixed sigma)
p_mu <- data_epsilon %>%
  filter(epsilon_sigma %in% c(0, 20, 40)) %>%
  mutate(sigma_label = fct_inorder(as.character(epsilon_sigma))) %>%
  ggplot(aes(
    x = epsilon_mu,
    y = activation_mean,
    color = sigma_label,
    fill = sigma_label
  )) +
  geom_ribbon(aes(
    ymin = pmax(0, activation_mean - activation_sd),
    ymax = pmin(1, activation_mean + activation_sd)
  ),
  alpha = 0.1,
  color = NA) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  scale_color_manual(values = c("#000000", "#CC3311", "#E69F00")) +
  scale_fill_manual(values = c("#000000", "#CC3311", "#E69F00")) +
  labs(
    x = expression("Systematic Bias " * (mu[epsilon])),
    y = NULL,
    # title = "B: Bias as a Shifter",
    color = expression("Heterogeneity " * (sigma[epsilon])),
    fill = expression("Heterogeneity " * (sigma[epsilon])),
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE),
         fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_this

plot_epsilon <- (p_sigma | p_mu)
# plot_annotation(
#   title = "Mechanisms of Informational Cascades",
#   subtitle = "Analysis of threshold gaps (10% Instigators vs 90% Followers)",
#   theme = theme(plot.title = element_text(face = "bold", size = 16))
# )

# annotate(
#   "text",
#   x = 5,
#   y = 0.15,
#   label = "Stalled",
#   color = "red",
#   fontface = "italic"
# ) +
# annotate(
#   "text",
#   x = 45,
#   y = 0.70,
#   label = "Catalytic Bridge",
#   color = "#009944",
#   fontface = "bold"
# )

list(plot_epsilon = plot_epsilon) %>%
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
