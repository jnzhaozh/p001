source("~/P001/scripts/theme_this.R")
librarian::shelf(tidyverse, truncnorm, furrr, ggplot2, latex2exp, scales)

# -------------------------------------------------------------------------

iterations <- 1:1000
N <- 100
n_range <- seq(0, N, length.out = 101)

environments <- list(
  non_bias_non_hetero = c(mu = 0, sigma = 0),
  non_bias_low_hetero = c(mu = 0, sigma = 20),
  non_bias_high_hetero = c(mu = 0, sigma = 40),
  neg_bias_low_hetero = c(mu = -15, sigma = 20),
  pos_bias_low_hetero = c(mu = 15, sigma = 20)
)

# -------------------------------------------------------------------------

analysis_fun <- function(epsilon_mu, epsilon_sigma, theta) {
  epsilon <- truncnorm::rtruncnorm(N, a = -N, b = Inf, epsilon_mu, epsilon_sigma)
  n_tilde <- theta * (N + epsilon)
  
  n_range %>%
    map_dbl(., ~ mean(n_tilde <= .x))
}


# -------------------------------------------------------------------------

set.seed(777)

data_single <- imap_dfr(environments, \(params, type) {
  map_dfr(iterations, \(i) {
    theta <- 0.5
    
    tibble(
      environment_type = type,
      n_range = n_range,
      activation_prob = analysis_fun(params["mu"], params["sigma"], theta)
    )
  }) %>%
    summarise(
      activation_prob = mean(activation_prob),
      .by = c(environment_type, n_range)
    )
})

# -------------------------------------------------------------------------

plot_single <- data_single %>%
  mutate(environment_type = fct_inorder(environment_type)) %>%
  ggplot(aes(x = n_range, y = activation_prob, color = environment_type)) +
  geom_step(
    data = . %>% filter(environment_type == "non_bias_non_hetero"),
    linewidth = 1
  ) +
  geom_line(
    data = . %>% filter(environment_type != "non_bias_non_hetero"),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c("#000000", "#CC3311", "#E69F00", "#009944", "#4477AA"),
    labels = \(x) map(environments[x], \(p) TeX(sprintf("$(%g, %g)$", p[1], p[2])))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    x = expression("Participation Level " * n[t]),
    y = expression("Activation Probability"),
    color = expression("Information Environment (" * mu[epsilon] * "," ~ sigma[epsilon] * ")")
  ) +
  theme_this

list(plot_single = plot_single) %>%
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
