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

analysis_fun <- function(epsilon_mu, epsilon_sigma, theta) {
  epsilon <- truncnorm::rtruncnorm(N, a = -N, b = Inf, epsilon_mu, epsilon_sigma)
  n_tilde <- theta * (N + epsilon)
  
  history <- numeric(N + 1)
  current_n <- 0
  step <- 1
  history[step] <- current_n
  
  repeat {
    new_n <- sum(n_tilde <= current_n)
    if (new_n == current_n || step > N)
      break
    step <- step + 1
    current_n <- new_n
    history[step] <- current_n
  }
  
  tibble(time_step = 0:(step - 1),
         participation_level = history[1:step] / N)
}

# -------------------------------------------------------------------------

set.seed(777)
plan(multisession, workers = parallel::detectCores() - 1)

data_temporal <- imap_dfr(environments, \(params, type) {
  future_map_dfr(iterations, \(i) {
    theta <- rnorm(N, mean = 0.25, sd = 0.122)
    
    analysis_fun(params["mu"], params["sigma"], theta) %>%
      complete(time_step = 0:N) %>%
      fill(participation_level, .direction = "down") %>%
      mutate(environment_type = type)
  }, .options = furrr_options(seed = TRUE)) %>%
    summarise(
      participation_mean = mean(participation_level),
      .by = c(environment_type, time_step)
    )
})

# -------------------------------------------------------------------------

plot_temporal <- data_temporal %>%
  mutate(environment_type = fct_inorder(environment_type)) %>%
  ggplot(aes(x = time_step, y = participation_mean, color = environment_type)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("#000000", "#CC3311", "#E69F00", "#009944", "#4477AA"),
    labels = \(x) map(environments[x], \(p) TeX(sprintf("$(%g, %g)$", p[1], p[2])))
  ) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  labs(
    x = "Time Step",
    y = "Participation (%)",
    color = expression("Information Environment (" * mu[epsilon] * "," ~ sigma[epsilon] * ")")
  ) +
  theme_this


list(plot_temporal = plot_temporal) %>%
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
