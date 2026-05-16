# packages ----------------------------------------------------------------

this_packages <- c(
  "dplyr",
  "tibble",
  "ggplot2",
  "purrr",
  "tidyr",
  "forcats",
  "furrr",
  "future",
  "truncnorm",
  "patchwork",
  "scales",
  "ggsci",
  "latex2exp",
  "here"
)

suppressPackageStartupMessages({
  invisible(lapply(this_packages, library, character.only = TRUE))
})


# theme -------------------------------------------------------------------

theme_set(theme_bw(base_size = 10, base_family = "sans"))

theme_replace(
  plot.background = element_blank(),
  panel.background = element_rect(fill = "grey98", color = NA),
  panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(color = NA, fill = NA, linewidth = 0.5),

  axis.title = element_text(size = rel(1.0)),
  axis.text = element_text(size = rel(1.0)),
  axis.ticks = element_blank(),
  axis.ticks.length = unit(0, "pt"),
  text = element_text(size = rel(1.0)),
  axis.title.x = element_text(vjust = -0.5, margin = margin(t = 8)),
  axis.title.y = element_text(hjust = 0.5, margin = margin(r = 8), angle = 90),

  plot.title = element_text(
    size = rel(1.0),
    face = "bold",
    hjust = 0.5,
    margin = margin(b = 10)
  ),
  plot.subtitle = element_text(
    size = rel(1.0),
    face = "bold",
    hjust = 0,
    margin = margin(b = 5)
  ),

  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = "horizontal",
  legend.box.just = "center",
  legend.title = element_text(
    size = rel(1.0),
    margin = margin(r = 15),
    vjust = 0.5
  ),
  legend.title.position = "left",
  legend.text = element_text(size = rel(1.0)),
  legend.background = element_rect(fill = "grey95", color = NA),
  legend.key = element_blank(),
  legend.key.size = unit(0.5, "cm"),
  legend.key.width = unit(1.2, "cm"),
  legend.key.height = unit(0.4, "cm"),
  legend.margin = margin(t = 5, r = 15, b = 5, l = 15, unit = "pt"),
  legend.spacing.x = unit(0.5, "cm"),

  panel.spacing = unit(1, "lines"),
  strip.background = element_rect(fill = "grey95", colour = NA),
  strip.placement = "outside",
  strip.text = element_text(
    size = rel(1.0),
    face = "bold",
    hjust = 0.5,
    margin = margin(4, 4, 4, 4)
  )
)

# utility functions -------------------------------------------------------

this_saveRDS <- function(x) {
  obj_name <- deparse(substitute(x))
  file_name <- paste0(obj_name, ".rds")
  file_path <- here::here("results", file_name)

  dir.create(here::here("results"), showWarnings = FALSE, recursive = TRUE)

  saveRDS(x, file = file_path)

  message(">>> Saved to: ", file_path)
}


this_ggsave <- function(x, width = 16, height = 14) {
  obj_name <- deparse(substitute(x))
  file_name <- paste0(obj_name, ".pdf")
  file_path <- here::here("figures", file_name)

  dir.create(here::here("figures"), showWarnings = FALSE, recursive = TRUE)

  ggsave(
    filename = file_path,
    plot = x,
    width = width,
    height = height,
    units = "cm",
    dpi = 300,
    device = grDevices::cairo_pdf
  )

  message(">>> Saved to: ", file_path)
}
