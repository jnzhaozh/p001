library(ggplot2)

theme_this <-
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(
    # --- Structural Elements ---
    plot.background  = element_blank(),
    panel.background = element_rect(fill = "grey95", color = NA),
    # panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 0.3),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    
    # --- Text & Labels ---
    text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    
    # --- Legend ---
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12, face = "bold"),
    legend.title.position = "left",
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "grey95", color = NA),
    # legend.box.background = element_rect(fill = "grey90", colour = NA),
    legend.margin = margin(t = 5, r = 15, b = 5, l = 15, unit = "pt"),
    legend.key = element_blank(),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.75, "cm"),
    legend.key.height = unit(0.5, "cm"),

    
    # --- Facets ---
    strip.background = element_rect(fill = "grey90", colour = NA),
    strip.text = element_text(size = 12, face = "bold", hjust = 0),
    strip.placement = "outside",
    panel.spacing.y = unit(6, "pt")
  ) 
