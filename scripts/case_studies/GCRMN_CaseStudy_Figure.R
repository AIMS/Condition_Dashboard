## ============================================================================
## Title: GCRMN Case Study Figure 
## Summary:
##   Builds a four-panel publication figure for reef case studies:
##   (A, C) hard coral cover time series and (B, D) radial condition summaries.
## Author: Manuel Gonzalez Rivero
## Script: scripts/case_studies/GCRMN_CaseStudy_Figure.R
## Last Updated: 2026-05-27
## Version: 1.0.0
## Inputs:
##   - data/indices.RData
##   - data/GCRMN_CaseStudy_HC_reefs.RData
## Outputs:
##   - Fig/GCRMN_CaseStudy_Figure.png
##   - Fig/GCRMN_CaseStudy_Figure.svg
## Usage:
##   Rscript scripts/case_studies/GCRMN_CaseStudy_Figure.R
## ============================================================================


# Load required libraries and functions ----
source("scripts/plotting_functions/radial.plot.summary.R")
source("scripts/plotting_functions/comp_change.R")
source("scripts/Misc/HighLevel_Classification.R")

library(cowplot)
library(dplyr)
library(ggplot2)
library(ggtext)
library(scales)

# Template configuration ----
cfg <- list(
  title = "GCRMN Case Study Figure",
  summary = "Four-panel case study figure with time-series and radial condition panels.",
  author = "Manuel Gonzalez Rivero",
  version = "1.0.0",
  run_date = as.character(Sys.Date()),
  conf_thsld = 0.8,
  ref = "Combined",
  indices_paths = "data/indices.RData",
  hard_coral_path = "data/GCRMN_CaseStudy_HC_reefs.RData",
  output_dir = "Fig",
  output_basename = "GCRMN_CaseStudy_Figure",
  output_width_cm = 35,
  output_height_cm = 25,
  site_1 = list(
    domain_name = "SNAPPER ISLAND",
    reef_name = "Snapper North",
    year = 2018,
    depth_filter = "shallow slope",
    shelf_override = NULL,
    depth_override = NULL,
    title_fill_hex = "#ED8B00FF"
  ),
  site_2 = list(
    domain_name = "LADY MUSGRAVE ISLAND",
    reef_name = "Lady Musgrave Island",
    year = 2012,
    depth_filter = NULL,
    shelf_override = "Inshore",
    depth_override = "Deep slope",
    title_fill_hex = "#0F85A0FF"
  )
)


# Load data ----
load(cfg$indices_paths) # Load indices data frame
load(cfg$hard_coral_path) # Modelled coral cover from monitoring dashboard


scores <- indices |>
  filter(Level == "reef") |>
  mutate(
    fYEAR = Year,
    Year = as.numeric(as.character(Year))
  )

# Plot styling ----
scale_fill_class.c <- function(...) {
  scale_fill_manual(
    values = c("Below" = "#FC6500", "Within" = "#48C617", "Above" = "#48C617"),
    drop = FALSE,
    ...
  )
}

my_theme <- theme(
  axis.text = element_text(size = 16, vjust = 0.5, family = "Bell MT"),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  axis.title = element_text(
    size = 18,
    face = "bold",
    family = "Bell MT",
    margin = margin(r = 10, unit = "mm")
  )
)

# Panel builders ----
make_time_series_panel <- function(domain_name_value, event_year) {
  GCRMN_CaseStudy_HC_reefs |>
    filter(domain_name == domain_name_value) |>
    mutate(cond.range = case_when(report_year > event_year ~ "After", .default = "Before")) |>
    ggplot() +
    geom_line(aes(y = median, x = report_year, color = cond.range), linewidth = 1) +
    geom_pointrange(
      aes(y = median, x = report_year, ymin = lower, ymax = upper, color = cond.range),
      size = 1,
      linewidth = 0.2,
      linetype = "dashed"
    ) +
    geom_segment(
      aes(x = event_year, y = 0.25, xend = event_year, yend = 0.15),
      lineend = "butt",
      linejoin = "mitre",
      linewidth = 5,
      colour = "#EC7014",
      arrow = arrow(length = unit(0.5, "cm"))
    ) +
    scale_y_continuous(labels = percent) +
    scale_color_manual(values = c("grey", "black")) +
    labs(x = "Year", y = "Hard Coral Cover (%)") +
    theme_classic() +
    my_theme +
    theme(legend.position = "none")
}

make_radial_panel <- function(df, title_fill_hex) {
  class_df <- Cond.Class(
    df |>
      filter(Reference == cfg$ref) |>
      mutate(across(c(Median, Lower, Upper), ~ case_when(is.na(.x) ~ 0.5, .default = .x))),
    conf = cfg$conf_thsld
  )

  radial.plot.summary(df, cfg$ref) +
    scale_fill_class.c() +
    scale_x_discrete(labels = label_wrap_gen(width = 15)) +
    labs(title = class_df |> pull(Class)) +
    my_theme +
    theme(
      axis.text.x = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_textbox_simple(
        fill = title_fill_hex,
        color = "white",
        box.color = NULL,
        r = unit(8, "pt"),
        padding = unit(c(5, 5, 5, 5), "pt"),
        width = NULL,
        size = 16,
        margin = unit(c(0, 0, 0, 0), "pt"),
        halign = 0.5,
        hjust = 1
      )
    )
}

build_radial_df <- function(site_cfg) {
  radial_df <- scores |>
    filter(Level == "reef", Year == site_cfg$year, Name == site_cfg$reef_name, Reference == cfg$ref)

  if (!is.null(site_cfg$depth_filter)) {
    radial_df <- radial_df |> filter(Depth == site_cfg$depth_filter)
  }

  radial_df <- radial_df |>
    mutate(
      Lower = ifelse(is.na(Lower), 0.5, Lower),
      Upper = ifelse(is.na(Upper), 0.5, Upper),
      Median = ifelse(is.na(Median), 0.5, Median),
      Classification = case_when(
        Lower > 0.5 ~ "Above",
        Upper < 0.5 ~ "Below",
        is.na(Lower) ~ NA,
        .default = "Within"
      )
    )

  if (!is.null(site_cfg$shelf_override)) {
    radial_df <- radial_df |> mutate(Shelf = site_cfg$shelf_override)
  }

  if (!is.null(site_cfg$depth_override)) {
    radial_df <- radial_df |> mutate(Depth = site_cfg$depth_override)
  }

  radial_df
}

# Build figure ----
f2a.temp <- make_time_series_panel(
  domain_name_value = cfg$site_1$domain_name,
  event_year = cfg$site_1$year
)

df2b <- build_radial_df(cfg$site_1)
f2b.cond <- make_radial_panel(df = df2b, title_fill_hex = cfg$site_1$title_fill_hex)

f2c.temp <- make_time_series_panel(
  domain_name_value = cfg$site_2$domain_name,
  event_year = cfg$site_2$year
)

df2d <- build_radial_df(cfg$site_2)
f2d.cond <- make_radial_panel(df = df2d, title_fill_hex = cfg$site_2$title_fill_hex)

f2 <- plot_grid(
  f2a.temp,
  f2b.cond,
  f2c.temp,
  f2d.cond,
  nrow = 2,
  ncol = 2,
  labels = c("A", "B", "C", "D")
)

# Export outputs ----
ggsave(
  plot = f2,
  path = cfg$output_dir,
  filename = paste0(cfg$output_basename, ".png"),
  width = cfg$output_width_cm,
  height = cfg$output_height_cm,
  units = "cm",
  dpi = 300,
  bg = "white"
)

ggsave(
  plot = f2,
  path = cfg$output_dir,
  filename = paste0(cfg$output_basename, ".svg"),
  width = cfg$output_width_cm,
  height = cfg$output_height_cm,
  units = "cm",
  device = "svg"
)

message("Export complete: ", file.path(cfg$output_dir, paste0(cfg$output_basename, ".png")))
message("Export complete: ", file.path(cfg$output_dir, paste0(cfg$output_basename, ".svg")))
