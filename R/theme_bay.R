#===================================================================
# R/theme_bay.R
#
# Core plotting theme + palettes for the whole-Bay SAV/oyster-lease
# pipeline (Kenosky/Gurbisz). 
#
# Design goals, in priority order:
#   1. Colorblind-safe by default, 
#   2. Readable for poor eyesight.
#   3. Clean and consistent -- centered titles, legend off by
#      default.
#   4. Palette family for the variable's structure:
#        - NOMINAL categories -> qualitative, hue-based
#          (Okabe-Ito).
#        - ORDINAL / sequential categories (span category, density
#          class) -> sequential, not qualitative.
#        - CONTINUOUS variables (PC1/PC2, raw metrics) -> viridis
#          continuous.
#      ggplot2's built-in viridis scales are used for the sequential/
#      continuous cases because they ship with ggplot2
#      itself (scale_*_viridis_d/c).
#
# Usage:
#   source(here::here("R", "theme_bay.R"))
#
#   ggplot(segment_summary, aes(x = home_zone, y = km_mean_span_years)) +
#     geom_boxplot() +
#     theme_bay() +
#     scale_color_bay_zone()   # nominal, e.g. color = home_zone
#
#   ggplot(chain_summary, aes(x = chain_span_years, fill = span_category)) +
#     geom_histogram() +
#     theme_bay() +
#     scale_fill_bay_span()    # ordinal, e.g. fill = span_category
#===================================================================

suppressPackageStartupMessages({
  library(ggplot2)
})

# -------------------------------------------------------------------
# 1. Base theme
# -------------------------------------------------------------------

theme_bay <- function(base_size = 13,
                      base_family = "sans",
                      legend_position = "none") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Titles centered, bold --
      plot.title = element_text(
        hjust = 0.5,
        face  = "bold",
        size  = rel(1.15),
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size  = rel(0.95),
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        size  = rel(0.75),
        color = "grey40",
        hjust = 1
      ),
      
      # Axes -- labels sized close to base (not the shrunken default),
      axis.title.x = element_text(size = rel(1), margin = margin(t = 10)),
      axis.title.y = element_text(size = rel(1), margin = margin(r = 10)),
      axis.text = element_text(size = rel(0.95), color = "grey20"),
      axis.ticks = element_line(color = "grey50", linewidth = 0.3),
      
      # Panel grid: visible enough to read values.
      panel.grid.major = element_line(color = "grey85", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      
      # Legend -- off by default.
      legend.position = legend_position,
      legend.title = element_text(face = "bold", size = rel(0.95)),
      legend.text = element_text(size = rel(0.9)),
      legend.key = element_blank(),
      
      # Facet strips -- bold, high-contrast background.
      strip.text = element_text(face = "bold", size = rel(0.95)),
      strip.background = element_rect(fill = "grey92", color = NA),
      
      plot.margin = margin(t = 10, r = 14, b = 10, l = 10)
    )
}

# Convenience wrappers for a legend.
theme_bay_legend_right <- function(...) {
  theme_bay(legend_position = "right", ...)
}

theme_bay_legend_bottom <- function(...) {
  theme_bay(legend_position = "bottom", ...)
}

# -------------------------------------------------------------------
# 2. Nominal (qualitative) palettes -- Okabe-Ito, colorblind-safe
# -------------------------------------------------------------------

# Okabe-Ito base colors (for reference; the definitive colorblind-
# safe qualitative palette, safe under deuteranopia, protanopia, and
# tritanopia):
# black        #000000
# orange       #E69F00
# sky blue     #56B4E9
# bluish green #009E73
# yellow       #F0E442
# blue         #0072B2
# vermillion   #D55E00
# reddish purp #CC79A7

# VIMS survey zone.
bay_palette_zone <- c(
  "Upper Bay"             = "#0072B2",  # blue
  "Mid-Bay"                = "#D55E00",  # vermillion
  "Lower Bay / Virginia"   = "#009E73",  # bluish green
  "Atlantic Coastal Bays"  = "#E69F00"   # orange
)

# Numeric-code version.
bay_palette_zone_code <- c(
  "1" = "#0072B2",  # Upper Bay
  "2" = "#D55E00",  # Mid-Bay
  "3" = "#009E73",  # Lower Bay / Virginia
  "4" = "#E69F00"   # Atlantic Coastal Bays
)

# Companion lookup.
bay_zone_code_to_label <- c(
  "1" = "Upper Bay",
  "2" = "Mid-Bay",
  "3" = "Lower Bay / Virginia",
  "4" = "Atlantic Coastal Bays"
)

# Region/salinity stratum.
bay_palette_region_stratum <- c(
  "Mainstem_TF"            = "#08306B",  # darkest blue
  "Mainstem_OH"            = "#2171B5",
  "Mainstem_MH"            = "#4292C6",
  "Mainstem_PH"            = "#9ECAE1",  # lightest blue
  "Tributary_TF"           = "#7F2704",  # darkest orange
  "Tributary_OH"           = "#D94801",
  "Tributary_MH"           = "#F16913",
  "Tributary_PH"           = "#FDAE6B",  # lightest orange
  "Atlantic Coastal Bay"   = "#009E73"   # bluish green, distinct
)

# -------------------------------------------------------------------
# 3. Ordinal (sequential) palettes -- viridis-based, order-preserving
# -------------------------------------------------------------------

# Chain span category.
bay_palette_span <- c(
  "ephemeral"    = viridisLite::viridis(3)[1],
  "short_lived"  = viridisLite::viridis(3)[2],
  "longer_lived" = viridisLite::viridis(3)[3]
)

# SAV density class (1-4, low to high)c.
bay_palette_density <- stats::setNames(
  viridisLite::viridis(4),
  as.character(1:4)
)

# -------------------------------------------------------------------
# 4. Scale helpers
# -------------------------------------------------------------------

# -- Nominal: survey zone (display-label) --
scale_color_bay_zone <- function(na.value = "grey70", ...) {
  scale_color_manual(values = bay_palette_zone, drop = FALSE, na.value = na.value, ...)
}
scale_fill_bay_zone <- function(na.value = "grey70", ...) {
  scale_fill_manual(values = bay_palette_zone, drop = FALSE, na.value = na.value, ...)
}

# -- Nominal: survey zone (numeric-code version, "1".."4") --
scale_color_bay_zone_code <- function(na.value = "grey70", ...) {
  scale_color_manual(values = bay_palette_zone_code, drop = FALSE, na.value = na.value, ...)
}
scale_fill_bay_zone_code <- function(na.value = "grey70", ...) {
  scale_fill_manual(values = bay_palette_zone_code, drop = FALSE, na.value = na.value, ...)
}

# -- Nominal: region/salinity stratum --
scale_color_bay_region_stratum <- function(na.value = "grey70", ...) {
  scale_color_manual(values = bay_palette_region_stratum, drop = FALSE, na.value = na.value, ...)
}
scale_fill_bay_region_stratum <- function(na.value = "grey70", ...) {
  scale_fill_manual(values = bay_palette_region_stratum, drop = FALSE, na.value = na.value, ...)
}

# -- Ordinal: chain span category --
scale_color_bay_span <- function(na.value = "grey70", ...) {
  scale_color_manual(values = bay_palette_span, drop = FALSE, na.value = na.value, ...)
}
scale_fill_bay_span <- function(na.value = "grey70", ...) {
  scale_fill_manual(values = bay_palette_span, drop = FALSE, na.value = na.value, ...)
}

# -- Ordinal: density class --
scale_color_bay_density <- function(na.value = "grey70", ...) {
  scale_color_manual(values = bay_palette_density, drop = FALSE, na.value = na.value, ...)
}
scale_fill_bay_density <- function(na.value = "grey70", ...) {
  scale_fill_manual(values = bay_palette_density, drop = FALSE, na.value = na.value, ...)
}

# -- Continuous: PC scores, raw metrics, anything on a real number
# line.
scale_color_bay_continuous <- function(...) {
  scale_color_viridis_c(...)
}
scale_fill_bay_continuous <- function(...) {
  scale_fill_viridis_c(...)
}

# ---------------------------------------------------------------
# 5. Theme wrapper.
# ---------------------------------------------------------------
use_bay_theme <- function() {
  if (exists("theme_bay", mode = "function")) return(theme_bay())
  ggplot2::theme_minimal(base_size = 13)
}