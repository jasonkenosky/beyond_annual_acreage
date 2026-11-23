#===================================================================
# R/theme_bay.R
#
# Core plotting theme + palettes for the whole-Bay SAV/oyster-lease
# pipeline (Kenosky/Gurbisz). Built fresh for this project's actual
# variables -- not adapted from an older, unrelated project's theme
# (see theme_sav.R, retained separately, built for md_sav_dynamics
# and effectively unused; do not source both in the same session
# without checking for name collisions).
#
# Design goals, in priority order:
#   1. Colorblind-safe by default, for every discrete AND continuous
#      scale -- not just the qualitative ones.
#   2. Readable at poor eyesight / small-screen / projector scale --
#      larger base font, thicker default lines/points than ggplot2's
#      defaults, high but not overwhelming gridline contrast.
#   3. Clean and consistent -- centered titles, legend off by
#      default (figures should stand alone with direct labeling
#      where practical), bold readable facet strips.
#   4. Right palette family for the variable's actual structure, not
#      one palette forced onto everything:
#        - NOMINAL categories (no inherent order: survey zone,
#          region/salinity stratum) -> qualitative, hue-based
#          (Okabe-Ito) -- distinguishing categories is the only job.
#        - ORDINAL / sequential categories (span category, density
#          class) -> sequential, NOT qualitative -- an arbitrary
#          rainbow of hues obscures the real order these variables
#          have; a single perceptually-ordered ramp (viridis)
#          preserves it, and remains readable by magnitude even if
#          fine hue differences aren't resolvable.
#        - CONTINUOUS variables (PC1/PC2, raw metrics) -> viridis
#          continuous, same reasoning as sequential above.
#      ggplot2's built-in viridis scales are used for the sequential/
#      continuous cases specifically because they ship with ggplot2
#      itself (scale_*_viridis_d/c) -- no new package dependency.
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
      # Titles centered, bold, generously sized relative to base --
      # the single most-read element of a figure at a glance.
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
      # generous margins so tick labels never crowd the axis title.
      axis.title.x = element_text(size = rel(1), margin = margin(t = 10)),
      axis.title.y = element_text(size = rel(1), margin = margin(r = 10)),
      axis.text = element_text(size = rel(0.95), color = "grey20"),
      axis.ticks = element_line(color = "grey50", linewidth = 0.3),
      
      # Panel grid: visible enough to read values off precisely, not
      # so heavy it competes with the data. Major only, no minor --
      # minor gridlines add clutter without adding much readable
      # information at this base size.
      panel.grid.major = element_line(color = "grey85", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      
      # Legend -- off by default (see file header); when turned on,
      # legend text/title sized to match the rest of the figure, not
      # ggplot2's shrunken defaults.
      legend.position = legend_position,
      legend.title = element_text(face = "bold", size = rel(0.95)),
      legend.text = element_text(size = rel(0.9)),
      legend.key = element_blank(),
      
      # Facet strips -- bold, high-contrast background so panel
      # labels don't get lost, a real readability issue at small
      # multiples scale.
      strip.text = element_text(face = "bold", size = rel(0.95)),
      strip.background = element_rect(fill = "grey92", color = NA),
      
      plot.margin = margin(t = 10, r = 14, b = 10, l = 10)
    )
}

# Convenience wrappers for the (less common) cases where a legend is
# actually needed -- e.g. a figure with more categories than can be
# directly labeled on the plot itself.
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

# VIMS survey zone (4 categories, no inherent order beyond a loose
# upstream-to-downstream geography -- treated as nominal).
bay_palette_zone <- c(
  "Upper Bay"             = "#0072B2",  # blue
  "Mid-Bay"                = "#D55E00",  # vermillion
  "Lower Bay / Virginia"   = "#009E73",  # bluish green
  "Atlantic Coastal Bays"  = "#E69F00"   # orange
)

# Numeric-code version (survey_zone as stored in the data: "1".."4"),
# same color assignments, since most scripts carry the numeric code
# through rather than the display label until a figure is built.
bay_palette_zone_code <- c(
  "1" = "#0072B2",  # Upper Bay
  "2" = "#D55E00",  # Mid-Bay
  "3" = "#009E73",  # Lower Bay / Virginia
  "4" = "#E69F00"   # Atlantic Coastal Bays
)

# Companion lookup: numeric code -> readable display label. Kept
# here, not duplicated per-script, so every figure that relabels a
# zone axis uses the same wording.
bay_zone_code_to_label <- c(
  "1" = "Upper Bay",
  "2" = "Mid-Bay",
  "3" = "Lower Bay / Virginia",
  "4" = "Atlantic Coastal Bays"
)

# Region/salinity stratum (9 categories: mainstem x 4 salinity zones,
# tributary x 4 salinity zones, Atlantic Coastal Bay standalone).
# A naive 9-hue qualitative palette is NOT reliably colorblind-safe
# beyond ~8 categories -- instead, mainstem and tributary each get a
# single base hue (blue-family, orange-family), varied by LIGHTNESS
# across the 4 salinity zones (TF -> OH -> MH -> PH, tidal-fresh to
# polyhaline), so mainstem-vs-tributary is read by hue and salinity
# position is read by lightness -- both channels colorblind-robust.
# Atlantic Coastal Bay, structurally distinct from both, gets its
# own hue.
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

# Chain span category (3 ordered levels: ephemeral < short_lived <
# longer_lived). Pulled from viridis at evenly-spaced points rather
# than a qualitative palette, specifically to preserve the real
# order -- see file header.
bay_palette_span <- c(
  "ephemeral"    = viridisLite::viridis(3)[1],
  "short_lived"  = viridisLite::viridis(3)[2],
  "longer_lived" = viridisLite::viridis(3)[3]
)

# SAV density class (1-4, low to high) -- same ordinal logic.
bay_palette_density <- stats::setNames(
  viridisLite::viridis(4),
  as.character(1:4)
)

# -------------------------------------------------------------------
# 4. Scale helpers
# -------------------------------------------------------------------

# -- Nominal: survey zone (display-label version) --
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
# line. Thin wrappers around ggplot2's built-in viridis scales --
# named for consistency with the rest of this file, not because they
# do anything ggplot2 doesn't already provide directly.
scale_color_bay_continuous <- function(...) {
  scale_color_viridis_c(...)
}
scale_fill_bay_continuous <- function(...) {
  scale_fill_viridis_c(...)
}

# ---------------------------------------------------------------
# 5. Public theme wrapper (fail-safe fallback, matching the pattern
# already established in theme_sav.R -- a missing/misnamed theme
# function is cosmetic, not a correctness problem, so this degrades
# gracefully rather than crashing a script over it)
# ---------------------------------------------------------------
use_bay_theme <- function() {
  if (exists("theme_bay", mode = "function")) return(theme_bay())
  ggplot2::theme_minimal(base_size = 13)
}