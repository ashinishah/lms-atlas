# utils.R — shared helpers for LMS Atlas
# All image URL construction goes through these functions.
# Never hardcode paths elsewhere in the app.

# ── Image base path ────────────────────────────────────────────────────────────
# Set LMS_IMAGE_BASE in .Renviron (local) or Connect Cloud env config (deploy).
# Falls back to data/images for local dev if env var is not set.

image_base <- function() {
  Sys.getenv("LMS_IMAGE_BASE", unset = "data/images")
}

# ── Image URL helpers ──────────────────────────────────────────────────────────

thumbnail_url <- function(slide_id) {
  file.path(image_base(), "thumbnails", paste0(slide_id, "_thumbnail.jpg"))
}

heatmap_url <- function(slide_id) {
  file.path(image_base(), "heatmaps", paste0(slide_id, "_heatmap.jpg"))
}

patch_url <- function(slide_id, patch_id) {
  file.path(image_base(), "patches", slide_id, paste0(patch_id, ".jpg"))
}

# ── Outcome badge ──────────────────────────────────────────────────────────────
# Returns an HTML span with a color-coded badge.

outcome_badge <- function(outcome) {
  cls <- if (tolower(outcome) == "favorable") "badge bg-success" else "badge bg-danger"
  label <- if (tolower(outcome) == "favorable") "Favorable" else "Adverse"
  htmltools::tags$span(class = cls, label)
}

# ── Dataset badge ──────────────────────────────────────────────────────────────

dataset_badge <- function(dataset) {
  cls <- if (dataset == "TCGA") "badge" else "badge"
  style <- if (dataset == "TCGA") {
    "background-color: #0D9488;"
  } else {
    "background-color: #D97706;"
  }
  htmltools::tags$span(class = cls, style = style, dataset)
}

# ── Attention marker color ─────────────────────────────────────────────────────
# Maps attention score (0–1) to viridis-inspired marker color.

attention_marker_color <- function(score) {
  dplyr::case_when(
    score >= 0.80 ~ "#FDE725",  # yellow — high attention
    score >= 0.60 ~ "#5DC963",  # green  — mid attention
    TRUE          ~ "#3B528B"   # blue-purple — lower attention
  )
}

# ── Filter metadata by dataset ─────────────────────────────────────────────────

filter_dataset <- function(metadata, dataset) {
  dplyr::filter(metadata, dataset == !!dataset)
}
