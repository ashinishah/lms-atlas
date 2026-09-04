# utils.R — shared helpers for LMS Atlas
# All image URL construction goes through these functions.
# Never hardcode paths elsewhere in the app.

# ── Bootstrap column shorthands ───────────────────────────────────────────────
col_4  <- function(...) column(4,  ...)
col_6  <- function(...) column(6,  ...)
col_12 <- function(...) column(12, ...)

# ── Image base path (thumbnails + heatmaps) ────────────────────────────────────
image_base <- function() {
  Sys.getenv("LMS_IMAGE_BASE", unset = "C:/data/lms-images")
}

# ── Globus/source data path (patch images + metadata CSVs) ────────────────────
globus_base <- function() {
  Sys.getenv("LMS_GLOBUS_BASE",
    unset = "C:/Users/ashin/OneDrive/Documents/Globus/shiny/tcga_sarc")
}

patches_base <- function() file.path(globus_base(), "patches")

# ── Image URL helpers ──────────────────────────────────────────────────────────

thumbnail_url <- function(slide_id) {
  paste0("lms-images/thumbnails/", slide_id, ".png")
}

heatmap_url <- function(slide_id) {
  paste0("lms-images/heatmaps/", slide_id, "_true_consensus.png")
}

# patch_id is the full {slide_id}_{x}_{y} string
patch_url <- function(slide_id, patch_id) {
  paste0("lms-patches/", slide_id, "/", patch_id, ".png")
}

# ── Outcome badge ──────────────────────────────────────────────────────────────
outcome_badge <- function(outcome) {
  if (tolower(outcome) == "favorable") {
    htmltools::tags$span(class = "badge",
      style = "background-color:#166534; color:#fff;", "Favorable")
  } else {
    htmltools::tags$span(class = "badge",
      style = "background-color:#991B1B; color:#fff;", "Adverse")
  }
}

# ── Dataset badge ──────────────────────────────────────────────────────────────
dataset_badge <- function(dataset) {
  style <- if (dataset == "TCGA") "background-color:#0D9488;" else "background-color:#D97706;"
  htmltools::tags$span(class = "badge", style = style, dataset)
}

# ── Short slide ID ─────────────────────────────────────────────────────────────
short_id <- function(slide_id) {
  sub("^([^-]+-[^-]+-[^-]+).*", "\\1", slide_id)
}

# ── Attention marker color (plasma colormap) ───────────────────────────────────
attention_marker_color <- function(score) {
  dplyr::case_when(
    score >= 0.80 ~ "#FCCE25",
    score >= 0.55 ~ "#E8612A",
    TRUE          ~ "#8B1FA8"
  )
}

attention_marker_text <- function(score) {
  dplyr::if_else(score >= 0.55, "#1A1A2E", "#FFFFFF")
}

# ── Filter metadata by dataset ─────────────────────────────────────────────────
filter_dataset <- function(metadata, dataset) {
  dplyr::filter(metadata, dataset == !!dataset)
}
