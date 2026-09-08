# utils.R — shared helpers for LMS Atlas
# All image URL construction goes through these functions.
# Never hardcode paths elsewhere in the app.

# ── Bootstrap column shorthands ───────────────────────────────────────────────
col_4  <- function(...) column(4,  ...)
col_6  <- function(...) column(6,  ...)
col_12 <- function(...) column(12, ...)

# ── Image / data base path ────────────────────────────────────────────────────
# LMS_IMAGE_BASE controls everything:
#   Local dev: path to Globus folder (tcga_sarc root)
#   Connect:   https://... S3 bucket URL
image_base <- function() {
  Sys.getenv("LMS_IMAGE_BASE",
    unset = "C:/Users/ashin/OneDrive/Documents/Globus/shiny/tcga_sarc")
}

# Globus fallback — defaults to image_base() so one env var covers both locally
globus_base <- function() {
  Sys.getenv("LMS_GLOBUS_BASE", unset = image_base())
}

tissue_base         <- function() file.path(globus_base(), "tissue50_mask50")
heatmaps_base       <- function() file.path(tissue_base(), "dense heatmaps")
patches_base        <- function() file.path(tissue_base(), "patches")
patch_metadata_path <- function() file.path(tissue_base(), "patch_metadata.csv")

# ── Image URL helpers ──────────────────────────────────────────────────────────
# When LMS_IMAGE_BASE is an https:// URL (e.g. S3), return absolute URLs.
# When it's a local path, return Shiny resource-path URLs (served via addResourcePath).

.image_base_is_url <- function() startsWith(image_base(), "http")

thumbnail_url <- function(slide_id) {
  if (.image_base_is_url())
    paste0(image_base(), "/thumbnails/", slide_id, ".png")
  else
    paste0("lms-images/", slide_id, ".png")  # served from wsis/ via addResourcePath
}

heatmap_url <- function(slide_id) {
  if (.image_base_is_url())
    paste0(image_base(), "/heatmaps/", slide_id, "_true_consensus.png")
  else
    paste0("lms-heatmaps/", slide_id, "_true_consensus.png")
}

# patch_id is the full {slide_id}_{x}_{y} string
patch_url <- function(slide_id, patch_id) {
  if (.image_base_is_url())
    paste0(image_base(), "/patches/", slide_id, "/", patch_id, ".png")
  else
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
  style <- if (dataset == "TCGA") "background-color:#0D9488;" else "background-color:#C9A227;"
  htmltools::tags$span(class = "badge", style = style, dataset)
}

# ── Short slide ID ─────────────────────────────────────────────────────────────
short_id <- function(slide_id) {
  sub("^([^-]+-[^-]+-[^-]+).*", "\\1", slide_id)
}

# ── Attention marker color (viridis: purple → blue → teal → green → yellow) ────
attention_marker_color <- function(score) {
  dplyr::case_when(
    score >= 0.85 ~ "#FDE725",
    score >= 0.65 ~ "#35B779",
    score >= 0.45 ~ "#21908C",
    score >= 0.25 ~ "#31688E",
    TRUE          ~ "#440154"
  )
}

attention_marker_text <- function(score) {
  dplyr::if_else(score >= 0.65, "#1A1A2E", "#FFFFFF")
}

# ── Recurrence marker color (viridis bands: 3 seeds=teal, 4=green, 5=yellow) ───
recurrence_marker_color <- function(count) {
  dplyr::case_when(
    count >= 5L ~ "#FDE725",
    count >= 4L ~ "#35B779",
    TRUE        ~ "#21908C"
  )
}

recurrence_marker_text <- function(count) {
  dplyr::if_else(count >= 4L, "#1A1A2E", "#FFFFFF")
}

# ── Filter metadata by dataset ─────────────────────────────────────────────────
filter_dataset <- function(metadata, dataset) {
  dplyr::filter(metadata, dataset == !!dataset)
}

# ── PNG dimension reader (no extra package needed) ─────────────────────────────
# Reads width/height from the PNG IHDR chunk header.
png_dims <- function(path) {
  if (!file.exists(path)) return(c(w = NA_integer_, h = NA_integer_))
  raw_bytes <- readBin(path, "raw", n = 24L)
  to_uint <- function(b) sum(as.integer(b) * c(16777216L, 65536L, 256L, 1L))
  c(w = to_uint(raw_bytes[17:20]), h = to_uint(raw_bytes[21:24]))
}

# Local file path for a slide's thumbnail
thumbnail_path <- function(slide_id) {
  file.path(image_base(), "thumbnails", paste0(slide_id, ".png"))
}


# Per-slide SVG viewBox extents from slide_dimensions.csv (true level-0 pixel
# dimensions read directly from the SVS files via OpenSlide).
# Returns list(x = named_int_vector, y = named_int_vector) keyed by slide_id.
slide_extents <- function() {
  path <- file.path("data", "slide_dimensions.csv")
  if (!file.exists(path)) return(NULL)
  df    <- readr::read_csv(path, show_col_types = FALSE)
  ext_x <- setNames(as.integer(df$level_0_width),  df$slide_id)
  ext_y <- setNames(as.integer(df$level_0_height), df$slide_id)
  list(x = ext_x, y = ext_y)
}
