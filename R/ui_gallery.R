# ui_gallery.R — Gallery tab UI

ui_gallery <- function() {
  div(
    class = "p-3",
    # ── Controls row ──────────────────────────────────────────────────────────
    div(
      class = "d-flex align-items-center gap-3 mb-3 flex-wrap",
      # Search
      div(
        style = "width: 220px;",
        textInput("gallery_search", label = NULL, placeholder = "Search slide ID…")
      ),
      # Outcome filter
      selectInput(
        "gallery_outcome_filter",
        label    = NULL,
        choices  = c("All outcomes" = "", "Favorable" = "Favorable", "Adverse" = "Adverse"),
        width    = "160px"
      ),
      # Sort
      selectInput(
        "gallery_sort",
        label    = NULL,
        choices  = c(
          "Slide ID"         = "slide_id",
          "Max attention ↓"  = "max_attention_desc",
          "Max attention ↑"  = "max_attention_asc",
          "Age"              = "age"
        ),
        width    = "180px"
      ),
      # Slide count badge
      span(uiOutput("gallery_slide_count_badge"), class = "ms-auto text-muted fs-xs")
    ),

    # ── Card grid ─────────────────────────────────────────────────────────────
    uiOutput("gallery_grid")
  )
}
