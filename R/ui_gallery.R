# ui_gallery.R — Gallery tab UI

ui_gallery <- function() {
  div(
    class = "p-3",

    # ── Controls row ──────────────────────────────────────────────────────────
    div(
      class = "gallery-controls d-flex align-items-center gap-3 mb-3 flex-wrap",
      div(
        style = "width: 220px;",
        textInput("gallery_search", label = NULL, placeholder = "Search slide ID…")
      ),
      selectInput(
        "gallery_outcome_filter",
        label   = NULL,
        choices = c("All outcomes" = "", "Favorable" = "Favorable", "Adverse" = "Adverse"),
        width   = "160px"
      ),
      span(uiOutput("gallery_slide_count_badge"), class = "ms-auto text-muted fs-xs")
    ),

    # ── Card/list grid ────────────────────────────────────────────────────────
    uiOutput("gallery_grid")
  )
}
