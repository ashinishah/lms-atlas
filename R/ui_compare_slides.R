# ui_compare_slides.R — Side-by-side slide comparison UI

ui_compare_slides <- function() {
  div(
    class = "compare-slides-layout",

    # ── Slide A ───────────────────────────────────────────────────────────────
    div(
      class = "compare-slide-panel",
      div(
        class = "compare-slide-header",
        div(class = "compare-slot-label", "SLIDE A"),
        uiOutput("compare_slides_picker_a")
      ),
      div(
        id    = "compare_canvas_a",
        class = "compare-slide-canvas",
        uiOutput("compare_slide_image_a"),
        div(
          class = "compare-zoom-controls",
          tags$button(class = "btn", onclick = "lmsZoomIn('compare_canvas_a')",    title = "Zoom in",    "+"),
          tags$button(class = "btn", onclick = "lmsZoomReset('compare_canvas_a')", title = "Reset zoom", "⊙"),
          tags$button(class = "btn", onclick = "lmsZoomOut('compare_canvas_a')",   title = "Zoom out",   "−")
        )
      ),
      div(class = "compare-slide-info", uiOutput("compare_slide_info_a"))
    ),

    # ── Divider ───────────────────────────────────────────────────────────────
    div(class = "compare-divider"),

    # ── Slide B ───────────────────────────────────────────────────────────────
    div(
      class = "compare-slide-panel",
      div(
        class = "compare-slide-header",
        div(class = "compare-slot-label", "SLIDE B"),
        uiOutput("compare_slides_picker_b")
      ),
      div(
        id    = "compare_canvas_b",
        class = "compare-slide-canvas",
        uiOutput("compare_slide_image_b"),
        div(
          class = "compare-zoom-controls",
          tags$button(class = "btn", onclick = "lmsZoomIn('compare_canvas_b')",    title = "Zoom in",    "+"),
          tags$button(class = "btn", onclick = "lmsZoomReset('compare_canvas_b')", title = "Reset zoom", "⊙"),
          tags$button(class = "btn", onclick = "lmsZoomOut('compare_canvas_b')",   title = "Zoom out",   "−")
        )
      ),
      div(class = "compare-slide-info", uiOutput("compare_slide_info_b"))
    )
  )
}
