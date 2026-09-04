# ui_inspect.R — Inspect tab UI
# Single-slide deep-dive. Four sub-modes: plain | heatmap | recurring | topk.

ui_inspect <- function() {
  div(
    style = "padding: 0; height: calc(100vh - 56px);",  # fill under navbar

    div(
      class = "d-flex h-100",

      # ── Left sidebar ───────────────────────────────────────────────────────
      div(
        class = "inspect-sidebar",
        uiOutput("inspect_sidebar_content")
      ),

      # ── Main panel ────────────────────────────────────────────────────────
      div(
        class = "d-flex flex-column flex-grow-1 overflow-hidden",

        # Mode bar
        div(
          class = "mode-bar d-flex align-items-center gap-2 px-3 py-2 border-bottom bg-white",
          strong("View:", class = "me-1 text-muted fs-xs"),
          div(
            class = "btn-group btn-group-sm",
            actionButton("mode_plain",     "Plain WSI",       class = "btn btn-outline-secondary"),
            actionButton("mode_heatmap",   "Dense Heatmap",   class = "btn btn-outline-secondary"),
            actionButton("mode_recurring", "Recurring Patches",class = "btn btn-outline-secondary"),
            actionButton("mode_topk",      "Top K Patches",   class = "btn btn-outline-secondary")
          ),
          # K selector — shown only in topk mode
          uiOutput("topk_selector_ui")
        ),

        # WSI canvas
        div(
          class = "wsi-canvas flex-grow-1 d-flex align-items-center justify-content-center",
          uiOutput("wsi_image_ui"),
          # Minimap
          div(class = "minimap", uiOutput("minimap_ui")),
          # Zoom controls
          div(
            class = "zoom-controls",
            actionButton("zoom_in",    "+", class = "btn"),
            actionButton("zoom_reset", "⊙", class = "btn"),
            actionButton("zoom_out",   "−", class = "btn")
          )
        ),

        # Bottom strip — patch strip OR heatmap hint bar, depending on mode
        uiOutput("inspect_bottom_strip")
      )
    )
  )
}
