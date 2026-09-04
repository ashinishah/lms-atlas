# ui_inspect.R — Inspect tab UI
# Layout: left info panel | WSI canvas | colorbar sidebar (heatmap) | right patch strip

ui_inspect <- function() {
  div(
    class = "inspect-layout",

    # ── Left info panel ──────────────────────────────────────────────────────
    div(
      class = "inspect-info-panel",
      div(class = "inspect-panel-divider"),
      div(
        class = "inspect-panel-section inspect-slide-info",
        div(class = "inspect-section-label", "SLIDE INFO"),
        uiOutput("inspect_sidebar_content")
      ),
      div(
        class = "inspect-panel-footer",
        actionButton(
          "inspect_add_compare", "+ Add to comparison",
          class = "btn btn-sm btn-outline-secondary w-100"
        )
      )
    ),

    # ── WSI canvas ───────────────────────────────────────────────────────────
    div(
      id    = "wsi_canvas",
      class = "wsi-canvas",
      uiOutput("wsi_image_ui"),
      uiOutput("wsi_marker_svg"),
      div(class = "minimap", uiOutput("minimap_ui")),
      div(
        class = "zoom-controls",
        tags$button(class = "btn", onclick = "lmsZoomIn('wsi_canvas')",    title = "Zoom in",    "+"),
        tags$button(class = "btn", onclick = "lmsZoomReset('wsi_canvas')", title = "Reset zoom", "⊙"),
        tags$button(class = "btn", onclick = "lmsZoomOut('wsi_canvas')",   title = "Zoom out",   "−")
      )
    ),

    # ── Colorbar sidebar (only in heatmap mode) ───────────────────────────────
    uiOutput("inspect_colorbar_sidebar"),

    # ── Right patch strip (topk / recurring mode) ─────────────────────────────
    uiOutput("inspect_right_strip")
  )
}
