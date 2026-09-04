# ui_compare.R — TCGA vs SPORE comparison tab UI
# Dataset toggle in header is grayed out on this tab (both datasets shown simultaneously)

ui_compare <- function() {
  nav_panel_body(
    class = "p-4",
    h4("TCGA vs. SPORE", class = "mb-1"),
    p("Side-by-side cross-dataset comparison. Dataset toggle is disabled here.",
      class = "text-muted mb-3"),

    fluidRow(
      col_6(
        div(
          class = "d-flex align-items-center gap-2 mb-2",
          span(class = "badge", style = "background:#0D9488;", "TCGA-SARC"),
          span(uiOutput("compare_tcga_count"), class = "text-muted fs-xs")
        ),
        uiOutput("compare_tcga_grid")
      ),
      col_6(
        div(
          class = "d-flex align-items-center gap-2 mb-2",
          span(class = "badge", style = "background:#D97706;", "SPORE"),
          span(uiOutput("compare_spore_count"), class = "text-muted fs-xs")
        ),
        uiOutput("compare_spore_grid")
      )
    )
  )
}
