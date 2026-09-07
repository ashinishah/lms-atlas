# ui_summary.R — Dashboard tab UI

ui_summary <- function() {
  div(
    class = "p-4",
    h4("Dashboard", class = "fw-bold mb-4"),

    # ── Stat tiles ──────────────────────────────────────────────────────────
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      class      = "mb-4",
      bslib::value_box(
        title = "TOTAL SLIDES",
        value = uiOutput("summary_slide_count"),
        uiOutput("summary_dataset_label"),
        theme = bslib::value_box_theme(bg = "#0D9488", fg = "#fff"),
        class = "total-tile"
      ),
      bslib::value_box(
        title = "FAVORABLE",
        value = uiOutput("summary_favorable_count"),
        uiOutput("summary_favorable_label"),
        theme = bslib::value_box_theme(bg = "#166534", fg = "#fff")
      ),
      bslib::value_box(
        title = "ADVERSE",
        value = uiOutput("summary_adverse_count"),
        uiOutput("summary_adverse_label"),
        theme = bslib::value_box_theme(bg = "#991B1B", fg = "#fff")
      ),
      bslib::value_box(
        title = "TRAINING RUNS",
        value = "5",
        p("Independent seeds", class = "mb-0 opacity-75 fs-xs"),
        theme = bslib::value_box_theme(bg = "#3B528B", fg = "#fff")
      )
    ),

    # ── Charts ───────────────────────────────────────────────────────────────
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      class      = "mb-4",
      bslib::card(
        bslib::card_header("Outcome Distribution"),
        bslib::card_body(padding = "0.5rem",
          plotly::plotlyOutput("summary_plot_outcome", height = "220px")
        )
      ),
      bslib::card(
        bslib::card_header("Primary Site"),
        bslib::card_body(padding = "0.5rem",
          plotly::plotlyOutput("summary_plot_site", height = "220px")
        )
      ),
      bslib::card(
        bslib::card_header("Age at Diagnosis"),
        bslib::card_body(padding = "0.5rem",
          plotly::plotlyOutput("summary_plot_age", height = "220px")
        )
      )
    ),

    # ── Analysis Pipeline ─────────────────────────────────────────────────────
    h5("Analysis Pipeline", class = "fw-semibold mb-3 mt-2"),
    bslib::card(
      bslib::card_body(
        class = "p-4",
        div(
          class = "pipeline-flow",
          div(class = "pipeline-step", onclick = "lmsPipelineClick(0)",
              div(class = "pipeline-icon", HTML("&#x1F52C;")),
              div(class = "pipeline-label", "H&E WSIs"),
              div(class = "pipeline-sub", "LMS slides")),
          div(class = "pipeline-arrow", "→"),
          div(class = "pipeline-step", onclick = "lmsPipelineClick(1)",
              div(class = "pipeline-icon", HTML("&#x2702;")),
              div(class = "pipeline-label", "Patch Extraction"),
              div(class = "pipeline-sub", "256×256 px · 20× · CLAM")),
          div(class = "pipeline-arrow", "→"),
          div(class = "pipeline-step", onclick = "lmsPipelineClick(2)",
              div(class = "pipeline-icon", HTML("&#x1F9EC;")),
              div(class = "pipeline-label", "Feature Extraction"),
              div(class = "pipeline-sub", "UNI2-h · 1536-dim")),
          div(class = "pipeline-arrow", "→"),
          div(class = "pipeline-step", onclick = "lmsPipelineClick(3)",
              div(class = "pipeline-icon", HTML("&#x2699;")),
              div(class = "pipeline-label", "ABMIL Training"),
              div(class = "pipeline-sub", "CLAM · 5 independent seeds")),
          div(class = "pipeline-arrow", "→"),
          div(class = "pipeline-step", onclick = "lmsPipelineClick(4)",
              div(class = "pipeline-icon", HTML("&#x1F5FA;")),
              div(class = "pipeline-label", "Attention Maps"),
              div(class = "pipeline-sub", "Patch scores · Heatmaps"))
        ),
        p(class = "text-muted fs-xs mt-2 mb-0 text-center",
          HTML("&#x2139;&#xFE0F; Click any step to learn more")),
        tags$div(id = "pipeline-detail-panel", hidden = TRUE)
      )
    ),

    # ── About the Data ────────────────────────────────────────────────────────
    h5("About the Data", class = "fw-semibold mb-3 mt-4"),
    uiOutput("summary_about_data")
  )
}
