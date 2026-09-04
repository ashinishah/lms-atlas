# ui_summary.R — Summary tab UI

ui_summary <- function() {
  div(
    class = "p-4",
    h4("Project Summary", class = "fw-bold mb-4"),

    # ── Stat tiles ──────────────────────────────────────────────────────────
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      class      = "mb-4",
      # Total tile: static element, --accent CSS var drives the color
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
        p("PFI > 2 years", class = "mb-0 opacity-75 fs-xs"),
        theme = bslib::value_box_theme(bg = "#166534", fg = "#fff")
      ),
      bslib::value_box(
        title = "ADVERSE",
        value = uiOutput("summary_adverse_count"),
        p("PFI ≤ 2 years", class = "mb-0 opacity-75 fs-xs"),
        theme = bslib::value_box_theme(bg = "#991B1B", fg = "#fff")
      ),
      bslib::value_box(
        title = "TRAINING RUNS",
        value = "5",
        p("Independent seeds", class = "mb-0 opacity-75 fs-xs"),
        theme = bslib::value_box_theme(bg = "#1A1A2E", fg = "#fff")
      )
    ),

    # ── Pie charts ───────────────────────────────────────────────────────────
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

    # ── About ────────────────────────────────────────────────────────────────
    bslib::card(
      bslib::card_header("About the Analysis"),
      bslib::card_body(
        class = "p-3",
        tags$dl(
          class = "row mb-0 fs-xs",
          tags$dt(class = "col-3 text-muted fw-normal", "Model"),
          tags$dd(class = "col-9", "Attention-based multiple instance learning (ABMIL)"),
          tags$dt(class = "col-3 text-muted fw-normal", "Training"),
          tags$dd(class = "col-9", "5 independent runs with different random seeds"),
          tags$dt(class = "col-3 text-muted fw-normal", "Recurring regions"),
          tags$dd(class = "col-9", "Patches in top-attended set in ≥ 3 of 5 seeds"),
          tags$dt(class = "col-3 text-muted fw-normal", "Attention score"),
          tags$dd(class = "col-9", "Normalized mean across all runs and WSIs (0–1 scale)"),
          tags$dt(class = "col-3 text-muted fw-normal", "Patch size"),
          tags$dd(class = "col-9", "256 × 256 px at 20× magnification"),
          tags$dt(class = "col-3 text-muted fw-normal", "Data source"),
          tags$dd(class = "col-9", "TCGA-SARC (LMS subtype) & SPORE")
        )
      )
    )
  )
}
