# ui_summary.R — Summary tab UI

ui_summary <- function() {
  nav_panel_body(
    class = "p-4",
    h4("Summary", class = "mb-3"),
    p("Dataset-level statistics, outcome distribution, and model performance metrics.",
      class = "text-muted"),

    fluidRow(
      # Stat tiles
      col_4(
        card(
          card_header("Slides"),
          card_body(uiOutput("summary_slide_count"))
        )
      ),
      col_4(
        card(
          card_header("Favorable"),
          card_body(uiOutput("summary_favorable_count"))
        )
      ),
      col_4(
        card(
          card_header("Adverse"),
          card_body(uiOutput("summary_adverse_count"))
        )
      )
    ),

    fluidRow(
      class = "mt-3",
      col_12(
        card(
          card_header("Outcome Distribution"),
          card_body(plotly::plotlyOutput("summary_outcome_plot", height = "280px"))
        )
      )
    )
  )
}
