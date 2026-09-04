# server_summary.R — Summary tab server logic

server_summary <- function(input, output, session, state, metadata, accent) {

  meta_filtered <- reactive({
    filter_dataset(metadata(), state$selected_dataset)
  })

  # ── Stat tiles ─────────────────────────────────────────────────────────────
  # Total tile accent color is driven by CSS var(--accent) via .total-tile class

  output$summary_slide_count <- renderUI({
    tags$span(nrow(meta_filtered()))
  })

  output$summary_favorable_count <- renderUI({
    tags$span(sum(meta_filtered()$outcome == "Favorable"))
  })

  output$summary_adverse_count <- renderUI({
    tags$span(sum(meta_filtered()$outcome == "Adverse"))
  })

  output$summary_dataset_label <- renderUI({
    p(sprintf("%s · LMS subtype", state$selected_dataset),
      class = "mb-0 opacity-75 fs-xs")
  })

  # ── Shared plotly layout helper ─────────────────────────────────────────────
  pie_layout <- function(p) {
    p |> plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin        = list(l = 5, r = 5, t = 5, b = 5),
      legend        = list(orientation = "h", x = 0.5, xanchor = "center",
                           y = -0.05, font = list(size = 10))
    )
  }

  # ── Outcome pie ────────────────────────────────────────────────────────────
  output$summary_plot_outcome <- plotly::renderPlotly({
    df     <- meta_filtered()
    counts <- as.data.frame(table(Outcome = df$outcome))
    colors <- c(Adverse = "#991B1B", Favorable = "#166534")

    plotly::plot_ly(
      data          = counts,
      labels        = ~Outcome,
      values        = ~Freq,
      type          = "pie",
      hole          = 0.42,
      marker        = list(colors = unname(colors[as.character(counts$Outcome)]),
                           line   = list(color = "#fff", width = 2)),
      textinfo      = "none",
      hovertemplate = "%{label}: %{value} slides (%{percent:.0%})<extra></extra>"
    ) |> pie_layout()
  })

  # ── Primary site pie ───────────────────────────────────────────────────────
  output$summary_plot_site <- plotly::renderPlotly({
    df  <- meta_filtered()
    raw <- trimws(tolower(df$site))
    site <- dplyr::case_when(
      grepl("uterus|uteri",                 raw) ~ "Uterus",
      grepl("retroperiton",                 raw) ~ "Retroperitoneum",
      grepl("thigh|femur|leg|extremit|limb",raw) ~ "Extremity",
      grepl("abdom",                        raw) ~ "Abdomen",
      grepl("pelvi",                        raw) ~ "Pelvis",
      grepl("chest|thorax|lung",            raw) ~ "Thorax",
      .default = "Other"
    )
    counts <- as.data.frame(table(Site = site))
    counts <- counts[order(-counts$Freq), ]

    plotly::plot_ly(
      data          = counts,
      labels        = ~Site,
      values        = ~Freq,
      type          = "pie",
      hole          = 0.42,
      marker        = list(line = list(color = "#fff", width = 2)),
      textinfo      = "none",
      hovertemplate = "%{label}: %{value} (%{percent:.0%})<extra></extra>"
    ) |> pie_layout()
  })

  # ── Age distribution pie ───────────────────────────────────────────────────
  output$summary_plot_age <- plotly::renderPlotly({
    df  <- meta_filtered()
    age <- as.numeric(df$age)
    cat <- cut(age,
               breaks = c(-Inf, 40, 50, 60, 70, Inf),
               labels = c("<40", "40–49", "50–59", "60–69", "70+"),
               right  = FALSE)
    counts <- as.data.frame(table(Age = cat))

    plotly::plot_ly(
      data          = counts,
      labels        = ~Age,
      values        = ~Freq,
      type          = "pie",
      hole          = 0.42,
      marker        = list(colors = c("#1E3A5F","#2563EB","#60A5FA","#93C5FD","#BFDBFE"),
                           line   = list(color = "#fff", width = 2)),
      textinfo      = "none",
      hovertemplate = "%{label}: %{value} slides (%{percent:.0%})<extra></extra>"
    ) |> pie_layout()
  })
}
