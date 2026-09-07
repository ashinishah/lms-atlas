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

  output$summary_favorable_label <- renderUI({
    lbl <- if (state$selected_dataset == "TCGA") "PFI > 2 years" else "Treatment response"
    p(lbl, class = "mb-0 opacity-75 fs-xs")
  })

  output$summary_adverse_label <- renderUI({
    lbl <- if (state$selected_dataset == "TCGA") "PFI ≤ 2 years" else "No response / progression"
    p(lbl, class = "mb-0 opacity-75 fs-xs")
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
    colors <- c(Adverse = "#C0392B", Favorable = "#1A7D45")

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

    # Viridis-anchored palette: purple → teal (brand) → gold (brand)
    site_palette <- c("#440154", "#3B528B", "#21908C", "#35B779", "#8FD744", "#FDE725", "#C9A227")
    site_colors  <- site_palette[seq_len(min(nrow(counts), length(site_palette)))]

    plotly::plot_ly(
      data          = counts,
      labels        = ~Site,
      values        = ~Freq,
      type          = "pie",
      hole          = 0.42,
      marker        = list(colors = site_colors,
                           line   = list(color = "#fff", width = 2)),
      textinfo      = "none",
      hovertemplate = "%{label}: %{value} (%{percent:.0%})<extra></extra>"
    ) |> pie_layout()
  })

  # ── About the Data (dataset-reactive) ─────────────────────────────────────
  output$summary_about_data <- renderUI({
    ds <- state$selected_dataset
    if (ds == "TCGA") {
      items <- list(
        list("Cohort",   "TCGA-SARC · Leiomyosarcoma (LMS) subtype"),
        list("Slides",   "88 H&E whole-slide images"),
        list("Outcome",  "Progression-free interval (PFI): Favorable = PFI > 2 yrs, Adverse = PFI ≤ 2 yrs"),
        list("Source",   "Publicly available via GDC / TCGA Data Portal"),
        list("Scanner",  "Multiple vendors; scanned at variable magnifications, normalized to 20×")
      )
    } else {
      items <- list(
        list("Cohort",   "SPORE · Sarcoma SPORE institutional cohort"),
        list("Slides",   "24 H&E whole-slide images"),
        list("Outcome",  "Treatment response: Favorable = response to treatment, Adverse = no response or disease progression"),
        list("Source",   "Internal institutional collection"),
        list("Scanner",  "Aperio GT450; scanned at 40×, downsampled to 20×")
      )
    }
    bslib::card(
      bslib::card_body(
        class = "p-3",
        tags$dl(
          class = "row mb-0 fs-xs",
          tagList(lapply(items, function(x) {
            tagList(
              tags$dt(class = "col-3 text-muted fw-normal", x[[1]]),
              tags$dd(class = "col-9", x[[2]])
            )
          }))
        )
      )
    )
  })

  # ── Age distribution pie ───────────────────────────────────────────────────
  output$summary_plot_age <- plotly::renderPlotly({
    df  <- meta_filtered()
    age <- as.numeric(df$age)
    cat <- cut(age,
               breaks = c(-Inf, 40, 50, 60, 70, Inf),
               labels = c("<40", "40–49", "50–59", "60–69", "70+"),
               right  = FALSE)
    # Keep all 5 levels in chronological order even if a bin is empty
    bin_levels <- c("<40", "40–49", "50–59", "60–69", "70+")
    cat        <- factor(cat, levels = bin_levels)
    counts     <- as.data.frame(table(Age = cat))
    counts$Age <- factor(counts$Age, levels = bin_levels)
    counts     <- counts[order(counts$Age), ]

    # Five evenly-spaced viridis points — maximum contrast across bins
    age_colors <- c("#440154", "#31688E", "#21908C", "#35B779", "#FDE725")

    plotly::plot_ly(
      data          = counts,
      labels        = ~Age,
      values        = ~Freq,
      type          = "pie",
      hole          = 0.42,
      sort          = FALSE,
      marker        = list(colors = age_colors,
                           line   = list(color = "#fff", width = 2)),
      textinfo      = "none",
      hovertemplate = "%{label}: %{value} slides (%{percent:.0%})<extra></extra>"
    ) |> pie_layout()
  })
}
