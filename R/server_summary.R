# server_summary.R — Summary tab server logic

server_summary <- function(input, output, session, state, metadata, accent) {

  # Filtered metadata for current dataset
  meta_filtered <- reactive({
    filter_dataset(metadata(), state$selected_dataset)
  })

  output$summary_slide_count <- renderUI({
    h2(nrow(meta_filtered()), class = "text-accent mb-0")
  })

  output$summary_favorable_count <- renderUI({
    n <- sum(meta_filtered()$outcome == "Favorable")
    h2(n, class = "text-success mb-0")
  })

  output$summary_adverse_count <- renderUI({
    n <- sum(meta_filtered()$outcome == "Adverse")
    h2(n, class = "text-danger mb-0")
  })

  output$summary_outcome_plot <- plotly::renderPlotly({
    df <- meta_filtered()
    counts <- table(df$outcome)
    plotly::plot_ly(
      x      = names(counts),
      y      = as.integer(counts),
      type   = "bar",
      marker = list(color = c("#22C55E", "#EF4444"))  # Favorable green, Adverse red
    ) |>
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Slides"),
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
}
