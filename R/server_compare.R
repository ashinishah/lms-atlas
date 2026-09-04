# server_compare.R — TCGA vs SPORE cross-dataset tab server logic
# Both datasets shown simultaneously; dataset toggle in header is grayed out.

server_compare <- function(input, output, session, state, metadata, accent) {

  tcga_slides  <- reactive({ filter_dataset(metadata(), "TCGA") })
  spore_slides <- reactive({ filter_dataset(metadata(), "SPORE") })

  output$compare_tcga_count <- renderUI({
    sprintf("(%d slides)", nrow(tcga_slides()))
  })

  output$compare_spore_count <- renderUI({
    sprintf("(%d slides)", nrow(spore_slides()))
  })

  mini_grid <- function(df, dataset) {
    if (nrow(df) == 0) return(div(class = "text-muted", "No slides."))
    accent_color <- if (dataset == "TCGA") "#0D9488" else "#D97706"
    mode <- state$inspect_mode

    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      sid <- row$slide_id
      img_src <- if (mode == "heatmap") heatmap_url(sid) else thumbnail_url(sid)

      div(
        class   = "col-6 mb-2",
        div(
          class   = "slide-card",
          style   = sprintf("border-color: %s;", accent_color),
          onclick = sprintf(
            "Shiny.setInputValue('compare_selected_slide', '%s', {priority: 'event'});", sid
          ),
          tags$img(
            src     = img_src,
            alt     = sid,
            onerror = "this.src='https://placehold.co/300x140/e2e8f0/94a3b8?text=No+image';"
          ),
          div(
            class = "card-body",
            div(class = "fw-semibold text-truncate fs-xs", short_id(sid)),
            div(class = "mt-1", outcome_badge(row$outcome))
          )
        )
      )
    })

    div(class = "row", cards)
  }

  output$compare_tcga_grid  <- renderUI({ mini_grid(tcga_slides(),  "TCGA") })
  output$compare_spore_grid <- renderUI({ mini_grid(spore_slides(), "SPORE") })

  observeEvent(input$compare_selected_slide, {
    state$selected_slide_id <- input$compare_selected_slide
    clicked_meta <- metadata()[metadata()$slide_id == input$compare_selected_slide, ]
    if (nrow(clicked_meta) > 0) {
      state$selected_dataset <- clicked_meta$dataset[1]
    }
    shinyjs::runjs("lmsNavigate('inspect');")
  })
}
