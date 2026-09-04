# server_compare.R — TCGA vs SPORE comparison tab server logic
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

  # Helper to render a mini grid
  mini_grid <- function(df, dataset) {
    if (nrow(df) == 0) return(div(class = "text-muted", "No slides."))
    accent_color <- if (dataset == "TCGA") "#0D9488" else "#D97706"

    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      sid <- row$slide_id
      div(
        class   = "col-6 mb-2",
        div(
          class   = "slide-card",
          style   = sprintf("border-color: %s;", accent_color),
          onclick = sprintf(
            "Shiny.setInputValue('compare_selected_slide', '%s', {priority: 'event'});", sid
          ),
          tags$img(
            src     = thumbnail_url(sid),
            alt     = sid,
            onerror = "this.src='https://placehold.co/300x140/e2e8f0/94a3b8?text=No+image';"
          ),
          div(
            class = "card-body",
            div(class = "fw-semibold text-truncate fs-xs", sid),
            div(
              class = "d-flex align-items-center gap-1 mt-1",
              outcome_badge(row$outcome),
              span(class = "text-muted fs-xs ms-auto",
                   sprintf("attn %.2f", row$max_attention))
            )
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
    # Set dataset to match the clicked slide
    clicked_meta <- metadata()[metadata()$slide_id == input$compare_selected_slide, ]
    if (nrow(clicked_meta) > 0) {
      state$selected_dataset <- clicked_meta$dataset[1]
      updateRadioButtons(session, "dataset_toggle", selected = clicked_meta$dataset[1])
    }
    updateNavbarPage(session, "main_nav", selected = "Inspect")
  })
}
