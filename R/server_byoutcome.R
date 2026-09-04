# server_byoutcome.R — By Outcome tab server logic

server_byoutcome <- function(input, output, session, state, metadata, accent) {

  meta_filtered <- reactive({
    filter_dataset(metadata(), state$selected_dataset)
  })

  favorable_slides <- reactive({
    meta_filtered()[meta_filtered()$outcome == "Favorable", ]
  })

  adverse_slides <- reactive({
    meta_filtered()[meta_filtered()$outcome == "Adverse", ]
  })

  output$byoutcome_favorable_count <- renderUI({
    sprintf("(%d slides)", nrow(favorable_slides()))
  })

  output$byoutcome_adverse_count <- renderUI({
    sprintf("(%d slides)", nrow(adverse_slides()))
  })

  # Helper to render a mini grid of thumbnails
  mini_grid <- function(df) {
    if (nrow(df) == 0) return(div(class = "text-muted", "No slides."))

    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      sid <- row$slide_id
      div(
        class   = "col-6 mb-2",
        div(
          class   = "slide-card",
          onclick = sprintf(
            "Shiny.setInputValue('byoutcome_selected_slide', '%s', {priority: 'event'});", sid
          ),
          tags$img(
            src     = thumbnail_url(sid),
            alt     = sid,
            onerror = "this.src='https://placehold.co/300x140/e2e8f0/94a3b8?text=No+image';"
          ),
          div(
            class = "card-body",
            div(class = "fw-semibold text-truncate fs-xs", sid),
            div(class = "text-muted fs-xs",
                sprintf("attn %.2f · %s", row$max_attention, row$site))
          )
        )
      )
    })

    div(class = "row", cards)
  }

  output$byoutcome_favorable_grid <- renderUI({ mini_grid(favorable_slides()) })
  output$byoutcome_adverse_grid   <- renderUI({ mini_grid(adverse_slides()) })

  observeEvent(input$byoutcome_selected_slide, {
    state$selected_slide_id <- input$byoutcome_selected_slide
    updateNavbarPage(session, "main_nav", selected = "Inspect")
  })
}
