# server_gallery.R — Gallery tab server logic

server_gallery <- function(input, output, session, state, metadata, accent) {

  # ── Filtered + sorted slides ───────────────────────────────────────────────
  slides_filtered <- reactive({
    df <- filter_dataset(metadata(), state$selected_dataset)

    if (nzchar(input$gallery_search %||% "")) {
      df <- df[grepl(input$gallery_search, df$slide_id, ignore.case = TRUE), ]
    }

    if (nzchar(input$gallery_outcome_filter %||% "")) {
      df <- df[df$outcome == input$gallery_outcome_filter, ]
    }

    sort_by <- input$gallery_sort %||% "slide_id"
    df <- switch(sort_by,
      "slide_id"           = df[order(df$slide_id), ],
      "max_attention_desc" = df[order(-df$max_attention), ],
      "max_attention_asc"  = df[order(df$max_attention), ],
      "age"                = df[order(df$age), ],
      df
    )
    df
  })

  output$gallery_slide_count_badge <- renderUI({
    sprintf("%d slides", nrow(slides_filtered()))
  })

  # ── Card grid ──────────────────────────────────────────────────────────────
  output$gallery_grid <- renderUI({
    df <- slides_filtered()
    if (nrow(df) == 0) {
      return(div(class = "text-muted p-4", "No slides match the current filters."))
    }

    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      sid <- row$slide_id

      div(
        class = "col-sm-6 col-md-4 col-lg-3 mb-3",
        div(
          class   = "slide-card",
          onclick = sprintf(
            "Shiny.setInputValue('gallery_selected_slide', '%s', {priority: 'event'});", sid
          ),
          tags$img(
            src     = thumbnail_url(sid),
            alt     = sid,
            onerror = "this.src='https://placehold.co/300x140/e2e8f0/94a3b8?text=No+image';"
          ),
          div(
            class = "card-body",
            div(class = "fw-semibold text-truncate", short_id(sid)),
            div(
              class = "mt-1",
              outcome_badge(row$outcome)
            )
          )
        )
      )
    })

    div(class = "row", cards)
  })

  # ── Handle card click → switch to Inspect tab ──────────────────────────────
  observeEvent(input$gallery_selected_slide, {
    state$selected_slide_id <- input$gallery_selected_slide
    shinyjs::runjs("lmsNavigate('inspect');")
  })
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
