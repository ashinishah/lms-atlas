# server_gallery.R — Gallery tab server logic

server_gallery <- function(input, output, session, state, metadata, accent) {

  # ── Filtered + sorted slides ───────────────────────────────────────────────
  slides_filtered <- reactive({
    df <- filter_dataset(metadata(), state$selected_dataset)

    # Search filter
    if (nzchar(input$gallery_search %||% "")) {
      df <- df[grepl(input$gallery_search, df$slide_id, ignore.case = TRUE), ]
    }

    # Outcome filter
    if (nzchar(input$gallery_outcome_filter %||% "")) {
      df <- df[df$outcome == input$gallery_outcome_filter, ]
    }

    # Sort
    sort_by <- input$gallery_sort %||% "slide_id"
    df <- switch(sort_by,
      "slide_id"            = df[order(df$slide_id), ],
      "max_attention_desc"  = df[order(-df$max_attention), ],
      "max_attention_asc"   = df[order(df$max_attention), ],
      "age"                 = df[order(df$age), ],
      df
    )
    df
  })

  output$gallery_slide_count_badge <- renderUI({
    n <- nrow(slides_filtered())
    sprintf("%d slides", n)
  })

  # ── Card grid ──────────────────────────────────────────────────────────────
  output$gallery_grid <- renderUI({
    df <- slides_filtered()
    if (nrow(df) == 0) {
      return(div(class = "text-muted p-4", "No slides match the current filters."))
    }

    cards <- lapply(seq_len(nrow(df)), function(i) {
      row  <- df[i, ]
      sid  <- row$slide_id
      btn_id <- paste0("gallery_select_", gsub("[^A-Za-z0-9]", "_", sid))

      div(
        class = "col-sm-6 col-md-4 col-lg-3 mb-3",
        div(
          class   = "slide-card",
          onclick = sprintf(
            "Shiny.setInputValue('gallery_selected_slide', '%s', {priority: 'event'});", sid
          ),
          # Thumbnail
          tags$img(
            src   = thumbnail_url(sid),
            alt   = sid,
            onerror = "this.src='https://placehold.co/300x140/e2e8f0/94a3b8?text=No+image';"
          ),
          # Card body
          div(
            class = "card-body",
            div(class = "fw-semibold text-truncate", sid),
            div(
              class = "d-flex align-items-center justify-content-between mt-1",
              outcome_badge(row$outcome),
              span(class = "text-muted fs-xs",
                   sprintf("attn %.2f", row$max_attention))
            ),
            div(class = "text-muted fs-xs mt-1",
                sprintf("Age %s · %s", row$age, row$site))
          )
        )
      )
    })

    div(class = "row", cards)
  })

  # ── Handle card click → switch to Inspect tab ──────────────────────────────
  observeEvent(input$gallery_selected_slide, {
    state$selected_slide_id <- input$gallery_selected_slide
    updateNavbarPage(session, "main_nav", selected = "Inspect")
  })
}

# Null-coalescing helper (base R doesn't have %||%)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
