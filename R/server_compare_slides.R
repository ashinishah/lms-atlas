# server_compare_slides.R — Side-by-side slide comparison server logic

server_compare_slides <- function(input, output, session, state, metadata, accent) {

  all_slides <- reactive({
    df <- metadata()
    setNames(df$slide_id, paste0(short_id(df$slide_id), " · ", df$outcome))
  })

  # ── Slide pickers ──────────────────────────────────────────────────────────
  output$compare_slides_picker_a <- renderUI({
    selectInput(
      "compare_pick_a", label = NULL,
      choices  = c("— select a slide —" = "", all_slides()),
      selected = state$compare_slide_a %||% "",
      width    = "100%"
    )
  })

  output$compare_slides_picker_b <- renderUI({
    selectInput(
      "compare_pick_b", label = NULL,
      choices  = c("— select a slide —" = "", all_slides()),
      selected = state$compare_slide_b %||% "",
      width    = "100%"
    )
  })

  observeEvent(input$compare_pick_a, {
    state$compare_slide_a <- input$compare_pick_a
    shinyjs::runjs("lmsZoomReset('compare_canvas_a');")
  })
  observeEvent(input$compare_pick_b, {
    state$compare_slide_b <- input$compare_pick_b
    shinyjs::runjs("lmsZoomReset('compare_canvas_b');")
  })

  # ── Image rendering ────────────────────────────────────────────────────────
  slide_image_ui <- function(sid) {
    if (is.null(sid) || sid == "") {
      return(div(
        class = "compare-empty",
        icon("image", class = "fa-2x text-muted mb-2"),
        p("Select a slide above", class = "text-muted fs-xs")
      ))
    }
    mode    <- state$inspect_mode
    img_src <- if (mode == "heatmap") heatmap_url(sid) else thumbnail_url(sid)
    tags$img(
      src     = img_src,
      alt     = sid,
      class   = "compare-wsi-img",
      onerror = "this.src='https://placehold.co/600x400/0f172a/94a3b8?text=No+image';"
    )
  }

  slide_info_ui <- function(sid, meta) {
    if (is.null(sid) || sid == "") return(NULL)
    row <- meta[meta$slide_id == sid, ]
    if (nrow(row) == 0) return(NULL)

    div(
      class = "compare-info-row",
      div(
        class = "compare-info-cell",
        div(class = "slide-label", "ID"),
        div(class = "slide-value fs-xs", short_id(sid))
      ),
      div(
        class = "compare-info-cell",
        div(class = "slide-label", "Outcome"),
        div(class = "slide-value", outcome_badge(row$outcome))
      ),
      div(
        class = "compare-info-cell",
        div(class = "slide-label", "Age"),
        div(class = "slide-value", paste(row$age, "yrs"))
      ),
      div(
        class = "compare-info-cell",
        div(class = "slide-label", "Site"),
        div(class = "slide-value fs-xs", row$site)
      ),
      div(
        class = "compare-info-cell",
        div(class = "slide-label", "Attn (mean)"),
        div(class = "slide-value", if (is.na(row$mean_attention)) "—" else sprintf("%.3f", row$mean_attention))
      )
    )
  }

  output$compare_slide_image_a <- renderUI({ slide_image_ui(state$compare_slide_a) })
  output$compare_slide_image_b <- renderUI({ slide_image_ui(state$compare_slide_b) })

  output$compare_slide_info_a <- renderUI({ slide_info_ui(state$compare_slide_a, metadata()) })
  output$compare_slide_info_b <- renderUI({ slide_info_ui(state$compare_slide_b, metadata()) })
}
