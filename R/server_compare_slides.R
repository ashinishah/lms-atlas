# server_compare_slides.R — Side-by-side slide comparison server logic

server_compare_slides <- function(input, output, session, state, metadata, accent) {

  # ── Patch metadata ─────────────────────────────────────────────────────────
  pm_path <- patch_metadata_path()
  all_patches <- if (file.exists(pm_path)) {
    readr::read_csv(pm_path, show_col_types = FALSE)
  } else NULL

  # True level-0 extents from SVS file dimensions (slide_dimensions.csv).
  slide_ext <- slide_extents()

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

  # ── Patch grid for a single slide ─────────────────────────────────────────
  patch_grid_for_slide <- function(sid) {
    if (is.null(all_patches))
      return(div(class = "text-muted p-3 fs-xs", "No patch data available."))

    mode <- state$inspect_mode
    df   <- all_patches[all_patches$slide_id == sid, ]

    if (mode == "topk") {
      df <- df[order(-df$attention_mean), ]
      df <- df[seq_len(min(state$topk_k, nrow(df))), ]
    } else {
      df <- df[df$n_seeds_top >= 3, ]
      if (nrow(df) == 0)
        return(div(
          style = "position: absolute; inset: 0; display: flex; align-items: center; justify-content: center; background: var(--bs-body-bg, #F8F9FA);",
          div(class = "text-muted fs-xs text-center",
              icon("info-circle", class = "me-1 d-block mb-1"),
              "No recurring patches for this slide.")
        ))
      df <- df[order(-df$recurrence_count, -df$attention_mean), ]
      df <- df[seq_len(min(state$recurring_k, nrow(df))), ]
    }

    if (nrow(df) == 0)
      return(div(class = "text-muted p-3 fs-xs", "No patches found."))

    ex <- slide_ext
    cards <- lapply(seq_len(nrow(df)), function(i) {
      row      <- df[i, ]
      patch_id <- row$patch_id
      score    <- row$attention_mean
      cx       <- as.integer(row$x_coord) + 128L
      cy       <- as.integer(row$y_coord) + 128L
      ew <- if (!is.null(ex) && sid %in% names(ex$x)) as.integer(ex$x[[sid]]) else 40000L
      eh <- if (!is.null(ex) && sid %in% names(ex$y)) as.integer(ex$y[[sid]]) else 40000L

      bg_color  <- attention_marker_color(score)
      txt_color <- attention_marker_text(score)

      div(
        class   = "byoutcome-patch-card",
        onclick = sprintf(
          "Shiny.setInputValue('compare_slides_patch_clicked','%s|%s|%d|%d|%d|%d',{priority:'event'});",
          sid, patch_id, cx, cy, ew, eh
        ),
        div(
          class = "patch-img-wrap",
          tags$img(
            src     = patch_url(sid, patch_id),
            alt     = patch_id,
            loading = "lazy",
            onerror = "this.src='https://placehold.co/134x134/f8f9fa/94a3b8?text=patch';"
          )
        ),
        div(
          class = "byoutcome-patch-meta",
          div(class = "byoutcome-patch-text text-muted text-truncate",
              sub(".*-", "", patch_id)),
          if (mode == "recurring") {
            tags$span(class = "patch-meta-badge",
                      style = sprintf("background:%s; color:%s;",
                                      recurrence_marker_color(row$recurrence_count),
                                      recurrence_marker_text(row$recurrence_count)),
                      paste0("×", row$recurrence_count))
          } else {
            tags$span(class = "patch-meta-badge",
                      style = sprintf("background:%s; color:%s;", bg_color, txt_color),
                      sprintf("%.2f", score))
          }
        )
      )
    })

    div(
      style = "position: absolute; inset: 0; overflow-y: auto; background: var(--bs-body-bg, #F8F9FA); padding: 8px;",
      div(class = "byoutcome-patch-grid", cards)
    )
  }

  # ── Image / patch rendering ────────────────────────────────────────────────
  slide_image_ui <- function(sid) {
    if (is.null(sid) || sid == "") {
      return(div(
        class = "compare-empty",
        icon("image", class = "fa-2x text-muted mb-2"),
        p("Select a slide above", class = "text-muted fs-xs")
      ))
    }
    mode <- state$inspect_mode
    if (mode %in% c("recurring", "topk")) {
      return(patch_grid_for_slide(sid))
    }
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
      )
    )
  }

  output$compare_slide_image_a <- renderUI({ slide_image_ui(state$compare_slide_a) })
  output$compare_slide_image_b <- renderUI({ slide_image_ui(state$compare_slide_b) })

  output$compare_slide_info_a <- renderUI({ slide_info_ui(state$compare_slide_a, metadata()) })
  output$compare_slide_info_b <- renderUI({ slide_info_ui(state$compare_slide_b, metadata()) })

  # ── Patch click → switch dataset if needed → Inspect + zoom ───────────────
  observeEvent(input$compare_slides_patch_clicked, {
    parts <- strsplit(input$compare_slides_patch_clicked, "\\|")[[1]]
    if (length(parts) < 6) return()
    sid <- parts[1]
    pid <- parts[2]
    cx  <- as.integer(parts[3])
    cy  <- as.integer(parts[4])
    ew  <- as.integer(parts[5])
    eh  <- as.integer(parts[6])

    # Switch dataset to match the clicked slide
    meta <- metadata()
    row  <- meta[meta$slide_id == sid, ]
    if (nrow(row) > 0 && !is.null(row$dataset)) {
      state$selected_dataset <- row$dataset[1]
    }

    state$selected_slide_id <- sid
    state$highlighted_patch <- pid

    shinyjs::runjs(sprintf(paste0(
      "lmsNavigate('inspect');",
      "lmsZoomAfterLoad('wsi_canvas',%d,%d,%d,%d);",
      "setTimeout(function(){lmsScrollToPatch('%s');},600);"
    ), cx, cy, ew, eh, pid))
  })
}
