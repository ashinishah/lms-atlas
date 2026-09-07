# server_byoutcome.R — By Outcome tab server logic

server_byoutcome <- function(input, output, session, state, metadata, accent) {

  # ── Patch metadata ─────────────────────────────────────────────────────────
  pm_path <- patch_metadata_path()
  all_patches <- if (file.exists(pm_path)) {
    readr::read_csv(pm_path, show_col_types = FALSE)
  } else NULL

  # True level-0 extents from SVS file dimensions (slide_dimensions.csv).
  slide_ext <- slide_extents()

  # ── Filtered slides ────────────────────────────────────────────────────────
  meta_filtered <- reactive({ filter_dataset(metadata(), state$selected_dataset) })

  favorable_slides <- reactive({
    meta_filtered()[meta_filtered()$outcome == "Favorable", ]
  })
  adverse_slides <- reactive({
    meta_filtered()[meta_filtered()$outcome == "Adverse", ]
  })

  # Patches for a set of slide IDs
  outcome_patches <- function(slide_ids) {
    if (is.null(all_patches) || length(slide_ids) == 0) return(NULL)
    df <- all_patches[all_patches$slide_id %in% slide_ids, ]
    df[order(-df$attention_mean), ]
  }

  # ── Count labels ───────────────────────────────────────────────────────────
  make_count_label <- function(slides_df) {
    mode <- state$inspect_mode
    if (!mode %in% c("recurring", "topk")) {
      return(span(sprintf("n = %d slides", nrow(slides_df)), class = "text-muted fs-xs"))
    }
    patches <- outcome_patches(slides_df$slide_id)
    if (is.null(patches)) return(span("no data", class = "text-muted fs-xs"))
    if (mode == "topk") {
      n <- min(state$topk_k, nrow(patches))
      span(sprintf("top %d patches", n), class = "text-muted fs-xs")
    } else {
      n <- min(state$recurring_k, sum(patches$n_seeds_top >= 3))
      span(sprintf("top %d recurring patches", n), class = "text-muted fs-xs")
    }
  }

  output$byoutcome_favorable_count <- renderUI({ make_count_label(favorable_slides()) })
  output$byoutcome_adverse_count   <- renderUI({ make_count_label(adverse_slides()) })

  # ── Slide grid (plain WSI / heatmap modes) ─────────────────────────────────
  slide_grid <- function(df) {
    if (nrow(df) == 0) return(div(class = "text-muted", "No slides."))
    mode  <- state$inspect_mode
    cards <- lapply(seq_len(nrow(df)), function(i) {
      row     <- df[i, ]
      sid     <- row$slide_id
      img_src <- if (mode == "heatmap") heatmap_url(sid) else thumbnail_url(sid)
      div(
        class   = "col-6 mb-2",
        div(
          class   = "slide-card",
          onclick = sprintf(
            "Shiny.setInputValue('byoutcome_selected_slide','%s',{priority:'event'});", sid
          ),
          tags$img(
            src     = img_src,
            alt     = sid,
            loading = "lazy",
            onerror = "this.src='https://placehold.co/300x140/e2e8f0/94a3b8?text=No+image';"
          ),
          div(
            class = "card-body",
            div(class = "fw-semibold text-truncate flex-grow-1 fs-xs", short_id(sid)),
            outcome_badge(row$outcome)
          )
        )
      )
    })
    div(class = "row", cards)
  }

  # ── Patch grid (recurring / topk modes) ────────────────────────────────────
  patch_grid <- function(slide_ids) {
    if (is.null(all_patches) || length(slide_ids) == 0)
      return(div(class = "text-muted p-3 fs-xs", "No patch data."))

    mode <- state$inspect_mode
    df   <- outcome_patches(slide_ids)
    if (is.null(df) || nrow(df) == 0)
      return(div(class = "text-muted p-3 fs-xs", "No patches found."))

    if (mode == "topk") {
      k  <- state$topk_k
      df <- df[seq_len(min(k, nrow(df))), ]
    } else {
      df <- df[df$n_seeds_top >= 3, ]
      df <- df[order(-df$recurrence_count, -df$attention_mean), ]
      df <- df[seq_len(min(state$recurring_k, nrow(df))), ]
    }

    if (nrow(df) == 0)
      return(div(class = "text-muted p-3 fs-xs", "No patches found."))

    ex <- slide_ext
    cards <- lapply(seq_len(nrow(df)), function(i) {
      row      <- df[i, ]
      sid      <- row$slide_id
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
          "Shiny.setInputValue('byoutcome_patch_clicked','%s|%s|%d|%d|%d|%d',{priority:'event'});",
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
          div(
            class = "byoutcome-patch-text",
            div(class = "fw-semibold text-truncate", short_id(sid)),
            div(class = "text-muted text-truncate", sub(".*-", "", patch_id))
          ),
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

    div(class = "byoutcome-patch-grid", cards)
  }

  # ── Grid outputs ───────────────────────────────────────────────────────────
  output$byoutcome_favorable_grid <- renderUI({
    mode <- state$inspect_mode
    if (mode %in% c("recurring", "topk")) {
      patch_grid(favorable_slides()$slide_id)
    } else {
      slide_grid(favorable_slides())
    }
  })

  output$byoutcome_adverse_grid <- renderUI({
    mode <- state$inspect_mode
    if (mode %in% c("recurring", "topk")) {
      patch_grid(adverse_slides()$slide_id)
    } else {
      slide_grid(adverse_slides())
    }
  })

  # ── Slide click → Inspect ─────────────────────────────────────────────────
  observeEvent(input$byoutcome_selected_slide, {
    state$selected_slide_id <- input$byoutcome_selected_slide
    shinyjs::runjs("lmsNavigate('inspect');")
  })

  # ── Patch click → Inspect + zoom ──────────────────────────────────────────
  observeEvent(input$byoutcome_patch_clicked, {
    parts <- strsplit(input$byoutcome_patch_clicked, "\\|")[[1]]
    if (length(parts) < 6) return()
    sid <- parts[1]
    pid <- parts[2]
    cx  <- as.integer(parts[3])
    cy  <- as.integer(parts[4])
    ew  <- as.integer(parts[5])
    eh  <- as.integer(parts[6])

    state$selected_slide_id <- sid
    state$highlighted_patch <- pid

    shinyjs::runjs(sprintf(paste0(
      "lmsNavigate('inspect');",
      "lmsZoomAfterLoad('wsi_canvas',%d,%d,%d,%d);",
      "setTimeout(function(){lmsScrollToPatch('%s');},600);"
    ), cx, cy, ew, eh, pid))
  })
}
