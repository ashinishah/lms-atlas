# server_compare.R — TCGA vs SPORE cross-dataset tab server logic
# Both datasets shown simultaneously; dataset toggle in header is grayed out.

server_compare <- function(input, output, session, state, metadata, accent) {

  # ── Patch metadata ─────────────────────────────────────────────────────────
  pm_path <- patch_metadata_path()
  all_patches <- if (file.exists(pm_path)) {
    readr::read_csv(pm_path, show_col_types = FALSE)
  } else NULL

  # True level-0 extents from SVS file dimensions (slide_dimensions.csv).
  slide_ext <- slide_extents()

  # ── Slides filtered by dataset AND selected outcome ────────────────────────
  tcga_slides <- reactive({
    df <- filter_dataset(metadata(), "TCGA")
    df[df$outcome == state$compare_outcome, ]
  })

  spore_slides <- reactive({
    df <- filter_dataset(metadata(), "SPORE")
    df[df$outcome == state$compare_outcome, ]
  })

  # ── Outcome toggle UI ──────────────────────────────────────────────────────
  output$compare_outcome_toggle_ui <- renderUI({
    oc <- state$compare_outcome
    div(
      class = "btn-group btn-group-sm",
      tags$button(
        class   = paste0("btn", if (oc == "Favorable") " btn-success" else " btn-outline-secondary"),
        style   = if (oc == "Favorable") "background:#166534; border-color:#166534;" else "",
        onclick = "Shiny.setInputValue('compare_outcome_toggle','Favorable',{priority:'event'});",
        "Favorable"
      ),
      tags$button(
        class   = paste0("btn", if (oc == "Adverse") " btn-danger" else " btn-outline-secondary"),
        style   = if (oc == "Adverse") "background:#991B1B; border-color:#991B1B;" else "",
        onclick = "Shiny.setInputValue('compare_outcome_toggle','Adverse',{priority:'event'});",
        "Adverse"
      )
    )
  })

  # ── Count labels ───────────────────────────────────────────────────────────
  make_count_label <- function(slides_df) {
    mode <- state$inspect_mode
    if (!mode %in% c("recurring", "topk")) {
      return(span(sprintf("(%d slides)", nrow(slides_df)), class = "text-muted fs-xs"))
    }
    if (is.null(all_patches) || nrow(slides_df) == 0)
      return(span("no data", class = "text-muted fs-xs"))
    df <- all_patches[all_patches$slide_id %in% slides_df$slide_id, ]
    if (mode == "topk") {
      n <- min(state$topk_k, nrow(df))
      span(sprintf("top %d patches", n), class = "text-muted fs-xs")
    } else {
      n <- min(state$recurring_k, sum(df$n_seeds_top >= 3))
      span(sprintf("top %d recurring patches", n), class = "text-muted fs-xs")
    }
  }

  output$compare_tcga_count  <- renderUI({ make_count_label(tcga_slides()) })
  output$compare_spore_count <- renderUI({ make_count_label(spore_slides()) })

  # ── Slide grid (plain / heatmap modes) ────────────────────────────────────
  mini_grid <- function(df, dataset) {
    if (nrow(df) == 0) return(div(class = "text-muted", "No slides."))
    accent_color <- if (dataset == "TCGA") "#0D9488" else "#C9A227"
    mode <- state$inspect_mode

    cards <- lapply(seq_len(nrow(df)), function(i) {
      row     <- df[i, ]
      sid     <- row$slide_id
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

  # ── Patch grid (recurring / topk modes) ───────────────────────────────────
  patch_grid <- function(slide_ids) {
    if (is.null(all_patches) || length(slide_ids) == 0)
      return(div(class = "text-muted p-3 fs-xs", "No patch data."))

    mode <- state$inspect_mode
    df   <- all_patches[all_patches$slide_id %in% slide_ids, ]
    df   <- df[order(-df$attention_mean), ]

    if (nrow(df) == 0)
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
          "Shiny.setInputValue('compare_xds_patch_clicked','%s|%s|%d|%d|%d|%d',{priority:'event'});",
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

  output$compare_tcga_grid <- renderUI({
    if (state$inspect_mode %in% c("recurring", "topk")) {
      patch_grid(tcga_slides()$slide_id)
    } else {
      mini_grid(tcga_slides(), "TCGA")
    }
  })

  output$compare_spore_grid <- renderUI({
    if (state$inspect_mode %in% c("recurring", "topk")) {
      patch_grid(spore_slides()$slide_id)
    } else {
      mini_grid(spore_slides(), "SPORE")
    }
  })

  observeEvent(input$compare_selected_slide, {
    state$selected_slide_id <- input$compare_selected_slide
    clicked_meta <- metadata()[metadata()$slide_id == input$compare_selected_slide, ]
    if (nrow(clicked_meta) > 0) {
      state$selected_dataset <- clicked_meta$dataset[1]
    }
    shinyjs::runjs("lmsNavigate('inspect');")
  })

  # ── Patch click → switch dataset if needed → Inspect + zoom ───────────────
  observeEvent(input$compare_xds_patch_clicked, {
    parts <- strsplit(input$compare_xds_patch_clicked, "\\|")[[1]]
    if (length(parts) < 6) return()
    sid <- parts[1]
    pid <- parts[2]
    cx  <- as.integer(parts[3])
    cy  <- as.integer(parts[4])
    ew  <- as.integer(parts[5])
    eh  <- as.integer(parts[6])

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
