# server_gallery.R — Gallery tab server logic

server_gallery <- function(input, output, session, state, metadata, accent) {

  # Load patch metadata once
  pm_path     <- patch_metadata_path()
  all_patches <- if (file.exists(pm_path))
    readr::read_csv(pm_path, show_col_types = FALSE) else NULL

  # True level-0 extents from SVS file dimensions (slide_dimensions.csv).
  slide_ext <- slide_extents()

  # ── Gallery mode / K from server-side state ────────────────────────────────
  gallery_mode <- reactive({ state$gallery_mode %||% "plain" })
  gallery_k    <- reactive({ state$gallery_k    %||% 10L })

  # ── Filtered + sorted slides ───────────────────────────────────────────────
  slides_filtered <- reactive({
    df <- filter_dataset(metadata(), state$selected_dataset)

    if (nzchar(input$gallery_search %||% ""))
      df <- df[grepl(input$gallery_search, df$slide_id, ignore.case = TRUE), ]

    if (nzchar(input$gallery_outcome_filter %||% ""))
      df <- df[df$outcome == input$gallery_outcome_filter, ]

    df[order(df$slide_id), ]
  })

  output$gallery_slide_count_badge <- renderUI({
    sprintf("%d slides", nrow(slides_filtered()))
  })

  # ── Patch helper: top-K or recurring for a set of slide IDs ───────────────
  get_patches <- function(slide_ids, mode, k) {
    if (is.null(all_patches) || length(slide_ids) == 0) return(NULL)
    df <- all_patches[all_patches$slide_id %in% slide_ids, ]
    if (nrow(df) == 0) return(NULL)
    if (mode == "topk") {
      df <- df[order(-df$attention_mean), ]
      df <- do.call(rbind, lapply(slide_ids, function(sid) {
        s <- df[df$slide_id == sid, ]
        s[seq_len(min(k, nrow(s))), ]
      }))
    } else {
      df <- df[df$n_seeds_top >= 3, ]
      df <- do.call(rbind, lapply(slide_ids, function(sid) {
        s <- df[df$slide_id == sid, ]
        if (nrow(s) == 0) return(s)
        s <- s[order(-s$recurrence_count, -s$attention_mean), ]
        s[seq_len(min(k, nrow(s))), ]
      }))
    }
    df
  }

  # ── Card grid (plain / heatmap modes) ─────────────────────────────────────
  render_card_grid <- function(df) {
    if (nrow(df) == 0)
      return(div(class = "text-muted p-4", "No slides match the current filters."))
    mode <- gallery_mode()
    cards <- lapply(seq_len(nrow(df)), function(i) {
      row     <- df[i, ]
      sid     <- row$slide_id
      img_src <- if (mode == "heatmap") heatmap_url(sid) else thumbnail_url(sid)
      # Hover preview shows heatmap in heatmap mode, thumbnail otherwise
      preview_src <- if (mode == "heatmap") heatmap_url(sid) else thumbnail_url(sid)
      div(
        class = "col-sm-6 col-md-4 col-lg-3 mb-3",
        div(
          class   = "slide-card",
          onclick = sprintf(
            "Shiny.setInputValue('gallery_selected_slide','%s',{priority:'event'});", sid
          ),
          tags$img(
            src                = img_src,
            alt                = sid,
            loading            = "lazy",
            `data-preview-src` = preview_src,
            onerror            = "this.src='https://placehold.co/300x140/e2e8f0/94a3b8?text=No+image';"
          ),
          div(
            class = "card-body",
            div(class = "fw-semibold text-truncate flex-grow-1", short_id(sid)),
            outcome_badge(row$outcome)
          )
        )
      )
    })
    div(class = "row", cards)
  }

  # ── Patch strip row for one slide ──────────────────────────────────────────
  patch_strip_row <- function(row, patches_df, mode, ext_x = NULL, ext_y = NULL) {
    sid     <- row$slide_id
    patches <- patches_df[patches_df$slide_id == sid, ]
    if (is.null(patches) || nrow(patches) == 0) return(NULL)

    ew <- if (!is.null(ext_x) && sid %in% names(ext_x)) as.integer(ext_x[[sid]]) else 40000L
    eh <- if (!is.null(ext_y) && sid %in% names(ext_y)) as.integer(ext_y[[sid]]) else 40000L

    thumbs <- lapply(seq_len(nrow(patches)), function(j) {
      p     <- patches[j, ]
      pid   <- p$patch_id
      score <- p$attention_mean
      cx    <- as.integer(p$x_coord) + 128L
      cy    <- as.integer(p$y_coord) + 128L
      if (mode == "recurring") {
        bg  <- recurrence_marker_color(p$recurrence_count)
        txt <- recurrence_marker_text(p$recurrence_count)
        lbl <- paste0("×", p$recurrence_count)
      } else {
        bg  <- attention_marker_color(score)
        txt <- attention_marker_text(score)
        lbl <- sprintf("%.2f", score)
      }
      div(
        class   = "gallery-patch-thumb",
        onclick = sprintf(
          "Shiny.setInputValue('gallery_patch_clicked','%s|%s|%d|%d|%d|%d',{priority:'event'});",
          sid, pid, cx, cy, ew, eh
        ),
        tags$img(
          src     = patch_url(sid, pid),
          alt     = pid,
          loading = "lazy",
          onerror = "this.src='https://placehold.co/72x72/f8f9fa/94a3b8?text=?';"
        ),
        tags$span(
          class = "patch-attn",
          style = sprintf("background:%s; color:%s;", bg, txt),
          lbl
        )
      )
    })

    div(
      class = "gallery-slide-row",
      div(
        class   = "gallery-slide-row-label",
        onclick = sprintf(
          "Shiny.setInputValue('gallery_selected_slide','%s',{priority:'event'});", sid
        ),
        div(class = "fw-semibold", short_id(sid)),
        outcome_badge(row$outcome)
      ),
      div(
        class = "gallery-strip-wrapper",
        tags$button(
          class   = "gallery-strip-arrow gallery-strip-arrow-left",
          onclick = "lmsGalleryStripScroll(this,-1); event.stopPropagation();"
        ),
        div(class = "gallery-slide-row-patches", thumbs),
        tags$button(
          class   = "gallery-strip-arrow gallery-strip-arrow-right",
          onclick = "lmsGalleryStripScroll(this,1); event.stopPropagation();"
        )
      )
    )
  }

  # ── Patch list view (recurring / topk modes) ───────────────────────────────
  render_patch_list <- function(df) {
    if (nrow(df) == 0)
      return(div(class = "text-muted p-4", "No slides match the current filters."))
    mode     <- gallery_mode()
    k        <- gallery_k()
    patches  <- get_patches(df$slide_id, mode, k)
    if (is.null(patches) || nrow(patches) == 0)
      return(div(class = "text-muted p-4", "No patch data available."))

    ex <- slide_ext
    outcome_filter <- input$gallery_outcome_filter %||% ""
    mode_label   <- if (mode == "topk") sprintf("Top %d Patches", k) else "Recurring Patches"
    outcome_label <- if (nzchar(outcome_filter)) outcome_filter else "All Outcomes"

    header <- div(
      class = "gallery-patch-mode-header",
      tags$span(class = "fw-semibold", mode_label),
      tags$span(class = "gallery-pmh-sep", "·"),
      tags$span(state$selected_dataset),
      tags$span(class = "gallery-pmh-sep", "·"),
      tags$span(outcome_label),
      tags$span(class = "gallery-pmh-sep", "·"),
      tags$span(class = "text-muted", sprintf("%d slides", nrow(df)))
    )

    make_section <- function(outcome_label_ui, outcome_val) {
      sub_df <- if (nzchar(outcome_val)) df[df$outcome == outcome_val, ] else df
      if (nrow(sub_df) == 0) return(NULL)
      rows <- Filter(Negate(is.null),
                     lapply(seq_len(nrow(sub_df)), function(i)
                       patch_strip_row(sub_df[i, ], patches, mode,
                                       ext_x = if (!is.null(ex)) ex$x else NULL,
                                       ext_y = if (!is.null(ex)) ex$y else NULL)))
      if (length(rows) == 0) return(NULL)
      tagList(
        div(class = "gallery-patch-section-header",
            sprintf("%s  (%d)", outcome_label_ui, nrow(sub_df))),
        rows
      )
    }

    if (nzchar(outcome_filter)) {
      rows <- Filter(Negate(is.null),
                     lapply(seq_len(nrow(df)), function(i)
                       patch_strip_row(df[i, ], patches, mode,
                                       ext_x = if (!is.null(ex)) ex$x else NULL,
                                       ext_y = if (!is.null(ex)) ex$y else NULL)))
      tagList(header, div(rows))
    } else {
      tagList(
        header,
        make_section("FAVORABLE", "Favorable"),
        make_section("ADVERSE",   "Adverse")
      )
    }
  }

  # ── Main grid output ───────────────────────────────────────────────────────
  output$gallery_grid <- renderUI({
    df   <- slides_filtered()
    mode <- gallery_mode()
    if (mode %in% c("plain", "heatmap")) {
      render_card_grid(df)
    } else {
      render_patch_list(df)
    }
  })

  # ── Slide card click → Inspect tab ────────────────────────────────────────
  observeEvent(input$gallery_selected_slide, {
    state$selected_slide_id <- input$gallery_selected_slide
    gmode <- state$gallery_mode %||% "plain"
    if (gmode %in% c("plain", "heatmap")) {
      state$inspect_mode <- gmode
    }
    shinyjs::runjs("lmsNavigate('inspect');")
  })

  # ── Patch thumb click → Inspect + zoom + scroll to patch ─────────────────
  observeEvent(input$gallery_patch_clicked, {
    parts <- strsplit(input$gallery_patch_clicked, "\\|")[[1]]
    if (length(parts) < 6) return()
    sid <- parts[1]
    pid <- parts[2]
    cx  <- as.integer(parts[3])
    cy  <- as.integer(parts[4])
    ew  <- as.integer(parts[5])
    eh  <- as.integer(parts[6])

    state$selected_slide_id <- sid
    state$inspect_mode      <- state$gallery_mode %||% "topk"
    state$highlighted_patch <- pid

    shinyjs::runjs(sprintf(paste0(
      "lmsNavigate('inspect');",
      "lmsZoomAfterLoad('wsi_canvas',%d,%d,%d,%d);",
      "setTimeout(function(){lmsScrollToPatch('%s');},600);"
    ), cx, cy, ew, eh, pid))
  })
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
