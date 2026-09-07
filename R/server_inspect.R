# server_inspect.R — Inspect tab server logic

server_inspect <- function(input, output, session, state, metadata, accent) {

  # ── Load patch metadata once ───────────────────────────────────────────────
  pm_path <- patch_metadata_path()
  all_patches <- if (file.exists(pm_path)) {
    readr::read_csv(pm_path, show_col_types = FALSE)
  } else {
    NULL
  }

  # True level-0 extents from SVS file dimensions (slide_dimensions.csv).
  slide_ext <- slide_extents()

  # ── Ordered slide list for the current dataset (for prev/next + picker) ────
  slide_list <- reactive({
    df <- filter_dataset(metadata(), state$selected_dataset)
    df[order(df$slide_id), ]
  })

  # ── Current slide row ──────────────────────────────────────────────────────
  current_slide <- reactive({
    req(state$selected_slide_id)
    meta <- metadata()
    meta[meta$slide_id == state$selected_slide_id, ]
  })

  # All patches for current slide, sorted by attention (for extent + topk base)
  slide_patches <- reactive({
    req(state$selected_slide_id)
    if (is.null(all_patches)) return(NULL)
    df <- all_patches[all_patches$slide_id == state$selected_slide_id, ]
    df[order(-df$attention_mean), ]
  })

  # Filtered + ranked patches for the current mode (shared by cards and markers)
  mode_patches <- reactive({
    df   <- slide_patches()
    if (is.null(df) || nrow(df) == 0) return(df)
    mode <- state$inspect_mode

    if (mode == "topk") {
      k <- state$topk_k
      df[seq_len(min(k, nrow(df))), ]
    } else if (mode == "recurring") {
      rec <- df[df$n_seeds_top >= 3, ]
      rec <- rec[order(-rec$recurrence_count, -rec$attention_mean), ]
      rec[seq_len(min(state$recurring_k, nrow(rec))), ]
    } else {
      df
    }
  })

  # ── Nav: label (X / N) ─────────────────────────────────────────────────────
  output$inspect_nav_label <- renderUI({
    df  <- slide_list()
    sid <- state$selected_slide_id
    if (is.null(sid)) return(span("— / —"))
    idx <- which(df$slide_id == sid)
    if (length(idx) == 0) return(span("— / —"))
    span(sprintf("%d / %d", idx, nrow(df)))
  })

  # ── Nav: slide picker dropdown ────────────────────────────────────────────
  output$inspect_slide_picker <- renderUI({
    df <- slide_list()
    choices <- setNames(
      df$slide_id,
      paste0(short_id(df$slide_id), " · ", df$outcome)
    )
    selectInput(
      "inspect_slide_select", NULL,
      choices  = choices,
      selected = state$selected_slide_id,
      width    = "100%"
    )
  })

  # ── Nav: prev / next ───────────────────────────────────────────────────────
  observeEvent(input$inspect_prev_click, {
    df  <- slide_list()
    idx <- which(df$slide_id == state$selected_slide_id)
    if (length(idx) > 0 && idx > 1L) {
      state$selected_slide_id <- df$slide_id[idx - 1L]
      state$highlighted_patch <- NULL
    }
  })

  observeEvent(input$inspect_next_click, {
    df  <- slide_list()
    idx <- which(df$slide_id == state$selected_slide_id)
    if (length(idx) > 0 && idx < nrow(df)) {
      state$selected_slide_id <- df$slide_id[idx + 1L]
      state$highlighted_patch <- NULL
    }
  })

  # Break circular update: only apply if the dropdown chose a *different* slide
  observeEvent(input$inspect_slide_select, {
    req(input$inspect_slide_select, nchar(input$inspect_slide_select) > 0)
    if (!identical(input$inspect_slide_select, state$selected_slide_id)) {
      state$selected_slide_id <- input$inspect_slide_select
      state$highlighted_patch <- NULL
    }
  }, ignoreInit = TRUE)

  # Reset zoom + pan when slide changes
  observeEvent(state$selected_slide_id, {
    shinyjs::runjs("lmsZoomReset('wsi_canvas');")
  })

  # Scroll right strip when highlighted patch changes
  observeEvent(state$highlighted_patch, {
    pid <- state$highlighted_patch
    req(!is.null(pid) && nchar(pid) > 0)
    shinyjs::runjs(sprintf("lmsScrollToPatch('%s');", pid))
  })

  # ── Slide info panel ───────────────────────────────────────────────────────
  output$inspect_sidebar_content <- renderUI({
    if (is.null(state$selected_slide_id)) {
      return(div(class = "text-muted mt-2 fs-sm",
                 "Select a slide from the Gallery to inspect it."))
    }
    row <- current_slide()
    if (nrow(row) == 0) return(div(class = "text-muted", "Slide not found."))

    tagList(
      div(class = "slide-label mt-2", "Slide ID"),
      div(class = "slide-value text-truncate fs-xs", row$slide_id),

      div(class = "slide-label", "Case ID"),
      div(class = "slide-value", short_id(row$slide_id)),

      div(class = "slide-label", "Dataset"),
      div(class = "slide-value", dataset_badge(row$dataset)),

      div(class = "slide-label", "Outcome"),
      div(class = "slide-value", outcome_badge(row$outcome)),

      hr(class = "my-2"),

      div(class = "slide-label", "Age at diagnosis"),
      div(class = "slide-value", paste(row$age, "yrs")),

      div(class = "slide-label", "Primary site"),
      div(class = "slide-value fs-xs", row$site),

      hr(class = "my-2"),

      div(class = "slide-label", "Total patches"),
      div(class = "slide-value",
          if (is.na(row$n_patches)) "—" else format(row$n_patches, big.mark = ","))
    )
  })

  # ── WSI image ──────────────────────────────────────────────────────────────
  output$wsi_image_ui <- renderUI({
    req(state$selected_slide_id)
    sid     <- state$selected_slide_id
    img_src <- if (state$inspect_mode == "heatmap") heatmap_url(sid) else thumbnail_url(sid)
    tags$img(
      class   = "wsi-image",
      src     = img_src,
      alt     = sid,
      onerror = "this.src='https://placehold.co/800x600/0f172a/94a3b8?text=No+image';"
    )
  })

  # ── Minimap ────────────────────────────────────────────────────────────────
  output$minimap_ui <- renderUI({
    req(state$selected_slide_id)
    tags$img(
      src     = thumbnail_url(state$selected_slide_id),
      alt     = "minimap",
      onerror = "this.style.display='none';"
    )
  })

  # ── WSI marker SVG (topk / recurring modes) ────────────────────────────────
  output$wsi_marker_svg <- renderUI({
    req(state$selected_slide_id)
    mode <- state$inspect_mode
    if (!mode %in% c("recurring", "topk")) return(NULL)

    df_mode <- mode_patches()
    if (is.null(df_mode) || nrow(df_mode) == 0) return(NULL)

    # True level-0 extents from SVS file dimensions (slide_dimensions.csv).
    sid <- state$selected_slide_id
    slide_ext_x <- if (!is.null(slide_ext) && sid %in% names(slide_ext$x))
      as.integer(slide_ext$x[[sid]])
    else
      max(df_mode$x_coord + 256L)
    slide_ext_y <- if (!is.null(slide_ext) && sid %in% names(slide_ext$y))
      as.integer(slide_ext$y[[sid]])
    else
      max(df_mode$y_coord + 256L)

    r_base <- max(200L, round(min(slide_ext_x, slide_ext_y) / 60))

    markers <- lapply(seq_len(nrow(df_mode)), function(i) {
      row  <- df_mode[i, ]
      cx   <- row$x_coord + 128L
      cy   <- row$y_coord + 128L
      pid  <- row$patch_id
      attn <- row$attention_mean
      bg   <- if (mode == "recurring")
                recurrence_marker_color(row$recurrence_count)
              else
                attention_marker_color(attn)
      txt  <- if (mode == "recurring")
                recurrence_marker_text(row$recurrence_count)
              else
                attention_marker_text(attn)
      highlighted <- identical(state$highlighted_patch, pid)
      r    <- if (highlighted) round(r_base * 1.5) else r_base
      fsz  <- round(r * 1.05)

      tagList(
        tags$circle(
          cx             = cx,
          cy             = cy,
          r              = r,
          fill           = bg,
          stroke         = if (highlighted) "#ffffff" else "rgba(0,0,0,0.35)",
          `stroke-width` = round(r / 6),
          style          = "cursor: pointer;",
          onclick        = sprintf(
            "Shiny.setInputValue('inspect_patch_clicked','%s',{priority:'event'});lmsZoomToSvgPoint('wsi_canvas',%d,%d,%d,%d);",
            pid, cx, cy, slide_ext_x, slide_ext_y
          )
        ),
        tags$text(
          x                  = cx,
          y                  = cy,
          `text-anchor`      = "middle",
          `dominant-baseline`= "central",
          fill               = txt,
          style              = sprintf(
            "font-size: %dpx; font-weight: 700; pointer-events: none;", fsz
          ),
          as.character(i)
        )
      )
    })

    tags$svg(
      class               = "wsi-marker-svg",
      viewBox             = paste0("0 0 ", slide_ext_x, " ", slide_ext_y),
      preserveAspectRatio = "xMidYMid meet",
      tagList(markers)
    )
  })

  # ── Colorbar sidebar (heatmap mode only, collapsible) ─────────────────────
  output$inspect_colorbar_sidebar <- renderUI({
    if (state$inspect_mode != "heatmap") return(NULL)
    div(
      class = "colorbar-sidebar",
      id    = "colorbar_sidebar",
      tags$button(
        class   = "colorbar-toggle",
        title   = "Toggle colorbar",
        onclick = paste0(
          "var s = document.getElementById('colorbar_sidebar');",
          "s.classList.toggle('collapsed');",
          "this.textContent = s.classList.contains('collapsed') ? '▶' : '◀';"
        ),
        "◀"
      ),
      div(
        class = "colorbar-body",
        div("High", class = "colorbar-scale-label"),
        div(class = "colorbar-bar-gradient"),
        div("Low", class = "colorbar-scale-label")
      )
    )
  })

  # ── Right patch strip ──────────────────────────────────────────────────────
  output$inspect_right_strip <- renderUI({
    mode <- state$inspect_mode
    if (!mode %in% c("recurring", "topk")) return(NULL)

    label <- if (mode == "topk") {
      sprintf("Top %d patches", state$topk_k)
    } else {
      sprintf("Top %d recurring", state$recurring_k)
    }

    div(
      class = "inspect-right-strip",
      div(
        class = "strip-header",
        div(label, class = "fw-semibold fs-xs"),
        div("ranked by attention", class = "text-muted fs-xs mt-1")
      ),
      uiOutput("right_patch_cards")
    )
  })

  # ── Patch cards in right strip ─────────────────────────────────────────────
  output$right_patch_cards <- renderUI({
    req(state$selected_slide_id)
    req(state$inspect_mode %in% c("recurring", "topk"))

    sid  <- state$selected_slide_id
    mode <- state$inspect_mode
    df   <- mode_patches()

    if (is.null(df) || nrow(df) == 0) {
      return(div(class = "text-muted p-3 fs-xs", "No patch data available."))
    }

    # True level-0 extents from SVS file dimensions (slide_dimensions.csv).
    sid <- state$selected_slide_id
    slide_ext_x <- if (!is.null(slide_ext) && sid %in% names(slide_ext$x))
      as.integer(slide_ext$x[[sid]])
    else
      max(df$x_coord + 256L)
    slide_ext_y <- if (!is.null(slide_ext) && sid %in% names(slide_ext$y))
      as.integer(slide_ext$y[[sid]])
    else
      max(df$y_coord + 256L)

    lapply(seq_len(nrow(df)), function(i) {
      row        <- df[i, ]
      score      <- row$attention_mean
      patch_id   <- row$patch_id
      cx         <- row$x_coord + 128L
      cy         <- row$y_coord + 128L
      bg_color   <- attention_marker_color(score)
      txt_color  <- attention_marker_text(score)
      highlighted <- identical(state$highlighted_patch, patch_id)

      div(
        class          = paste0("patch-card-v", if (highlighted) " highlighted" else ""),
        `data-patch-id`= patch_id,
        onclick        = sprintf(
          "Shiny.setInputValue('inspect_patch_clicked','%s',{priority:'event'});lmsZoomToSvgPoint('wsi_canvas',%d,%d,%d,%d);",
          patch_id, cx, cy, slide_ext_x, slide_ext_y
        ),
        div(
          class = "patch-img-wrap",
          tags$img(
            src     = patch_url(sid, patch_id),
            alt     = patch_id,
            onerror = "this.src='https://placehold.co/134x134/f8f9fa/94a3b8?text=patch';"
          ),
          tags$span(class = "patch-rank", as.character(i)),
          if (mode == "recurring") {
            tags$span(
              class = "patch-recurrence-badge",
              style = sprintf("background:%s; color:%s;",
                              recurrence_marker_color(row$recurrence_count),
                              recurrence_marker_text(row$recurrence_count)),
              paste0("×", row$recurrence_count)
            )
          } else {
            tags$span(
              class = "patch-attn",
              style = sprintf("background:%s; color:%s;", bg_color, txt_color),
              sprintf("%.2f", score)
            )
          }
        )
      )
    })
  })

  # ── Patch click → highlight + scroll ──────────────────────────────────────
  observeEvent(input$inspect_patch_clicked, {
    state$highlighted_patch <- input$inspect_patch_clicked
  })

  # ── Add to comparison ──────────────────────────────────────────────────────
  observeEvent(input$inspect_add_compare, {
    req(state$selected_slide_id)
    if (is.null(state$compare_slide_a)) {
      state$compare_slide_a <- state$selected_slide_id
    } else {
      state$compare_slide_b <- state$selected_slide_id
    }
    shinyjs::runjs("lmsNavigate('compare_slides');")
  })
}
