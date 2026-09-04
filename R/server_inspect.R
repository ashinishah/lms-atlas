# server_inspect.R — Inspect tab server logic

server_inspect <- function(input, output, session, state, metadata, accent) {

  # ── Load patch metadata once ───────────────────────────────────────────────
  pm_path <- file.path(globus_base(), "patch_metadata.csv")
  all_patches <- if (file.exists(pm_path)) {
    readr::read_csv(pm_path, show_col_types = FALSE)
  } else {
    NULL
  }

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
      rec[seq_len(min(30L, nrow(rec))), ]
    } else {
      df
    }
  })

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

    # Use all patches for extent so scaling matches full slide
    df_all <- slide_patches()
    slide_ext_x <- if (!is.null(df_all) && nrow(df_all) > 0)
      max(df_all$x_coord + 256L) else max(df_mode$x_coord + 256L)
    slide_ext_y <- if (!is.null(df_all) && nrow(df_all) > 0)
      max(df_all$y_coord + 256L) else max(df_mode$y_coord + 256L)

    r_base <- max(200L, round(min(slide_ext_x, slide_ext_y) / 60))

    markers <- lapply(seq_len(nrow(df_mode)), function(i) {
      row  <- df_mode[i, ]
      cx   <- row$x_coord + 128L
      cy   <- row$y_coord + 128L
      pid  <- row$patch_id
      attn <- row$attention_mean
      bg   <- attention_marker_color(attn)
      txt  <- attention_marker_text(attn)
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
            "Shiny.setInputValue('inspect_patch_clicked','%s',{priority:'event'});", pid
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
        tags$img(
          src   = "lms-images/heatmaps/attention_colorbar.png",
          class = "colorbar-bar-img",
          alt   = "Attention scale"
        ),
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
      "Recurring patches"
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

  # ── Patch cards with real data ─────────────────────────────────────────────
  output$right_patch_cards <- renderUI({
    req(state$selected_slide_id)
    req(state$inspect_mode %in% c("recurring", "topk"))

    sid  <- state$selected_slide_id
    mode <- state$inspect_mode
    df   <- mode_patches()

    if (is.null(df) || nrow(df) == 0) {
      return(div(class = "text-muted p-3 fs-xs", "No patch data available."))
    }

    lapply(seq_len(nrow(df)), function(i) {
      row        <- df[i, ]
      score      <- row$attention_mean
      patch_id   <- row$patch_id
      bg_color   <- attention_marker_color(score)
      txt_color  <- attention_marker_text(score)
      highlighted <- identical(state$highlighted_patch, patch_id)

      div(
        class          = paste0("patch-card-v", if (highlighted) " highlighted" else ""),
        `data-patch-id`= patch_id,
        onclick        = sprintf(
          "Shiny.setInputValue('inspect_patch_clicked','%s',{priority:'event'});", patch_id
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
