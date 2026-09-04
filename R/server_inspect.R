# server_inspect.R — Inspect tab server logic
# Most complex server module. Handles mode switching, image display, and patch strip.

server_inspect <- function(input, output, session, state, metadata, accent) {

  # ── Current slide row ──────────────────────────────────────────────────────
  current_slide <- reactive({
    req(state$selected_slide_id)
    meta <- metadata()
    meta[meta$slide_id == state$selected_slide_id, ]
  })

  # ── Mode switching ─────────────────────────────────────────────────────────
  observeEvent(input$mode_plain,     { state$inspect_mode <- "plain"     })
  observeEvent(input$mode_heatmap,   { state$inspect_mode <- "heatmap"   })
  observeEvent(input$mode_recurring, { state$inspect_mode <- "recurring" })
  observeEvent(input$mode_topk,      { state$inspect_mode <- "topk"      })

  # Highlight the active mode button
  observe({
    mode <- state$inspect_mode
    modes <- c("plain", "heatmap", "recurring", "topk")
    ids   <- c("mode_plain", "mode_heatmap", "mode_recurring", "mode_topk")
    for (i in seq_along(modes)) {
      cls <- if (modes[i] == mode) {
        "btn btn-outline-secondary active"
      } else {
        "btn btn-outline-secondary"
      }
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').className = '%s';", ids[i], cls
      ))
    }
  })

  # ── K selector (Top K mode only) ───────────────────────────────────────────
  output$topk_selector_ui <- renderUI({
    req(state$inspect_mode == "topk")
    div(
      class = "ms-3 d-flex align-items-center gap-2",
      span("K =", class = "text-muted fs-xs"),
      div(
        class = "btn-group btn-group-sm",
        lapply(c(5L, 10L, 20L, 30L), function(k) {
          is_active <- identical(state$topk_k, k)
          actionButton(
            paste0("topk_k_", k),
            label = as.character(k),
            class = if (is_active) "btn btn-accent" else "btn btn-outline-secondary"
          )
        })
      )
    )
  })

  lapply(c(5L, 10L, 20L, 30L), function(k) {
    observeEvent(input[[paste0("topk_k_", k)]], {
      state$topk_k <- k
    }, ignoreInit = TRUE)
  })

  # ── Left sidebar ───────────────────────────────────────────────────────────
  output$inspect_sidebar_content <- renderUI({
    if (is.null(state$selected_slide_id)) {
      return(div(
        class = "text-muted",
        p("Select a slide from the Gallery to inspect it.")
      ))
    }

    row <- current_slide()
    if (nrow(row) == 0) return(div(class = "text-muted", "Slide not found."))

    mode_label <- switch(state$inspect_mode,
      plain     = "Plain WSI",
      heatmap   = "Dense Heatmap",
      recurring = "Recurring Patches",
      topk      = sprintf("Top K patches: K = %d", state$topk_k)
    )

    tagList(
      div(class = "slide-label", "Slide ID"),
      div(class = "slide-value text-truncate", row$slide_id),

      div(class = "slide-label", "Dataset"),
      div(class = "slide-value", dataset_badge(row$dataset)),

      div(class = "slide-label", "Outcome"),
      div(class = "slide-value", outcome_badge(row$outcome)),

      hr(class = "my-2"),

      div(class = "slide-label", "Age"),
      div(class = "slide-value", row$age),

      div(class = "slide-label", "Grade"),
      div(class = "slide-value", row$grade),

      div(class = "slide-label", "Site"),
      div(class = "slide-value", row$site),

      hr(class = "my-2"),

      div(class = "slide-label", "Max attention"),
      div(class = "slide-value", sprintf("%.3f", row$max_attention)),

      div(class = "slide-label", "Mean attention"),
      div(class = "slide-value", sprintf("%.3f", row$mean_attention)),

      div(class = "slide-label", "Patches"),
      div(class = "slide-value", row$n_patches),

      hr(class = "my-2"),

      div(class = "slide-label", "Mode"),
      div(class = "mode-info", mode_label)
    )
  })

  # ── WSI image ──────────────────────────────────────────────────────────────
  output$wsi_image_ui <- renderUI({
    req(state$selected_slide_id)
    sid <- state$selected_slide_id

    img_src <- if (state$inspect_mode == "heatmap") {
      heatmap_url(sid)
    } else {
      thumbnail_url(sid)
    }

    tags$img(
      class  = "wsi-image",
      src    = img_src,
      alt    = sid,
      onerror = "this.src='https://placehold.co/800x600/0f172a/94a3b8?text=No+image';"
    )
  })

  # ── Minimap ────────────────────────────────────────────────────────────────
  output$minimap_ui <- renderUI({
    req(state$selected_slide_id)
    tags$img(
      src   = thumbnail_url(state$selected_slide_id),
      alt   = "minimap",
      onerror = "this.style.display='none';"
    )
    # TODO: add viewport-rect overlay once pan/zoom is implemented
  })

  # ── Bottom strip ───────────────────────────────────────────────────────────
  output$inspect_bottom_strip <- renderUI({
    mode <- state$inspect_mode

    if (mode == "heatmap") {
      # Hint bar with colorbar
      div(
        class = "heatmap-hint",
        div(
          class = "colorbar-wrap",
          span("Low"),
          div(class = "colorbar"),
          span("High"),
          span("· Viridis attention scale (0–1)", class = "ms-2")
        )
      )
    } else if (mode %in% c("recurring", "topk")) {
      # Patch strip — populated by server
      div(
        class = "patch-strip-wrapper",
        uiOutput("patch_strip_cards")
      )
    } else {
      # Plain mode — no strip
      NULL
    }
  })

  # ── Patch strip cards ──────────────────────────────────────────────────────
  output$patch_strip_cards <- renderUI({
    req(state$selected_slide_id)
    req(state$inspect_mode %in% c("recurring", "topk"))

    sid <- state$selected_slide_id

    # TODO: load real patch metadata from file
    # For now, generate placeholder patch cards
    k <- if (state$inspect_mode == "topk") state$topk_k else 8L
    patch_ids <- paste0("patch_", seq_len(k))
    fake_scores <- sort(runif(k, 0.5, 0.95), decreasing = TRUE)

    lapply(seq_len(k), function(i) {
      pid   <- patch_ids[i]
      score <- fake_scores[i]
      color <- attention_marker_color(score)
      highlighted <- identical(state$highlighted_patch, pid)

      div(
        class   = paste0("patch-card", if (highlighted) " highlighted" else ""),
        onclick = sprintf(
          "Shiny.setInputValue('inspect_patch_clicked', '%s', {priority: 'event'});", pid
        ),
        tags$img(
          src     = patch_url(sid, pid),
          alt     = pid,
          onerror = "this.src='https://placehold.co/108x80/f8f9fa/94a3b8?text=patch';"
        ),
        div(
          class = "patch-meta d-flex align-items-center gap-1",
          tags$span(
            class = "patch-badge",
            style = sprintf("background: %s;", color),
            as.character(i)
          ),
          sprintf("attn %.3f", score)
        )
      )
    })
  })

  # ── Bidirectional patch linking ────────────────────────────────────────────
  observeEvent(input$inspect_patch_clicked, {
    state$highlighted_patch <- input$inspect_patch_clicked
    # TODO: pan WSI to patch coordinates when pan/zoom is implemented
  })
}
