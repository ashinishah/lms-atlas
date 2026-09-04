library(shiny)
library(bslib)
library(shinyjs)

# All R/ files are auto-sourced by Shiny — no source() calls needed.

# Expose image directories to the browser
addResourcePath("lms-images",   image_base())
addResourcePath("lms-patches",  patches_base())

# ── Sidebar nav button ─────────────────────────────────────────────────────────
nav_btn <- function(id, label, fa) {
  tags$button(
    id      = paste0("nav_", id),
    class   = "lms-nav-item",
    onclick = sprintf("lmsNavigate('%s');", id),
    icon(fa, class = "lms-nav-icon"),
    label
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(src = "app.js")
  ),
  bslib::page_fillable(
    padding = 0,
    gap     = 0,
    theme   = bs_theme(
      version   = 5,
      base_font = font_google("Inter"),
      code_font = font_google("JetBrains Mono"),
      bg        = "#F8F9FA",
      fg        = "#1A1A2E",
      primary   = "#0D9488"
    ),
    div(
      class = "lms-layout",

      # ── Global header (full width) ────────────────────────────────────────
      div(
        class = "lms-global-header",
        div(
          class = "lms-header-brand",
          div(class = "lms-brand-name", "LMS Atlas"),
          div(class = "lms-brand-sub",  "Leiomyosarcoma · Attention Viz")
        ),
        div(
          class = "lms-header-content",
          div(
            div(class = "lms-header-title", "LMS Sarcoma · Whole-Slide Attention Visualization"),
            div(class = "lms-header-sub",   "TCGA-SARC & SPORE · ABMIL model · 5 training seeds")
          )
        )
      ),

      # ── Body row (sidebar + main) ─────────────────────────────────────────
      div(
        class = "lms-body",

        # ── Left sidebar ─────────────────────────────────────────────────────
        tags$aside(
          class = "lms-sidebar",
          tags$nav(
            class = "lms-nav",
            div(class = "lms-nav-section-label", "EXPLORE"),
            nav_btn("summary",        "Summary",          "chart-bar"),
            nav_btn("gallery",        "Gallery",           "th-large"),
            nav_btn("inspect",        "Inspect",           "search"),
            div(class = "lms-nav-section-label", "COMPARE"),
            nav_btn("byoutcome",      "By Outcome",        "columns"),
            nav_btn("compare_slides", "Compare Slides",    "exchange-alt"),
            div(class = "lms-nav-section-label", "CROSS-DATASET"),
            nav_btn("compare",        "TCGA vs. SPORE",    "layer-group")
          )
        ),

        # ── Main area ────────────────────────────────────────────────────────
        div(
          class = "lms-main",
          div(
            class = "lms-topbar",
            uiOutput("view_mode_bar_ui"),
            div(class = "ms-auto", uiOutput("dataset_switcher_ui"))
          ),
          div(
            class = "lms-pages",
            div(id = "page_summary",        class = "lms-page",        ui_summary()),
            div(id = "page_gallery",        class = "lms-page d-none", ui_gallery()),
            div(id = "page_inspect",        class = "lms-page d-none", ui_inspect()),
            div(id = "page_byoutcome",      class = "lms-page d-none", ui_byoutcome()),
            div(id = "page_compare_slides", class = "lms-page d-none", ui_compare_slides()),
            div(id = "page_compare",        class = "lms-page d-none", ui_compare())
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Shared reactive state ──────────────────────────────────────────────────
  state <- reactiveValues(
    selected_dataset  = "TCGA",
    selected_slide_id = NULL,
    inspect_mode      = "plain",
    topk_k            = 10L,
    highlighted_patch = NULL,
    compare_slide_a   = NULL,
    compare_slide_b   = NULL
  )

  # ── Metadata ───────────────────────────────────────────────────────────────
  metadata <- reactive({
    readr::read_csv("data/metadata.csv", show_col_types = FALSE)
  })

  # ── Accent color ───────────────────────────────────────────────────────────
  accent <- reactive({
    if (state$selected_dataset == "TCGA") "#0D9488" else "#D97706"
  })

  observe({
    shinyjs::runjs(sprintf(
      "document.documentElement.style.setProperty('--accent','%s');", accent()
    ))
  })

  # ── Global view mode bar ───────────────────────────────────────────────────
  # Hidden on summary and gallery; shown on all other pages.
  output$view_mode_bar_ui <- renderUI({
    tab <- input$active_tab %||% "summary"
    if (tab %in% c("summary", "gallery")) return(NULL)

    mode  <- state$inspect_mode
    mk_btn <- function(id, label, m) {
      cls <- if (m == mode) "btn btn-sm mode-btn mode-btn-active" else "btn btn-sm mode-btn"
      tags$button(
        class   = cls,
        onclick = sprintf("Shiny.setInputValue('global_mode','%s',{priority:'event'});", m),
        label
      )
    }
    tagList(
      div(
        class = "view-mode-bar",
        mk_btn("plain",     "Plain WSI",         "plain"),
        mk_btn("heatmap",   "Dense Heatmap",      "heatmap"),
        mk_btn("recurring", "Recurring Patches",  "recurring"),
        mk_btn("topk",      "Top K Patches",      "topk")
      ),
      if (mode == "topk") {
        div(
          class = "d-flex align-items-center gap-1 ms-2",
          span("K =", class = "text-muted fs-xs"),
          div(
            class = "btn-group btn-group-sm",
            lapply(c(5L, 10L, 20L, 30L), function(k) {
              tags$button(
                class   = if (identical(state$topk_k, k)) "btn btn-accent btn-sm" else "btn btn-outline-secondary btn-sm",
                onclick = sprintf("Shiny.setInputValue('global_topk_k',%d,{priority:'event'});", k),
                as.character(k)
              )
            })
          )
        )
      }
    )
  })

  observeEvent(input$global_mode, {
    state$inspect_mode <- input$global_mode
  })

  observeEvent(input$global_topk_k, {
    state$topk_k <- as.integer(input$global_topk_k)
  })

  # ── Dataset switcher ───────────────────────────────────────────────────────
  output$dataset_switcher_ui <- renderUI({
    disabled <- isTRUE(input$active_tab == "compare")
    ds <- state$selected_dataset
    div(
      class = if (disabled) "opacity-50 pe-none" else "",
      div(
        class = "btn-group btn-group-sm",
        role  = "group",
        tags$button(
          type    = "button",
          class   = paste0("btn dataset-btn", if (ds == "TCGA")  " active" else ""),
          onclick = "Shiny.setInputValue('dataset_toggle','TCGA',{priority:'event'});",
          "TCGA-SARC"
        ),
        tags$button(
          type    = "button",
          class   = paste0("btn dataset-btn", if (ds == "SPORE") " active" else ""),
          onclick = "Shiny.setInputValue('dataset_toggle','SPORE',{priority:'event'});",
          "SPORE"
        )
      )
    )
  })

  observeEvent(input$dataset_toggle, {
    state$selected_dataset <- input$dataset_toggle
  })

  # ── Module servers ─────────────────────────────────────────────────────────
  server_summary(input, output, session, state, metadata, accent)
  server_gallery(input, output, session, state, metadata, accent)
  server_inspect(input, output, session, state, metadata, accent)
  server_byoutcome(input, output, session, state, metadata, accent)
  server_compare_slides(input, output, session, state, metadata, accent)
  server_compare(input, output, session, state, metadata, accent)
}

shinyApp(ui, server)
