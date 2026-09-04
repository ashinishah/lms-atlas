library(shiny)
library(bslib)
library(shinyjs)

# All R/ files are auto-sourced by Shiny — no source() calls needed.

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = div(
    class = "d-flex align-items-center gap-3",
    span("LMS Atlas"),
    # Dataset switcher in header (disabled on Compare tab)
    uiOutput("dataset_switcher_ui")
  ),
  id = "main_nav",
  theme = bs_theme(
    version    = 5,
    base_font  = font_google("Inter"),
    code_font  = font_google("JetBrains Mono"),
    bg         = "#F8F9FA",
    fg         = "#1A1A2E",
    primary    = "#0D9488"   # TCGA teal default; overridden via CSS var at runtime
  ),

  useShinyjs(),

  nav_panel("Summary",        ui_summary()),
  nav_panel("Gallery",        ui_gallery()),
  nav_panel("Inspect",        ui_inspect()),
  nav_panel("By Outcome",     ui_byoutcome()),
  nav_panel("TCGA vs. SPORE", ui_compare())
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Shared reactive state ──────────────────────────────────────────────────
  state <- reactiveValues(
    selected_dataset  = "TCGA",
    selected_slide_id = NULL,
    inspect_mode      = "plain",  # "plain" | "heatmap" | "recurring" | "topk"
    topk_k            = 10L,
    highlighted_patch = NULL
  )

  # ── Metadata ───────────────────────────────────────────────────────────────
  metadata <- reactive({
    readr::read_csv("data/metadata.csv", show_col_types = FALSE)
  })

  # ── Accent color (switches with dataset) ───────────────────────────────────
  accent <- reactive({
    if (state$selected_dataset == "TCGA") "#0D9488" else "#D97706"
  })

  # Push accent CSS variable to browser whenever it changes
  observe({
    shinyjs::runjs(sprintf(
      "document.documentElement.style.setProperty('--accent', '%s');",
      accent()
    ))
  })

  # ── Dataset switcher UI ────────────────────────────────────────────────────
  output$dataset_switcher_ui <- renderUI({
    # Disable on Compare tab (shows both datasets simultaneously)
    disabled <- isTRUE(input$main_nav == "TCGA vs. SPORE")
    div(
      class = if (disabled) "opacity-50 pe-none" else "",
      radioButtons(
        "dataset_toggle",
        label    = NULL,
        choices  = c("TCGA", "SPORE"),
        selected = state$selected_dataset,
        inline   = TRUE
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
  server_compare(input, output, session, state, metadata, accent)
}

# ── Run ───────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
