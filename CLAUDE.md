# LMS Atlas — Shiny App

## What this is
R Shiny visualization app for pathology whole-slide images (WSI), model-generated
attention heatmaps, and clinical metadata. Reads pre-generated Python outputs only —
no live model inference, no `reticulate`.

## Stack
- UI framework: `bslib` (Bootstrap 5 theming)
- Data table: `DT`
- Interactive plots: `plotly`
- JS helpers: `shinyjs`
- File I/O: `readr`
- No Python dependencies — do NOT add `reticulate`

## Deployment target
Posit Connect Cloud. Always use `renv`. Flag any package that requires system
libraries before installing.

## File layout
```
app.R               # thin: calls ui() and server() only
R/
  utils.R           # image URL helpers, badge renderers — always use these, never hardcode paths
  ui_summary.R      # Summary tab UI
  ui_gallery.R      # Gallery tab UI
  ui_inspect.R      # Inspect tab UI
  ui_byoutcome.R    # By Outcome tab UI
  ui_compare.R      # TCGA vs SPORE tab UI
  server_summary.R  # Summary server logic
  server_gallery.R  # Gallery server logic
  server_inspect.R  # Inspect server logic (most complex)
  server_byoutcome.R
  server_compare.R
data/
  metadata.csv      # one row per slide — the ONLY data file in the repo
www/
  custom.css        # accent color CSS variable + shared styles
```
Shiny auto-sources everything in R/ — no manual source() calls needed.

## Image paths
Images are NOT in the repo. Always use the helpers in R/utils.R:
- `thumbnail_url(slide_id)` → WSI thumbnail
- `heatmap_url(slide_id)`   → attention overlay
- `patch_url(slide_id, patch_id)` → individual patch tile

Base path comes from env var `LMS_IMAGE_BASE`:
- Local dev: set in `.Renviron` (e.g. `LMS_IMAGE_BASE=C:/data/lms-images`)
- Connect Cloud: set in the app's environment variables config

## Datasets
- TCGA-SARC: ~90 slides (~12 favorable, ~78 adverse)
- SPORE: ~24 slides (~5 favorable, ~19 adverse)
- Outcome labels: "Favorable" / "Adverse" (never "high/low grade" or "good/bad")

## Color system
| Token           | Hex       | Usage                                      |
|-----------------|-----------|--------------------------------------------|
| TCGA accent     | `#0D9488` | Active states, buttons when TCGA selected  |
| SPORE accent    | `#D97706` | Active states, buttons when SPORE selected |
| Favorable badge | green     | Outcome badge                              |
| Adverse badge   | red       | Outcome badge                              |

Accent switching: `--accent` CSS variable in www/custom.css, swapped via
`shinyjs::runjs()` whenever `selected_dataset` changes.

Heatmap colormap: viridis (purple #440154 → green #5DC963 → yellow #FDE725, 0–1 scale)

## Key reactive state (all in server() in app.R)
```r
state <- reactiveValues(
  selected_dataset  = "TCGA",   # "TCGA" | "SPORE"
  selected_slide_id = NULL,      # character slide ID
  inspect_mode      = "plain",   # "plain" | "heatmap" | "recurring" | "topk"
  topk_k            = 10L,       # 5 | 10 | 20 | 30
  highlighted_patch = NULL       # patch_id for bidirectional strip ↔ marker linking
)
```
Pass `state` and `accent` reactive down to all module server functions.

## Five main views
1. Summary — dataset stats, outcome distribution, model performance
2. Gallery — sortable/filterable card grid; click → sets selected_slide_id → navigates to Inspect
3. Inspect — single-slide deep-dive; four sub-modes toggled by mode bar
4. By Outcome — two-column favorable vs. adverse comparison
5. TCGA vs. SPORE — side-by-side cross-dataset (dataset toggle grayed out here)

## Inspect view modes
- Plain WSI: slide thumbnail only, no overlay, no patch strip
- Dense Heatmap: viridis overlay; hint bar at bottom instead of patch strip
- Recurring Patches: patches in top-K set for ≥3 of 5 training seeds; patch strip shown
- Top K Patches: top-K by attention score; K ∈ {5,10,20,30} default 10; patch strip shown

Patch strip: 148px tall, horizontally scrollable, plain H&E thumbnails (no overlay).
Bidirectional: click marker on WSI ↔ scroll + highlight patch in strip.

## Design reference
Wireframe canvas (7 artboards): https://claude.ai/code/artifact/1d5139af-cf84-419f-b4bb-e2f082d4325c
Artboards: Summary · Gallery · Inspect (Dense HM) · Inspect (Top K) · By Outcome · TCGA vs SPORE · Theme Options
