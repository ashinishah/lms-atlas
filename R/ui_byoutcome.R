# ui_byoutcome.R — By Outcome tab UI
# Two scrollable columns: Favorable (left) | Adverse (right)

ui_byoutcome <- function() {
  div(
    class = "byoutcome-layout",

    # ── Favorable column ──────────────────────────────────────────────────────
    div(
      class = "byoutcome-col",
      div(
        class = "byoutcome-col-header",
        style = "color: #166534;",
        span("Favorable prognosis"),
        span(class = "text-muted fw-normal fs-xs ms-2", uiOutput("byoutcome_favorable_count"))
      ),
      div(class = "byoutcome-col-grid", uiOutput("byoutcome_favorable_grid"))
    ),

    div(class = "byoutcome-divider"),

    # ── Adverse column ────────────────────────────────────────────────────────
    div(
      class = "byoutcome-col",
      div(
        class = "byoutcome-col-header",
        style = "color: #991B1B;",
        span("Adverse prognosis"),
        span(class = "text-muted fw-normal fs-xs ms-2", uiOutput("byoutcome_adverse_count"))
      ),
      div(class = "byoutcome-col-grid", uiOutput("byoutcome_adverse_grid"))
    )
  )
}
