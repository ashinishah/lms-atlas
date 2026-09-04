# ui_byoutcome.R — By Outcome tab UI
# Two-column layout: Favorable (left) vs Adverse (right)

ui_byoutcome <- function() {
  nav_panel_body(
    class = "p-4",
    h4("By Outcome", class = "mb-3"),
    p("Slides grouped by prognosis for the selected dataset.", class = "text-muted mb-3"),

    fluidRow(
      col_6(
        div(
          class = "d-flex align-items-center gap-2 mb-2",
          span(class = "badge bg-success", "Favorable"),
          span(uiOutput("byoutcome_favorable_count"), class = "text-muted fs-xs")
        ),
        uiOutput("byoutcome_favorable_grid")
      ),
      col_6(
        div(
          class = "d-flex align-items-center gap-2 mb-2",
          span(class = "badge bg-danger", "Adverse"),
          span(uiOutput("byoutcome_adverse_count"), class = "text-muted fs-xs")
        ),
        uiOutput("byoutcome_adverse_grid")
      )
    )
  )
}
