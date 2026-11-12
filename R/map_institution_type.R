flow_panel <- function() {
  nav_panel(
    "flow_panel",
    gov_main_layout(
      width = "standard",
      gov_row(
        full = TRUE,
        heading_text("Flow diagram of institution types", size = "m", level = 2),
        br(
          "This diagram shows the institution types and groups covered within the attainment part of the dashboard."
        ),
        br(),
        tags$div(
          tags$img(
            src = "images/institute_type_group.svg",
            alt = "Flow diagram of institution types"
          )
        )
      )
    )
  )
}
