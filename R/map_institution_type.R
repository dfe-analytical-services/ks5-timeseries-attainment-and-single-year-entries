flow_panel <- function() {
  tabPanel(
    "Institution flow diagram",
    gov_main_layout(
      gov_row(
        column(width=12,
               h1("Flow diagram of institution types"),
               br("This diagram shows the institution types and groups covered within the attainment part of the dashboard."),
               br(),
            
               tags$div(
                 tags$img(src = "images/institute_type_group.svg", alt="Flow diagram of institution types"))
          
        )
      )
    )
  )
}
