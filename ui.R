# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#



ui <- function(input, output, session) {
  fluidPage(
    # use_tota11y(),
    title = tags$head(tags$link(
      rel = "shortcut icon",
      href = "dfefavicon.png"
    )),
    shinyjs::useShinyjs(),
    useShinydashboard(),
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    shinyGovstyle::header(
      main_text = "DfE",
      main_link = "https://www.gov.uk/government/organisations/department-for-education",
      secondary_text = "16 to 18 time series attainment and single year entry",
      logo = "images/DfE_logo.png"
    ),
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      paste0(
        "<b>We're looking for volunteers! We've developed several new dashboards ",
        "in the last 12 months and we'd really like to know what you think of them. ",
        "If you're interested in helping us improve our products, please sign up ",
        "using our <a href='https://forms.office.com/e/ZjNxf10uuN'>user-testing volunteer form</a>.</b><br>",
        "This Dashboard is in beta phase and we are still reviewing performance and reliability. "
      )
    ),
    shiny::navlistPanel(
      "",
      id = "navlistPanel",
      widths = c(2, 8),
      well = FALSE,
      homepage_panel(),
      dashboard_panel_aps(),
      dashboard_panel_sub_all(),
      dashboard_panel_sub_fm(),
      a11y_panel(),
      support_links()
    ),
    tags$script(
      src = "script.js"
    ),
    footer(full = TRUE)
  )
}
