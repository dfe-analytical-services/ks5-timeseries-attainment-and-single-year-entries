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
  gov_page(
    width = "full",
    bslib::page_fluid(
      # shina11y::use_totally(),
      tags$head(HTML("<title> site_title </title>")),
      tags$head(tags$link(rel = "shortcut icon", href = "dfefavicon.png")),
      use_shiny_title(),
      useShinyjs(),
      tags$html(lang = "en"),
      tags$head(tags$style(HTML(".dt-buttons .btn.btn-secondary {
        background-color: #1d70b8 !important;
        border-color: #1d70b8 !important;
        color: #ffffff !important;
      } "))),


      # Add meta description for search engines
      meta() %>%
        meta_general(
          application_name = site_title,
          description = site_title,
          robots = "index,follow",
          generator = "R-Shiny",
          subject = "16 to 18 statistics",
          rating = "General",
          referrer = "no-referrer"
        ),

      # Custom disconnect function ----------------------------------------------
      # Variables used here are set in the global.R file

      dfeshiny::custom_disconnect_message(
        dashboard_title = site_title,
        publication_name = ees_pub_name,
        publication_link = ees_publication
      ),

      # Load javascript dependencies --------------------------------------------
      shinyjs::useShinyjs(),

      # Cookies -----------------------------------------------------------------
      # Setting up cookie consent based on a cookie recording the consent:
      dfeshiny::dfe_cookies_script(),
      dfeshiny::cookies_banner_ui(
        name = site_title
      ),


      # Skip_to_main -------------------------------------------------------------
      # Add a 'Skip to main content' link for keyboard users to bypass navigation.
      # It stays hidden unless focussed via tabbing.
      shinyGovstyle::skip_to_main(),

      # Google analytics --------------------------------------------------------
      tags$head(includeHTML(("google-analytics.html"))),

      # Header ------------------------------------------------------------------

      shinyGovstyle::header(
        org_name = "",
        service_name = site_title # "16 to 19 time-series attainment and single year entry"
      ),

      # Beta banner
      shinyGovstyle::banner(
        "beta banner",
        "Beta",
        paste0(
          "This Dashboard is in beta phase and we are still reviewing performance and reliability."
        )
      ),
      # see dashboard_panels for coverage

      gov_main_layout(
        bslib::navset_hidden(
          id = "pages",
          nav_panel(
            "dashboard",
            # Dashboard nav_panels -----------------------------------------------
            shiny::navlistPanel(
              "",
              id = "navlistPanel",
              widths = c(2, 7),
              well = FALSE,
              homepage_panel(),
              dashboard_panel_aps(),
              dashboard_panel_ms(),
              dashboard_panel_sub_all(),
              dashboard_panel_sub_fm(),
              flow_panel()
              # a11y_panel(),
              # support_panel()
            )
          ),
          nav_panel(
            value = "accessibility_statement",
            "Accessibility",
            layout_columns(
              col_widths = c(-2, 8, -2),
              actionLink(
                class = "govuk-back-link",
                style = "margin: 0",
                "accessibility_to_dashboard",
                "Back to dashboard"
              ),
              dfeshiny::a11y_panel(
                dashboard_title = site_title,
                dashboard_url = site_primary,
                date_tested = "03 November 2026",
                date_prepared = "03 November 2026",
                date_reviewed = "03 November 2026",
                issues_contact = "attainment.statistics@education.gov.uk",
                publication_name = "A level and other 16 to 18 results",
                publication_slug = "a-level-and-other-16-to-18-results",
                non_accessible_components = c(
                  ""
                ),
                specific_issues = c("Keyboard navigation through the interactive charts is
                                    currently limited, and some features are unavailable for keyboard only users,
                                    Line colours on charts may not line up with legends.")
              )
            )
          ),
          nav_panel(
            value = "cookies_panel_ui",
            "use of cookies",
            layout_columns(
              col_widths = c(-2, 8, -2),

              # Add backlink
              actionLink(
                class = "govuk-back-link",
                style = "margin: 0",
                "cookies_to_dashboard",
                "Back to dashboard"
              ),
              cookies_panel_ui(google_analytics_key = google_analytics_key)
            )
          ),
          nav_panel(
            value = "support_panel",
            "Support and feedback",
            # Set up column layout to center it ---------------------------------------
            layout_columns(
              col_widths = c(-2, 8, -2),

              # Add in back link
              actionLink(
                class = "govuk-back-link",
                style = "margin: 0",
                "support_to_dashboard",
                "Back to dashboard"
              ),
              support_panel()
            )
          )
        )
        # )
      ),

      # Footer ------------------------------------------------------------------
      shinyGovstyle::footer(
        full = TRUE,
        links = c(
          "Support and feedback",
          "Accessibility statement",
          "Use of cookies"
        )
      )
    )
  )
}
