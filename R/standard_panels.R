support_panel <- function() {
  tabPanel(
    "Support and feedback",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          heading_text("Support and feedback", size = "l", level = 1),
          br(),
          heading_text("Give us feedback", size = "m", level = 2),
          "This dashboard is a new service that we are developing. If you have any feedback or suggestions for improvements, please submit them using our ",
          a(
            href = "https://forms.cloud.microsoft/pages/responsepage.aspx?id=yXfS-grGoU2187O4s0qC-Zz2vw_NbKtFvp9cE0vbZ2tUQ1JINVFJSzY4QkpFMkdDOFc4MEJQRkVSRS4u&route=shorturl",
            "feedback form",
            .noWS = c("after")
          ),
          ".",
          br(),
          "Alternatively, if you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
          a(
            href = "mailto:attainment.statistics@education.gov.uk",
            "attainment.statistics@education.gov.uk",
            .noWS = c("after")
          ),
          ".",
          br(),
          heading_text("Find more information on the data", size = "m", level = 2),
          "Data on A level and other 16 to 19 results along with methodological information can be found on",
          a(
            href = "https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results",
            "Explore Education Statistics",
            .noWS = c("after")
          ),
          ".",
          br(),
          heading_text("Contact us", size = "m", level = 2),
          "If you have questions about the dashboard or data within it, please contact us at ",
          a(
            href = "mailto:attainment.statistics@education.gov.uk",
            "attainment.statistics@education.gov.uk",
            .noWS = c("after")
          ),
          br(),
          heading_text("See the source code", size = "m", level = 2),
          "The source code for this dashboard is available in our ",
          a(
            href = "https://github.com/dfe-analytical-services/ks5-timeseries-attainment-and-single-year-entries",
            "GitHub repository",
            .noWS = c("after")
          ),
          ".",
          br(),
          br(),
          br(),
          br(),
          br(),
          br()
        )
      )
    )
  )
}
