homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          heading_text(
            "16 to 19 time series attainment and single year entry dashboard",
            level = 1,
            size = "l"
          )
        )
      ),
      br(),
      br(),


      # Left panel ---------------------------------------------------------------------------------------------------------------------------------------------------------


      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(
            heading_text("Contents", size = "m", level = 2)
          ),
          card_body(
            gov_text("This dashboard allows users to explore time series data for 16 to 18 qualifications in England.
                    It brings together figures compiled from the current and previous versions of the (A level and other 16 to 18 results) statistical release with focus on:"),
            tags$div(
              title = "Headline attainment and A level time series by institution type",
              h3(actionLink("link_to_headline_tab", "Headline attainment and A level result by institution type")),
              # p(" The headline attainment does not include vocational and technical qualifications for 2024 provisional data due to a data collection issue. This will be resolved in the revised publication."),
              gov_text("The year in tables/charts is the academic year in which students completed 16-18 study (i.e. 2025 means 2024/25). 16-18 study normally lasts either 2 or 3 years and attainment accounts for all entries during that period."),
              gov_text("Drop-down box within the chart area allows for selection of one institution type.  To view latest results and trend, click on the drop-down box and  select institution type required."),
              h4("APS for A level by all & sex"),
              gov_text("The line charts display the yearly trend and grades for all students and by sex, while the female - male sex gap shows the difference in average point score.
                   To view and explore trend select up to four institution types from the drop-down menu at the top of the page."),
              tags$div(
                title = "(A level students entering maths and science by sex",
                h3(actionLink("link_to_alevelMathsScience_tab", "A level students entering maths and science by sex")),
                gov_text("The line chart shows the yearly trend in the proportion of A level students entering maths and science  from 2009/10 to 2024/25 (end of 16-18 study).
                  Select one subject or combination from the dropdown menu at the top of the page, followed by the start year to view changes over time (end year is fixed at the latest year available).")
              ),
              tags$div(
                title = "(A level subject entry and grade: comparison by subject",
                h3(actionLink("link_to_alevelAllSubject_tab", "A level subject entry and grade: comparison by subject")),
                gov_text("A level data on subject entry refers to all entries by 16-18 year olds in the year stated (where 2025 means 2024/25 academic year)."),
                gov_text("The line charts and cumulative percentage grades display the yearly trend from 1995/96 to 2024/25.
                 Drop-down menus at the top of the page allows for the selection and comparison of up to four subjects
                  and cumulative grades.
                    ")
              ),
              tags$div(
                title = "A level subject entry and result: comparison by sex",
                h3(actionLink("link_to_alevelFmSubject_tab", "A level subject entry and result: comparison by sex")),
                gov_text("The line charts for subject entries and cumulative percentage grades display the yearly trend for female and male from 1995/96 to 2024/25.

                  Select one subject from the dropdown menu at the top of the page, followed by the start year to view changes over time (end year is fixed at the latest year available).")
              )
            )
          )
        ),

        # Right panel ------------------------------------------------------------------------------------------------------------------------------------------------------

        card(
          card_header(
            heading_text("Background Information", size = "m", level = 2)
          ),
          card_body(
            tags$div(
              title = "Attainment by institution type",
              h3("Attainment by institution type"),
              gov_text("Headline attainment is the average point score (APS) per entry also expressed as a grade.
                  Coverage includes A level, applied general and tech level qualifications.  A level APS per entry started in 2012/13 with
                  a points scale between 0-300 where grade A* was 300 points and a grade E was equal to 150 points.
                  A simpler points scale of 0-60 points was introduced in 2015/16, where a grade A* is given 60 points
                  and a grade E is given 10 points (U/fail is given 0 points in both old and current point scales).
                  This is shown in the A level APS panel for all students.  A level APS per entry is presented across 2 charts,
                  scaled such that a change of one grade appears the same for both the old and current points scale.
                  Applied general and tech level APS per entry started in 2015/16 with a points scale between 0-50."),
              gov_text("Full details of the headline attainment measure can be found in the "),
              tags$a(
                href = "https://www.gov.uk/government/publications/16-to-19-accountability-headline-measures-technical-guide",
                "technical guidance (16 to 18 accountability headline measures - GOV.UK)"
              )
            ),
            tags$div(
              title = "Comparing institution types",
              h3("Comparing institution types"),
              gov_text("The dropdown menu at the top of the page for A level APS per entry allows for the selection of up to four
                  institution types.  Care should be taken when comparing across institution types due to significant
                  differences in cohort sizes, for example there are very low numbers of students in free schools,
                  16-19 free schools, university technical colleges and studio schools compared with other institution types.")
            ),
            tags$div(
              title = "A level subject entry and cumulative grade",
              h3("A level subject entry and cumulative grade"),
              gov_text("The subject entries and cumulative grades covered on the dashboard are based on
                  subjects grouped from 1995/96 to 2024/25. More detailed breakdowns of some subjects from 2015/16
                  are available via the full dataset download, at the top of the page."),
              h4("Subject definitions"),
              gov_text("Total English covers English literature, English language, English language and literature. "),
              gov_text("Total Maths covers Maths, Pure maths, Statistics, Use of maths and Other maths"),
              gov_text("Total Modern languages cover French, German, Spanish and other foreign modern languages. "),
              gov_text("Total Classical studies covers Latin, Greek, Classical civilisation, and Other classical studies"),
              gov_text("STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science. "),
              gov_text("Total Music includes Music and Music technology."),
              gov_text("The data refers to all entries by 16-18 year olds in the year stated (where 2025 means 2024/25 academic year).")
            )
          )
        )
      )
    )
  )
}

# --------------- Headline APS and A level APS Panel -------------------------------

dashboard_panel_aps <- function() {
  tabPanel(
    value = "dashboard",
    "Attainment: APS per entry & average result (end of 16-19 study)",
    gov_main_layout(
      width = "standard",
      gov_row(
        column(
          width = 12,
          heading_text(
            level = 2,
            size = "m",
            "Headline attainment and A level time series by institution type in England"
          )
        ),
        expandable(
          inputId = "details", label = textOutput("dropdown_label"),
          contents =
            card(
              class = "border-0 shadow-none",
              layout_columns(
                col_widths = c(4, 8),
                selectizeInput(
                  inputId = "allSex",
                  label = "Select students",
                  choices = unique(dfAlevelAps$characteristic_sex) %>%
                    sort(), selected = "All students", width = "100%"
                ),
                selectizeInput("alevelInstitute", "Select up to 4 institution types",
                  choices = list(
                    `Institution types` = c(
                      "All institutions", "All FE sector colleges", "All independent schools", "All schools", "All state-funded",
                      "All state-funded schools"
                    ),
                    `All FE sector colleges` = c("Other FE sector colleges", "Sixth form colleges"),
                    `All independent schools` = c("Independent schools", "Independent special schools"),
                    `All state-funded schools` = c(
                      "Converter academies - mainstream", "Free schools", "Free schools 16-19", "LA maintained mainstream schools",
                      "Sponsored academies - mainstream", "Studio schools",
                      "University technical colleges (UTCs)"
                    )
                  ), multiple = TRUE, options = list(maxItems = 4),
                  selected = c("All FE sector colleges", "All state-funded schools"), width = "100%"
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  gov_text(strong("Download full dataset")),
                  downloadButton(
                    outputId = "downloadDataAps",
                    label = "Download data",
                    icon = shiny::icon("download"),
                    class = "downloadButton"
                  )
                ),
                column(
                  width = 4,
                  gov_text(strong("For more tables and metadata")),
                  actionButton(
                    inputId = "ees_1",
                    label = "Visit Explore Education Statistics",
                    icon = icon("table-cells"),
                    onclick = "window.open('https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results/2024-25', '_blank')",
                    style = "width:100%;white-space:normal;"
                  )
                ),
                column(
                  width = 4,
                  gov_text(strong("Reset chart selection")),
                  align = "right",
                  actionButton(inputId = "resetApsAll", label = "Reset selections", icon = icon("fas fa-arrows-rotate"))
                )
              )
            )
        )
      ),
      column(
        width = 12,
        tabsetPanel(
          id = "tabsetpanels",
          tabPanel(
            value = "headline",
            "Headline attainment",
            gov_main_layout(
              column(
                width = 12,
                h3(" 2025 Average results and time series for level 3 attainment, all students"),

                # The uiOutput cover information for value box.  Value boxes are covered in the server.
                fluidRow(
                  column(4, uiOutput("headBox1")),
                  bsTooltip("headBox1", "Average result - A level", placement = "top", trigger = "hover", options = NULL),
                  column(4, uiOutput("headBox2")),
                  bsTooltip("headBox2", "Average result - Applied general", placement = "top", trigger = "hover", options = NULL),
                  column(4, uiOutput("headBox3")),
                  bsTooltip("headBox3", "Average result - Tech Level", placement = "top", trigger = "hover", options = NULL)
                ),
                layout_columns(
                  col_widths = c(8, 4),
                  textOutput("textHeadline"),
                  card(
                    fill = TRUE,
                    class = "border-0 shadow-none",
                    selectInput(
                      inputId = "headlineAps",
                      "Select institution type",
                      choices = sort(unique(dfAttainment$establishment_type)), selected = c("All institutions")
                    )
                  )
                ),
                column(
                  width = 12,
                  card(
                    width = 12,
                    plotlyOutput("plotHeadline") %>% spinner()
                  )
                ),
                dataTableOutput("tabHeadline")
              )
            )
          ),
          tabPanel(
            "A level APS by all",
            value = "alevel_all",
            gov_main_layout(
              h3("A level average point score (APS) per entry and grade by institution type"),
              htmlOutput("textApsAll"),
              column(
                width = 12,
                card(
                  width = 12,
                  plotOutput("plotAlevelAps") %>% spinner()
                )
              ),
              DTOutput("tabApsAll")
            )
          ),
          tabPanel(
            "A level APS sex gap",
            value = "ggap",
            gov_main_layout(
              h3("A level female - male average points difference by institution type in England"),
              textOutput("textGgap"),
              card(
                width = 12,
                plotlyOutput("plotGgap") %>% spinner()
              )
            )
          ),
          tabPanel(
            "A level APS by sex",
            value = "alevel_fm",
            gov_main_layout(
              h3("A level average point score and grade for female and male by Institution type"),
              column(
                width = 12,
                textOutput("textApsFm")
              ),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  plotOutput("plotFemaleAlAPS") %>% spinner()
                ),
                card(
                  plotOutput("plotMaleAlAPS") %>% spinner()
                )
              ),
              DTOutput("tabFm")
            )
          )
        )
      )
    )
  )
}


## -------------------- Subject entries and comparison for all------------------


dashboard_panel_sub_all <- function() {
  tabPanel(
    value = "dashboard_alse",
    "A level subject entry & grade: comparison by subject (single academic year)",
    gov_main_layout(
      heading_text(
        "A level single entry and grade distribution:  Comparison by subject, England from 1995/96 to 2024/25",
        level = 2,
        size = "m"
      ),
      card(
        card_body(
          class = "border-0 shadow-none",
          layout_columns(
            selectInput(
              inputId = "subByAll",
              label = "Select students",
              choices = unique(subjectByAll$characteristic_value) %>%
                sort(), selected = "All students"
            ),
            selectizeInput(
              inputId = "subCompareAll", label = "Select up to 4 subjects to compare:",
              choices = sort(unique(subjectByAll$subject_name)),
              multiple = T,
              options = list(maxItems = 4), selected = c("Total Maths", "Total English")
            ),
            selectInput(
              inputId = "year_start",
              label = "Start year",
              choices = seq(1996, 2018, 1),
              selected = 1996
            ),
            selectInput(
              inputId = "year_end",
              label = " End year",
              choices = latest_year
            ),
            col_widths = c(3, 5, 2, 2)
          ),
          layout_columns(
            div(
              p("Download full dataset"),
              downloadButton(
                outputId = "downloadDataSubject",
                label = "Download data",
                icon = shiny::icon("download")
              )
            ),
            div(
              p("For more tables and metadata"),
              actionButton(
                inputId = "ees_2a",
                label = "Visit Explore Education Statistics",
                icon = icon("table-cells"),
                onclick = "window.open('https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results/2024-25', '_blank')",
                style = "width:100%;"
              )
            ),
            div(
              p("Reset chart selection"),
              actionButton(inputId = "resetEntries", label = "Reset selections", icon = icon("fas fa-arrows-rotate"))
            ),
            col_widths = c(3, 6, 3)
          )
        )
      ),
      gov_main_layout(
        tabsetPanel(
          id = "tabsetpanels2a",
          tabPanel(
            value = "alevelAllSubject",
            "Subject entries by all",
            h3("A level single academic year:  Comparison by subject"),
            layout_columns(
              col_widths = c(8, 4),
              textOutput("textSubAll"),
              div(
                # gov_text("Download chart data:"),
                # br(),
                downloadButton(
                  outputId = "downloadSubAll",
                  label = "Download data",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                )
              )
            ),
            card(
              plotlyOutput("plotAlevelSubject") %>% spinner(),
              gov_text("Notes:
                              - Total English covers English literature, English language, English language and literature.
                                  - Total Maths covers Maths, Pure maths, statistics, Use of maths and Other maths.
                                  - Total Modern languages cover French, German, Spanish and Other foreign modern languages.
                                  - Total Classical studies covers Latin, Greek, Classical civilisation and Other classical studies.
                                  - STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science.
                                  - Total Music includes Music and Music technology.")
            )
          ),
          tabPanel(
            "Cumulative results by all",
            h3("A level cumulative percentage grade"),
            layout_columns(
              col_widths = c(9, 3),
              textOutput("textSubResultAll"),
              selectInput(
                inputId = "resByAll",
                "Select cumulative result",
                choices = list(
                  "A*-A" = "`A*-A`",
                  "A*-B" = "`A*-B`",
                  "A*-C" = "`A*-C`",
                  "A*-D" = "`A*-D`",
                  "A*-E" = "`A*-E`"
                )
              )
            ),
            br(),
            div(
              # gov_text("Download chart data & results:"), br(),
              downloadButton(
                outputId = "downloadResAll",
                label = "Download data",
                icon = shiny::icon("download"),
                class = "downloadButton"
              )
            ),
            card(
              plotlyOutput("plotResultAll") %>% spinner(),
              gov_text("Notes: A* grade was first awarded in 2010. - Total English covers English literature, English language, English language and literature.
                                  - Total Maths covers Maths, Pure maths, statistics, Use of maths and Other maths.
                                  - Total Modern languages cover French, German, Spanish and Other foreign modern languages.
                                  - Total Classical studies covers Latin, Greek, Classical civilisation and Other classical studies.
                                  - STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science.
                                  - Total Music includes Music and Music technology.")
            )
          )
        )
      )
    )
  )
}

# ---------------------- Subject entries and results for Female, Male and all -----------------

dashboard_panel_sub_fm <- function() {
  tabPanel(
    value = "dashboard_fm",
    "A level subject entry & result: comparison by sex (single academic year)",
    gov_main_layout(
      heading_text("A level subject entry and grade distribution: comparison by sex, England from 1995/96 to 2024/25",
        level = 2,
        size = "m"
      )
    ),
    card(
      card_body(
        class = "border-0 shadow-none",
        layout_columns(
          selectizeInput(
            inputId = "subjectFm", label = "Select a subject:",
            choices = sort(unique(subjectBySex$subject_name)),
            multiple = F,
            selected = c("Total English")
          ),
          selectInput(
            inputId = "year_start_fm",
            label = "Select start year",
            choices = seq(1996, latest_year, 1),
            selected = 1996
          ),
          selectInput(
            inputId = "year_end_fm",
            label = " End year",
            choices = latest_year,
            selected = latest_year
          ),
          col_widths = c(6, 3, 3)
        ),
        layout_columns(
          div(
            p("Download full dataset"),
            downloadButton(
              outputId = "downloadDataSubjectFm",
              label = "Download data",
              icon = shiny::icon("download"),
              class = "downloadButton"
            )
          ),
          div(
            p("For more tables and metadata"),
            actionButton(
              inputId = "ees_2b",
              label = "Visit Explore Education Statistics",
              icon = icon("table-cells"),
              onclick = "window.open('https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results/2024-25', '_blank')",
              style = "width:100%;white-space:normal;"
            )
          ),
          div(
            p("Reset chart selection"),
            actionButton(inputId = "resetSubFm", label = "Reset selections", icon = icon("fas fa-arrows-rotate"))
          ),
          col_widths = c(3, 6, 3)
        )
      )
    ),
    gov_main_layout(
      tabsetPanel(
        id = "tabsetpanels2b",
        tabPanel(
          value = "alevelFmSubject",
          "Subject entries by sex",
          h3("A level subject: Comparison by sex"),
          layout_columns(
            col_widths = c(8, 4),
            textOutput("textSubFm"), br(),
            div(
              # gov_text("Download chart data:"),
              # br(),
              downloadButton(
                outputId = "downloadSubFm",
                label = "Download data",
                icon = shiny::icon("download"),
                class = "downloadButton"
              )
            )
          ),
          card(
            plotlyOutput("plotSubjectFm") %>% spinner(),
            gov_text("Notes:
                                  - Total English covers English literature, English language, English language and literature.
                                  - Total Maths covers Maths, Pure maths, statistics, Use of maths and Other maths.
                                  - Total Modern languages cover French, German, Spanish and Other foreign modern languages.
                                  - Total Classical studies covers Latin, Greek, Classical civilisation and Other classical studies.
                                  - STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science.
                                  - Total Music includes Music and Music technology.")
          )
        ),
        tabPanel(
          "Cumulative results by sex",
          h3("A level cumulative percentage grade by subject and sex"),
          layout_columns(
            col_widths = c(9, 3),
            textOutput("textResFm"),
            selectInput(
              inputId = "resByFm",
              "Select cumulative result",
              choices = list(
                "A*-A" = "`A*-A`",
                "A*-B" = "`A*-B`",
                "A*-C" = "`A*-C`",
                "A*-D" = "`A*-D`",
                "A*-E" = "`A*-E`"
              )
            )
          ),
          br(),
          div(
            # p("Download chart data & results:"), br(),
            downloadButton(
              outputId = "downloadResFm",
              label = "Download data",
              icon = shiny::icon("download"),
              class = "downloadButton"
            )
          ),
          card(
            plotlyOutput("plotResultFm") %>% spinner(),
            gov_text("Notes: A* grade was first awarded  in 2010. - Total English covers English literature, English language,  English language and literature.
                                - Total Maths covers Maths, Pure maths, statistics, Use of maths and Other maths.
                                - Total Modern languages cover French, German, Spanish and Other foreign modern languages.
                                - Total Classical studies covers Latin, Greek, Classical civilisation and Other classical studies.
                                - STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science.
                                - Total Music includes Music and Music technology.")
          )
        )
      )
    )
  )
}


# -------------------------  Maths and science  ----------------------------------

dashboard_panel_ms <- function() {
  tabPanel(
    value = "dashboard_ms",
    "A level maths & science: comparison by sex (end of 16-18 study)",
    gov_main_layout(
      heading_text(
        "A level maths and science entry (end of 16 to 18 study): comparison by sex, England from 2009/10 to 2024/25",
        level = 2,
        size = "m"
      ),
      card(
        card_body(
          class = "border-0 shadow-none",
          layout_columns(
            selectizeInput(
              inputId = "subjectMs", label = "Select one subject or combination",
              choices = list(
                `Single subject` = list(
                  "Biology",
                  "Chemistry",
                  "Computer science",
                  "Further maths",
                  "Maths",
                  "Physics"
                ),
                `Combination` = list(
                  "One maths/science subject",
                  "Two maths/science subjects",
                  "Three maths/science subjects",
                  "Four maths/science subjects",
                  "Zero maths/science subjects"
                ), selected = c("Maths")
              )
            ),
            selectInput(
              inputId = "year_start_ms",
              label = "Select start year",
              choices = seq(2010, latest_year, 1),
              selected = 2010
            ),
            selectInput(
              inputId = "year_end_ms",
              label = " End year",
              choices = latest_year,
              # choices = seq(1996, latest_year, 1),
              selected = latest_year
            ),
            col_widths = c(6, 3, 3)
          ),
          layout_columns(
            div(
              p("Download full dataset"),
              downloadButton(
                outputId = "downloadDataSubjectMs",
                label = "Download data",
                icon = shiny::icon("download")
                # class = "downloadButton"
              )
            ),
            div(
              p(strong("For more tables and metadata")),
              actionButton(
                inputId = "ees_3",
                label = "Visit Explore Education Statistics",
                icon = icon("table-cells"),
                onclick = "window.open('https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results/2024-25', '_blank')",
                style = "width:100%;white-space:normal;"
              )
            ),
            div(
              p(strong("Reset chart selection")),
              align = "right",
              actionButton(inputId = "resetSubMs", label = "Reset selections", icon = icon("fas fa-arrows-rotate"))
            ),
            col_widths = c(4, 4, 4)
          )
        )
      ),
      column(
        width = 12,
        tabsetPanel(
          id = "tabsetpanel3",
          tabPanel(
            value = "alevelMsSubject",
            "Maths and Science",
            h3("A level maths and science: Comparison by sex"),
            gov_main_layout(
              textOutput("textSubMs"),
              column(
                width = 12,
                card(
                  width = 12,
                  plotlyOutput("plotSubjectMs") %>% spinner(),
                  gov_text("Note: The number of students from 2009/10 to 2017/18 has been calculated from rounded data, so less precise than the number of students
                  from 2018/19 to 2024/25")
                )
              ),
              DTOutput("tabMs")
            )
          )
        )
      )
    )
  )
}
