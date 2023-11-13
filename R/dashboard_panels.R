homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("16 to 18  time series attainment and single year entry dashboard"),
          br(),
          br()
        ),
        
        ## Left panel -------------------------------------------------------
        
        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Contents")
              ),
              div(
                class = "panel-body",
                tags$div(
                  title = "",
                  br(),
                  p("This dashboard allows users to explore time series data for 16 to 18 qualifications in England.  
                    It brings together figures compiled from the current and previous versions of the ‘A level and other 16 to 18 results’ statistical release with focus on:") 
                  
                  ),
               
                tags$div(
                  title="Headline attainment and A level time series by institution type",
                  h3(actionLink("link_to_headline_tab", "Headline attainment and A level result by institution type")),
                  p("The headline boxes show the latest average result for A level, applied general and tech level qualifications for all students.  The chart shows the yearly trend from 2015/16 to 2022/23.  "),
                  
                  p("The year in tables/charts is the academic year in which students completed 16-18 study (i.e. 2023 means 2022/23). 16-18 study normally lasts either 2 or 3 years and attainment accounts for all entries during that period."),
                 
                  p("Drop-down box within the chart area allows for selection of one institution type.  To view latest results and trend, click on the drop-down box and  select institution type required."),
                  
                 
                  h4("APS for A level by all & gender"),
                  p("The line charts display the yearly trend and grades for all students and by gender, while the female - male gender gap shows the difference in average point score.
                   To view and explore trend select up to four institution types from the drop-down menu at the top of the page."    
                  ),
                       
                tags$div(
                  title="(A level subject entries and result by all",
                  h3(actionLink("link_to_alevelAllSubject_tab", "A level subject entry and result by all")),
                  p("The line charts for subject entries and cumulative percentage grades display the yearly trend from 1995/96 to 2022/23. 
                 Drop-down menus at the top of the page allows for the selection and comparison of up to four subjects 
                  and cumulative grades.
                    "),
                  p("The data refers to all entries by 16-18 year olds in the year stated (where 2023 means 2022/23 academic year).")
                  
                ),
             
                tags$div(
                  title="(A level subject entries and result by gender",
                  h3(actionLink("link_to_alevelFmSubject_tab", "A level subject entry and result by gender")),
                  p("The line charts for subject entries and cumulative percentage grades display the yearly trend for female and male from 1995/96 to 2022/23.  
                  Select one subject from the dropdown menu at the top of the page, followed by the start year to view changes over time (end year is fixed at the latest year available)."),
                  p(""), br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                  
                  
                ))
               
              )
            )
          )
        ),
        
        ## Right panel ------------------------------------------------------
        
        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Background Information")
              ),
              div(
                class = "panel-body",
                
                tags$div(
                    title="Attainment by institution type",
                    h3("Attainment by institution type"),
                  
                  p("Headline attainment is the average point score (APS) per entry also expressed as a grade. 
                  Coverage includes A level, applied general and tech level qualifications.  A level APS per entry started in 2012/13 with 
                  a points scale between 0-300 where grade A* was 300 points and a grade E was equal to 150 points. 
                  A simpler points scale of 0-60 points was introduced in 2015/16, where a grade A* is given 60 points
                  and a grade E is given 10 points (U/fail is given 0 points in both old and current point scales).  
                  This is shown in the A level APS panel for all students.  A level APS per entry is presented across 2 charts,
                  scaled such that a change of one grade appears the same for both the old and current points scale.
                  Applied general and tech level APS per entry started in 2015/16 with a points scale between 0-50."
                    ),
                  
                  p("Full details of the headline attainment measure can be found in the "),
                  tags$a(href = "https://www.gov.uk/government/publications/16-to-19-accountability-headline-measures-technical-guide", 
                         "technical guidance (16 to 18 accountability headline measures - GOV.UK)")

                ),
                
                tags$div(
                  
                  title="Comparing institution types",
                  h3("Comparing institution types"),
                  
                  p("The dropdown menu at the top of the page for A level APS per entry allows for the selection of up to four 
                  institution types.  Care should be taken when comparing across institution types due to significant
                  differences in cohort sizes, for example there are very low numbers of students in free schools,
                  16-19 free schools, university technical colleges and studio schools compared with other institution types.")
                ),
                
                tags$div(
                  
                  title="A level subject entry and cumulative grade",
                  h3("A level subject entry and cumulative grade"),
                  
                  p("The subject entries and cumulative grades covered on the dashboard are based on 
                  subjects grouped from 1995/96 to 2022/23. More detailed breakdowns of some subjects from 2015/16
                  are available via the full dataset download  button at the top of the page."),
                
                h4("Subject definitions"),
                p("•	Total English covers English literature, English language, English language and literature. "),
                p("•	Total Maths covers Maths, Pure maths, Statistics, Use of maths and Other maths"),
                p("•	Total Modern languages cover French, German, Spanish and other foreign modern languages. "),
                p("•	Total Classical studies covers Latin, Greek, Classical civilisation, and Other classical studies"),
                p("•	STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science. "),
                p("•	Total Music includes Music and Music technology."),
                p("The data refers to all entries by 16-18 year olds in the year stated (where 2022 means 2022/23 academic year).") 
             
            ))
            ) 
          ))
  )))
}


dashboard_panel_aps <- function() {
  tabPanel(
    value = "dashboard",
    "Attainment: APS per entry and average result",
    
    # Define UI for application that draws a histogram
    
    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width=12,
        h2("Headline attainment and A level time series by institution type in England")
        ),
      column(
          width=12,
          expandable(
            inputId = "details", label = textOutput("dropdown_label"), contents =
              div(
                id = "div_a",
                class = "well",
                style = "min-height: 100%; height: 100%; overflow-y: visible",
          
      
          gov_row(
              column(
                width = 3,
                
                selectizeInput(inputId = "allGender",
                             label = "Select students",
                             choices =  unique(dfAlevelAps$characteristic_gender)%>%
                               sort(), selected = "All students"
                             )
                ),
              
              column(
              width=9,
              
              selectizeInput("alevelInstitute", "Select up to 4 institution types", choices=list(

                `Institution types` = c("All Institutions", "All FE sector colleges", "All independent schools",  "All schools", "All state-funded",
                                        "All state-funded schools"),
                `All FE sector colleges` = c("Other FE sector colleges", "Sixth form colleges"),
                `All independent schools` = c("Independent schools", "Independent special schools"),
                `All state-funded schools` = c("Converter academies - mainstream", "Free schools", "Free schools 16-19", "LA maintained mainstream schools",
                                               "Sponsored academies - mainstream", "Studio schools",
                                               "University technical colleges (UTCs)")), multiple=TRUE, options = list(maxItems = 4),
                selected = c("All FE sector colleges", "All state-funded schools"))


              )
            ),
          
          fluidRow(
            column(
              width = 4,
              p(strong("Download full dataset")), 
              downloadButton(
                outputId = "downloadDataAps",
                label= "Download data",
                icon = shiny::icon("download"),
                class = "downloadButton"
       
          )
        ),
        
        
        column(
          width=4,
          p(strong("For more tables and metadata")),
          actionButton(inputId='ees_1', 
                       label="Visit Explore Education Statistics", 
                       icon = icon("table-cells"), 
                       onclick ="window.open('https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results/2021-22', '_blank')",
                       style = "width:100%;white-space:normal;"
          
        
            )
          ),
        
        
        column(
          width=4, 
          p(strong ("Reset chart selection")), 
          align = "right",
                        actionButton(inputId = "resetApsAll", label = 'Reset selections', icon = icon("fas fa-arrows-rotate"))
            )
            )
        ))
        
        ),
        
        column(
          width=12,
               tabsetPanel(id = "tabsetpanels",
                           
                           
                           tabPanel(value="headline",
                             "Headline attainment", 
                             
                             
                             fluidRow(
                               column(
                                
                                 width=12,
                                 
                                 
                                 h3(" 2023 Average result and time series for level 3 attainment, all students"),
                                 fluidRow( 
                                   valueBoxOutput("headBox1" , width=4
                                                  
                                                  
                                                  
                                   ),
                                    bsTooltip("headBox1", "Average A level result", placement="top", trigger="hover", options=NULL),
                                    valueBoxOutput("headBox2" , width=4

                                   ),
                                   
                                   bsTooltip("headBox2", "Average result, Applied general", placement="top", trigger="hover", options=NULL),
                                   valueBoxOutput("headBox3", width=4
                                   ),
                                   bsTooltip("headBox3", "Average result, Tech Level", placement="top", trigger="hover", options=NULL)),
                                column(
                                  width= 9, textOutput("textHeadline")), 
                                  column(
                                    width=3, 
                                    div(
                                      class = "well",
                                      style = "min-height: 100%; height: 100%; overflow-y: visible",
                                      fluidRow(
                                        column(
                                          width=12,
                                          selectInput(inputId="headlineAps",
                                                      "Select institution type",
                                                      choices=sort(unique(dfAttainment$school_type)), selected=c("All Institutions"))
                                        )
                                      )
                                    ) 
                                  ),
                                
                                 
                                 column(
                                   width=12,
                                   
                                   box(
                                     width=12,
                                     plotlyOutput("plotHeadline") %>% spinner()
                                   )
                                  ),
                                
                                # ),
                                   
                                dataTableOutput("tabHeadline")
                               
                                 )
                              )
                            ),
                           
                          tabPanel(
                             "A level APS by all", value="alevel_all",
                             fluidRow(
                               column(
                                 width=12,
                                 h3("A level average point score (APS) per entry and grade by institution type"),
                                # textOutput("textApsAll"),
                                 htmlOutput("textApsAll"),
                                 
                                # uiOutput("boxapsAlevel", width = 2),
                                
                                
                                column(
                                  width=12,
                                  
                                  box(
                                  width=12,
                                  plotOutput("plotAlevelAps") %>% spinner()
                                  )
                                 ),
                               dataTableOutput("tabApsAll")
                               
                                 )
                               )
                                  #
                             ),
                           
                           tabPanel(
                             "A level APS gender gap", value="ggap",
                             fluidRow(
                               column(
                                 width=12,
                                 h3("A level female - male average points difference by institution type from 2015/16 to 2022/23 in England"),
                                 textOutput("textGgap"),
                                 
                                 #htmlOutput("boxapsAlevel", width = 2),
                                 box(
                                   width=12,
                                   plotlyOutput("plotGgap") %>% spinner()
                                 )
                               )
                               
                               )
                             ),
                           
                           
                           tabPanel(
                             "A level APS by gender", value="alevel_fm",
                             fluidRow(
                               column(
                                 width=12,
                                 h3("A level average point score and grade for female and male by Institution type"), 
                                 column(width=12,
                                        textOutput("textApsFm")),
                                 
                                # p("This is the standard paragraph style for adding guiding info around data content."),
                                 column(
                                   width=6,
                                   box(
                                     width=12, p(""),
                                     plotOutput("plotFemaleAlAPS") %>% spinner()
                                     )
                                   ),
                                 column(
                                   width=6,
                                   box(
                                     width=12,
                                     plotOutput("plotMaleAlAPS") %>% spinner()
                                     )
                                   ),
                                column(
                                  width=12,
                                  # div(
                                  #   class = "well",
                                  #   style = "min-height: 100%; height: 100%; overflow-y: visible",
                                  fluidRow(
                                    column(width=12
                                    )
                                  )
                                  ),
                                dataTableOutput("tabFm")
                                
                                
              
          
        ))
              
        
      )))
  )
        # add box to show user input
))
          
  
}



#############################################






dashboard_panel_sub_all <- function() {
  tabPanel(
    value = "dashboard_alse",
    "A level subject entries and grade: comparison by subject",
    
    # Define UI for application that draws a histogram
    
    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width=12,
          h2("A level single entry and grade distribution:  Comparison by subject, England from 1995/96 to 2022/23")
        ),
        column(
          width=12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(
              
              
              column(
                width = 3,
                selectInput(inputId = "subByAll",
                            label = "Select students",
                            choices =  unique(subjectByAll$characteristic_gender)%>%
                              sort(), selected = "All students"
                )
              ),
              
              column(
                width = 5,
                
                selectizeInput(inputId = "subCompareAll", label = "Select up to 4 subjects to compare:",
                               choices = sort(unique(subjectByAll$subject_name)),
                               multiple = T,
                               options = list(maxItems = 4), selected=c("Total Maths","Total English")
                )),
              
              
              column(
                width = 2, 
                selectInput(inputId = 'year_start',
                            label = "Select start year",
                            choices = seq(1996, 2018, 1), 
                            selected = 1996
                ) 
              ), 
              
              column(
                width = 2, 
                selectInput(inputId = 'year_end',
                            label = " End year",
                            choices = latest_year),
                         
               
              br()
            )),
          br(),
          
          fluidRow(
              
              column(
                width = 4,
                p(strong("Download full dataset")),
                downloadButton(
                  outputId = "downloadDataSubject",
                  label= "Download data",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                  
                )
              ),
              
              column(
                width=4,
                p(strong("For more tables and metadata")),
                actionButton(inputId='ees_2a', 
                             label="Visit Explore Education Statistics", 
                             icon = icon("table-cells"), 
                             onclick ="window.open('https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results/2021-22', '_blank')",
                             style = "width:100%;white-space:normal;"
                             
                             
                )
              ),
              
              column(
                width=4, 
                p(strong ("Reset chart selection")), 
                align = "right",
                actionButton(inputId = "resetEntries", label = 'Reset selections', icon = icon("fas fa-arrows-rotate")
                ))
            )
            
          )
        ),
        
        column(
          width=12,
          tabsetPanel(id = "tabsetpanels2a",
                      tabPanel(value= "alevelAllSubject",
                        "Subject entries by all",
                        fluidRow(
                          column(
                            width=12,
                            h3("A level single academic year:  Comparison by subject"),
                            textOutput("textSubAll"),  
                            
                            br(),
                            
                            column(
                              width = 6,
                              paste("Download chart data:"), br(),
                              downloadButton(
                                outputId = "downloadSubAll",
                                label= "Download data",
                                icon = shiny::icon("download"),
                                class = "downloadButton"
                              )
                            
                              )),
                            
                          
                            box(
                              width=12, 
                              plotlyOutput("plotAlevelSubject")%>% spinner(),
                              
                              p("Notes: 
                                  - Total English covers English literature, English language, English language and literature. 
                                  - Total Maths covers Maths, Pure maths, statistics, Use of maths and Other maths. 
                                  - Total Modern languages cover French, German, Spanish and Other foreign modern languages. 
                                  - Total Classical studies covers Latin, Greek, Classical civilisation and Other classical studies. 
                                  - STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science. 
                                  - Total Music includes Music and Music technology.")
                             
                              )
                            )
                        
                          ),
                        
                       
                      tabPanel(
                        "Cumulative results by all",
                        fluidRow(
                          
                          column(
                            width=9,
                            h3("A level cumulative percentage grade"),
                            textOutput("textSubResultAll")
                          ),
                          column(width=3,
                                     div(width=3,
                                       class = "well",
                                       style = "min-height: 100%; height: 100%; overflow-y: visible",
                                       
                                       
                                           selectInput(inputId="resByAll",
                                          "Select cumulative result",
                                          choices=list("A*-A" = "`A*-A`",
                                                       "A*-B" = "`A*-B`",
                                                       "A*-C" = "`A*-C`",
                                                       "A*-D" = "`A*-D`",
                                                       "A*-E" = "`A*-E`"))))),
                      br(),
                        fluidRow(
                        column(
                              width=12,
                              column(
                                width = 6,
                                paste("Download chart data & results:"), br(),
                                downloadButton(
                                  outputId = "downloadResAll",
                                  label= "Download data",
                                  icon = shiny::icon("download"),
                                  class = "downloadButton"
                                  
                                ) )),
                              
                              box(
                                width=12,
                                plotlyOutput("plotResultAll")%>% spinner(),
                               
                                p("Notes: A* grade was first awarded in 2010. - Total English covers English literature, English language, English language and literature. 
                                  - Total Maths covers Maths, Pure maths, statistics, Use of maths and Other maths. 
                                  - Total Modern languages cover French, German, Spanish and Other foreign modern languages. 
                                  - Total Classical studies covers Latin, Greek, Classical civilisation and Other classical studies. 
                                  - STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science. 
                                  - Total Music includes Music and Music technology.")
                              )
                            )
                           
                          #  dataTableOutput("tabSubAll")
                            
                          )
                        
                        
                    ))
      )
      
    ))
  
  
}









dashboard_panel_sub_fm <- function() {
  tabPanel(
    value = "dashboard_fm",
    "A level entry and grade: comparison by gender",
    
    # Define UI for application that draws a histogram
    
    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width=12,
          h2("A level single entry and grade distribution: comparison by gender, England from 1995/96 to 2022/23")
        ),
        column(
          width=12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(
              
              
              column(
                width = 6,
                
                selectizeInput(inputId = "subjectFm", label = "Select a subject:",
                               choices = sort(unique(subjectByGender$subject_name)),
                               multiple = F,
                               selected=c("Total English")
                )
              ),
              
              
              column(
                width = 3, 
                selectInput(inputId = 'year_start_fm',
                            label = "Select start year",
                            choices = seq(1996, latest_year, 1), 
                            selected = 1996
                ) 
              ), 
              
              
              column(
                width = 3, 
                selectInput(inputId = 'year_end_fm',
                            label = " End year",
                            choices = latest_year, 
                            #choices = seq(1996, latest_year, 1),  
                            selected = latest_year
                )
              ),
              
              
              
              
              
              column(
                width = 4,
                p(strong("Download full dataset")),
                downloadButton(
                  outputId = "downloadDataSubjectFm",
                  label= "Download data",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                  
                )
              ),
              
              column(
                width=4,
                p(strong("For more tables and metadata")),
                actionButton(inputId='ees_2b', 
                             label="Visit Explore Education Statistics", 
                             icon = icon("table-cells"), 
                             onclick ="window.open('https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results/2021-22', '_blank')",
                             style = "width:100%;white-space:normal;"
                             
                             
                )
              ),
              
              column(
                width=4, 
                p(strong("Reset chart selection")), 
                align = "right",
                actionButton(inputId = "resetSubFm", label = 'Reset selections', icon = icon("fas fa-arrows-rotate")
                ))
            )
            
          )
        ),
        
        column(
          width=12,
          tabsetPanel(id = "tabsetpanels2b",
                      
                      
                      
                      tabPanel(value= "alevelFmSubject",
                               "Subject entries by gender",
                               fluidRow(
                                 column(
                                   width=12,
                                   h3("A level subject: Comparison by gender"),
                                   textOutput("textSubFm"), br(),
                                   column(
                                     width = 6,
                                     paste("Download chart data:"), br(),
                                     downloadButton(
                                       outputId = "downloadSubFm",
                                       label= "Download data",
                                       icon = shiny::icon("download"),
                                       class = "downloadButton"
                                     ))),
                                   #valueBoxOutput("", width = 3),
                                   # valueBoxOutput("boxapsGrade", width = 6),
                                   box(
                                     width=12,
                                     plotlyOutput("plotSubjectFm")%>% spinner(), 
                                     p("Notes: 
                                  - Total English covers English literature, English language, English language and literature. 
                                  - Total Maths covers Maths, Pure maths, statistics, Use of maths and Other maths. 
                                  - Total Modern languages cover French, German, Spanish and Other foreign modern languages. 
                                  - Total Classical studies covers Latin, Greek, Classical civilisation and Other classical studies. 
                                  - STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science. 
                                  - Total Music includes Music and Music technology.")
                                   )
                                 )
                                 
                               ),
                               
                               
                      tabPanel(
                        "Cumulative results by gender",
                        fluidRow(
                          
                          column(
                            width=9,
                            h3("A level cumulative percentage grade by subject and gender"),
                            textOutput("textResFm")
                          ), 
                          column(width=3,
                                 div(width=3,
                                     class = "well",
                                     style = "min-height: 100%; height: 100%; overflow-y: visible",
                                     
                                     
                                     selectInput(inputId="resByFm",
                                                 "Select cumulative result",
                                                 choices=list("A*-A" = "`A*-A`",
                                                              "A*-B" = "`A*-B`",
                                                              "A*-C" = "`A*-C`",
                                                              "A*-D" = "`A*-D`",
                                                              "A*-E" = "`A*-E`"))))), br(),
                        fluidRow(
                          column(
                            width=12,
                            column(
                              width = 6,
                              paste("Download chart data & results:"), br(),
                              downloadButton(
                                outputId = "downloadResFm",
                                label= "Download data",
                                icon = shiny::icon("download"),
                                class = "downloadButton"
                                
                              ))),
                            box(
                              width=12,
                              plotlyOutput("plotResultFm") %>% spinner(),
                              p("Notes: A* grade was first awarded  in 2010. - Total English covers English literature, English language,  English language and literature. 
                                - Total Maths covers Maths, Pure maths, statistics, Use of maths and Other maths. 
                                - Total Modern languages cover French, German, Spanish and Other foreign modern languages. 
                                - Total Classical studies covers Latin, Greek, Classical civilisation and Other classical studies. 
                                - STEM subjects cover Biology, Chemistry, Physics, Total maths, Further mathematics and Computer science. 
                                - Total Music includes Music and Music technology.")
                            )
                          )
                          
                          #  dataTableOutput("tabSubAll")
                          
                        )
                        
                        
                      
          ))
      )
      
    ))
  
  
}




