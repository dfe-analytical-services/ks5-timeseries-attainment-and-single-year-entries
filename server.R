# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run th
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {
  # Bookmarking ---------------------------------------------------------------
  # The template uses bookmarking to store input choices in the url. You can
  # exclude specific inputs (for example extra info created for a datatable
  # or plotly chart) using the list below, but it will need updating to match
  # any entries in your own dashboard's bookmarking url that you don't want
  # including.
  setBookmarkExclude(c(
    "cookies",
    "link_to_app_content_tab",
    "tabBenchmark_rows_current",
    "tabBenchmark_rows_all",
    "tabBenchmark_columns_selected",
    "tabBenchmark_cell_clicked",
    "tabBenchmark_cells_selected",
    "tabBenchmark_search",
    "tabBenchmark_rows_selected",
    "tabBenchmark_row_last_clicked",
    "tabBenchmark_state",
    "plotly_relayout-A",
    "plotly_click-A",
    "plotly_hover-A",
    "plotly_afterplot-A",
    ".clientValue-default-plotlyCrosstalkOpts"
  ))

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })


  # Cookies logic -------------------------------------------------------------
  output$cookies_status <- dfeshiny::cookies_banner_server(
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key
  )

  dfeshiny::cookies_panel_server(
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )


  # Top level service navigation actions

  # Navigation links -------------------------------------------------------------


  # Navigation with links
  observeEvent(input$home_page, {
    bslib::nav_select("pages", "dashboard")
    bslib::nav_select("navlistPanel", "homepage")
  })


  observeEvent(input$attainment, {
    bslib::nav_select("pages", "dashboard")
    bslib::nav_select("navlistPanel", "dashboard")
  })


  observeEvent(input$maths_and_science, {
    bslib::nav_select("pages", "dashboard")
    bslib::nav_select("navlistPanel", "dashboard_ms")
  })


  observeEvent(input$subject_entry_and_grade, {
    bslib::nav_select("pages", "dashboard")
    bslib::nav_select("navlistPanel", "dashboard_alse")
  })


  observeEvent(input$subject_entry_and_grade_by_sex, {
    bslib::nav_select("pages", "dashboard")
    bslib::nav_select("navlistPanel", "dashboard_fm")
  })


  observeEvent(input$institution_flow_diagram, {
    bslib::nav_select("pages", "dashboard")
    bslib::nav_select("navlistPanel", "flow_panel")
  })


  #------------------------------------------------------------------------------------------------------

  ## The headline attainment reactive starts here -------------------------------------------------------
  #------------------------------------------------------------------------------------------------------
  # Reactive for headline attainment

  reactiveHeadline <- reactive({
    headlineData <- dfAttainment %>%
      filter(
        establishment_type == input$headlineAps
      )
  })


  output$plotHeadline <- renderPlotly({
    createTimeSeriesHeadline(reactiveHeadline(),
      allAps = input$headlineAps
    )
  })


  #  Disable top panel on institution type and group for headline
  #  Disable top panel on type of students for Female and Male


  # Add value box for A level

  output$headBox1 <- renderUI({
    latest <- reactiveHeadline() %>%
      filter(
        year == max(year),
        exam_cohort == "A level",
        establishment_type == input$headlineAps
      ) %>%
      pull(aps)

    grade <- reactiveHeadline() %>%
      filter(
        year == max(year),
        exam_cohort == "A level",
        establishment_type == input$headlineAps
      ) %>%
      pull(aps_grade)

    bslib::value_box(
      title = paste(
        "Average A level result"
      ),
      value = div(
        style = "text-align: center; white-space: nowrap",
        grade
      ),
      HTML(paste0(
        "equivalent to ",
        latest, " points: <strong>",
        input$headlineAps, "</strong>"
      )),
      theme = "primary"
    )
  })


  # Add value box for Applied general
  output$headBox2 <- renderUI({
    latest <- reactiveHeadline() %>%
      filter(
        year == max(year),
        exam_cohort == "Applied general",
        establishment_type == input$headlineAps
      ) %>%
      pull(aps)

    grade <- reactiveHeadline() %>%
      filter(
        year == max(year),
        exam_cohort == "Applied general",
        establishment_type == input$headlineAps
      ) %>%
      pull(aps_grade)

    bslib::value_box(
      title = paste(
        "Average Applied general  result"
      ),
      value = div(
        style = "text-align: center; white-space: nowrap",
        grade
      ),
      HTML(paste0(
        "equivalent to ",
        latest, " points: <strong>",
        input$headlineAps, "</strong>"
      )),
      theme = "primary"
    )
  })


  # #
  # #   # Value box for Tech level

  output$headBox3 <- renderUI({
    latest <- reactiveHeadline() %>%
      filter(
        year == max(year),
        exam_cohort == "Tech level",
        establishment_type == input$headlineAps
      ) %>%
      pull(aps)

    grade <- reactiveHeadline() %>%
      filter(
        year == max(year),
        exam_cohort == "Tech level",
        establishment_type == input$headlineAps
      ) %>%
      pull(aps_grade)

    bslib::value_box(
      title = paste(
        "Average Tech level result"
      ),
      value = div(
        style = "text-align: center; white-space: nowrap",
        grade
      ),
      HTML(paste0(
        "equivalent to ",
        latest, " points: <strong>",
        input$headlineAps, "</strong>"
      )),
      theme = "primary"
    )
  })


  # Create reactive for Headline data

  selectedHeadlineData <- reactive({
    headlineData <- dfAttainment
    headlineData <- subset(
      headlineData,
      establishment_type == input$headlineAps
    ) %>%
      select(
        Year = year,
        `Academic year` = time_period,
        `Establishment type` = establishment_type,
        `Entry count` = entry_count,
        `APS per entry` = aps,
        `APS per entry grade` = aps_grade,
        `Exam cohort` = exam_cohort,
        Version = version
      )
  })


  # Data table for headline page
  output$tabHeadline <- renderDataTable({
    datatable(selectedHeadlineData(),
      extens = "Buttons",
      options = (
        list(
          infor = F, paging = F,
          searching = F,
          stripClasses = F,
          lengthChange = F,
          scrollY = "260px",
          scrollX = T,
          scrollCollapse = T,
          dom = "Bfrtip",
          buttons = (c("copy", "csv", "excel")),
          class = "display"
      )),
      rownames = FALSE
    )
  })

  # Hide the tab-panels that are not relevant

  observe({
    validate(need(!is.null(input$tabsetpanels), ""))
    if (input$tabsetpanels == "headline") {
      hide("alevelInstitute")
    } else {
      show("alevelInstitute")
    }
  })

  observe({
    validate(need(!is.null(input$tabsetpanels), ""))
    if (input$tabsetpanels == "headline") {
      hide("allSex")
    } else {
      show("allSex")
    }
  })

  observe({
    validate(need(!is.null(input$tabsetpanels), ""))
    if (input$tabsetpanels == "headline") {
      disable("resetApsAll")
    } else {
      enable("resetApsAll")
    }
  })

  observe({
    validate(need(!is.null(input$tabsetpanels), ""))
    if (input$tabsetpanels == "alevel_fm" || input$tabsetpanels == "ggap") {
      disable("allSex")
    } else {
      enable("allSex")
    }
  })


  #  Reactive for A level APS -----------------------------------------------

  reactiveType <- reactive({
    inType <- dfAlevelAps

    inType <- subset(
      inType,
      establishment_type %in% input$alevelInstitute &

        sex == input$allSex
    )


    inType
  })


  observeEvent(input$resetApsAll, {
    updateSelectizeInput(session, "alevelInstitute", selected = c("All FE sector colleges", "All state-funded schools"))
    updateSelectizeInput(session, "allSex", selected = "All students")
  })

  output$plotAlevelAps <- renderPlot({
    createApsTimeSeries(reactiveType(),
      allSex = input$allSex,
      instType = input$alevelInstitute
    )
  })


  # Alevel Aps for female and male

  reactiveFmAps <- reactive({
    inType <- dfAlevelAps
    inType <- subset(
      inType,
      establishment_type %in% input$alevelInstitute
    )
    inType
  })


  output$plotFemaleAlAPS <- renderPlot({
    createApsFmTimeSeries(
      reactiveFmAps() %>%
        filter(sex == "Female" & year > 2015), # %>%
      # mutate(aps_per_entry=as.numeric(aps_per_entry)),
      fmSex = "Female",
      instType = input$alevelInstitute
    )
  })


  output$plotMaleAlAPS <- renderPlot({
    createApsFmTimeSeries(
      reactiveFmAps() %>%
        filter(sex == "Male" & year > 2015), # %>%
      # mutate(aps_per_entry=as.numeric(aps_per_entry)),
      fmSex = "Male",
      instType = input$alevelInstitute
    )
  })


  # Reactive for difference between female and male  sex gap APS  A level
  # This is based on female male average result

  reactiveGgap <- reactive({
    gGap <- fmDiff
    gGap <- subset(
      gGap,
      establishment_type %in% input$alevelInstitute
    )
    gGap
  })


  output$plotGgap <- renderPlotly({
    createSexGap(reactiveGgap(),
      instType = input$alevelInstitute
    )
  })

  selectedGgapData <- reactive({
    gGap <- dfAlevelAps %>%
      filter(sex != "All students" & year >= 2016)
    gGap <- subset(gGap, establishment_type %in% input$alevelInstitute) %>%
      select(
        Year = year,
        `Academic year` = time_period,
        `Establishment type` = establishment_type,
        Sex = sex,
        `Entry count` = entry_count,
        `APS per entry` = aps_per_entry,
        `APS per entry grade` = aps_per_entry_grade,
        Version = version
      )
  })


  # Data table for gap
  output$tabFm <- renderDataTable({
    datatable(selectedGgapData(),
      extens = "Buttons",
      options = (
        list(
          infor = F, paging = F,
          searching = F,
          stripClasses = F,
          lengthChange = F,
          scrollY = "260px",
          scrollX = T,
          scrollCollapse = T,
          dom = "Bfrtip",
          buttons = (c("copy", "csv", "excel")),
          class = "display"
      )),
      rownames = FALSE
    )
  })


  # Create textOutput for APS Alevel


  output$textHeadline <- renderText({
    # val <- glue::glue_collapse(input$headlineAps, ", ", last = " and ")
    val <- paste(input$headlineAps, collapse = " , ")
    HTML(paste("The boxes display the latest average results for A level, Applied general and Tech level for  ", val, " in England. The
    chart shows the APS from 2015/16 to 2025/26. Point scores for 2020 and 2021 are based on Centre assessment grade and Teacher assessed grade respectively.", "
    The concept of qualifications approved for reporting has been applied since 2015/16 following Professor Alison Wolf's Review of vocational education.
    From 2017/18 the quality threshold for vocational and technical qualifications to be included in performance
    measures further increased. The later reforms include criteria relating to the size, content, and assessment, including a requirement
    that a proportion of a qualification's content is subject to external assessment.", "To view results, click on the drop-down box and select one institution type."))
  })

  output$textApsAll <- renderText({
    val <- glue::glue_collapse(input$alevelInstitute, ", ", last = " and ")
    val1 <- paste(input$allSex, collapse = ", ")
    HTML(paste("A level average point score (APS) per entry was first published in 2012/13 with a scale of 0-300.
    The points changed to 0-60 scale in 2015/16, but average grade remains consistent. APS is presented across 2 charts,
    scales truncated so a change of one grade appears the same for both the old and current points scale (i.e. you can read left to right across the two charts).", br(), br(), "
    The charts display the APS and grades achieved by students throughout 16 to 18 study for  ", val, " in England.
    Up to four institution types can be selected from the drop-down menu. Care should be taken when comparing across institution types due to significant differences in cohort sizes.
           For breakdown of institution types, see institution flow diagram on top panel. "))
  })

  output$textGgap <- renderText({
    val <- glue::glue_collapse(input$alevelInstitute, ", ", last = " and ")

    paste("The line chart shows the female - male average points difference (sex gap) from 2015/16 to 2025/26  for ", val, " in England.
          Up to four institution types can be selected from the drop-down menu.  Care should be taken when comparing across institution types due to significant
                  differences in cohort sizes.")
  })


  output$textApsFm <- renderText({
    val <- glue::glue_collapse(input$alevelInstitute, ", ", last = " and ")
    val1 <- paste(input$allSex, collapse = ", ")

    paste("The line charts display the average points and grades achieved  by female and male students for ", val, " in England from 2015/16 to 2025/26.
          Up to four institution types can be selected from the drop-down menu.  Care should be taken when comparing across institution types due to significant
                  differences in cohort sizes.  For breakdown of institution types, see institution flow diagram on top panel.")
  })


  #  A level single entry subject

  # create reactive for subject entries
  # create observe event and output for plot


  reactiveSubject <- reactive({
    subjName <- subjectByAll

    subjName <- subset(
      subjName,
      subject_name %in% input$subCompareAll &
        characteristic_value == input$subByAll &
        # grade == input$resByAll &
        between(year, as.numeric(input$year_start), as.numeric(input$year_end))
    )

    subjName
  })


  observeEvent(input$resetEntries, {
    updateSelectInput(session, "year_start", selected = 1996)
    updateSelectInput(session, "year_end", selected = latest_year)
    updateSelectizeInput(session, "subCompareAll", selected = c("Total Maths", "Total English"))
    updateSelectInput(session, "subByAll", selected = "All students")
    updateSelectInput(session, "resByAll", selected = "A*-A")
  })

  # observeEvent(input$year_start, {updateSelectInput(session, "year_end", label = NULL,
  #                                                   choices = seq(ifelse(input$year_start == latest_year, latest_year, as.numeric(input$year_start)), latest_year, 1),
  #                                                   selected = input$year_end) })
  observeEvent(input$year_end, {
    updateSelectInput(session, "year_start",
      label = NULL,
      choices = seq(1996, ifelse(input$year_end == 1996, 1996, as.integer(2020)), 1),
      selected = input$year_start
    )
  })


  output$plotAlevelSubject <- renderPlotly({
    createTimeSeriesSubject(reactiveSubject(), #
      subName = input$subCompareAll,
      subAll = input$subByAll
    )
  })


  output$plotResultAll <- renderPlotly({
    createTimeSeriesResult(reactiveSubject(),
      subName = input$subCompareAll,
      subAll = input$subByAll,
      resAll = input$resByAll
    )
  })


  ## Reactive and output for A level subject by sex ###########


  reactiveSubjectFm <- reactive({
    subjNameFm <- subjectBySex # dfMs

    subjNameFm <- subset(
      subjNameFm,
      subject_name == input$subjectFm & # subjectMs

        between(year, as.numeric(input$year_start_fm), as.numeric(input$year_end_fm))
    )

    subjNameFm
  })

  output$plotSubjectFm <- renderPlotly({
    createTimeSeriesSubjectFm(reactiveSubjectFm(),
      subByFm = input$subjectFm
    )
  })

  output$plotResultFm <- renderPlotly({
    createTimeSeriesResultFm(reactiveSubjectFm(),
      resByFm = input$resByFm,
      subByFm = input$subjectFm
    )
  })

  # observeEvent(input$year_start_fm, {updateSelectInput(session, "year_end_fm", label = NULL,
  #                                                   choices = seq(ifelse(input$year_start_fm == latest_year, latest_year, as.numeric(input$year_start_fm)), latest_year, 1),
  #                                                   selected = input$year_end_fm) })
  observeEvent(input$year_end_fm, {
    updateSelectInput(session, "year_start_fm",
      label = NULL,
      choices = seq(1996, ifelse(input$year_end_fm == 1996, 1996, as.integer(2020)), 1),
      selected = input$year_start_fm
    )
  })

  observeEvent(input$resetSubFm, {
    updateSelectInput(session, "subjectFm", selected = "Total English")
    updateSelectInput(session, "year_start_fm", selected = 1996)
    updateSelectInput(session, "year_end_fm", selected = latest_year)
    updateSelectInput(session, "resByFm", selected = "A*-A")
  })


  # Text output for subject entries and results


  output$textSubAll <- renderText({
    val <- glue::glue_collapse(input$subCompareAll, ", ", last = " and ")
    val1 <- paste(input$year_start, collapse = ", ")
    val2 <- paste(input$year_end, collapse = ", ")
    val3 <- paste(input$subByAll, collapse = ",")

    paste("The line chart shows the A level exam entries for ", val, "from  ", val1, " to ", val2, " for ", val3, " in England. ")
  })


  output$textSubResultAll <- renderText({
    val <- glue::glue_collapse(input$subCompareAll, ", ", last = " and ")
    val1 <- paste(input$year_start, collapse = ", ")
    val2 <- paste(input$year_end, collapse = ", ")
    val3 <- paste(input$subByAll, collapse = ",")
    paste(
      "The line chart shows the A level cumulative percentage grades for", val, "\n from ", val1, " to ", val2, " for ",
      val3, " in England.   The shaded area on chart shows the Centre Assessment Grade (CAG) and Teacher Assessed Grade (TAG)
          awarded in 2019/20 and 2020/21 respectively."
    )
  })


  output$textSubFm <- renderText({
    val <- paste(input$subjectFm, collapse = ",")
    val1 <- paste(input$year_start_fm, collapse = ", ")
    val2 <- paste(input$year_end_fm, collapse = ", ")

    paste(
      "The line chart shows the A level  exam entries on ", val, " from ", val1, " to ", val2,
      " for female, male and all students in England. "
    )
  })


  output$textResFm <- renderText({
    val <- paste(input$subjectFm, collapse = ", ")
    val1 <- paste(input$year_start_fm, collapse = ", ")
    val2 <- paste(input$year_end_fm, collapse = ", ")

    paste(
      "The line chart shows the A level cumulative percentage grades on ", val, "\n from ", val1, " to ", val2,
      " for female, male and all students in England.  The shaded area on chart shows the Centre Assessment Grade (CAG) and Teacher Assessed Grade (TAG)
          awarded in 2019/20 and 2020/21 respectively."
    )
  })


  # Download selected subjects

  selectedSubAll <- reactive({
    subjName <- subjectByAll %>%
      select(
        year, time_period, subject_name,
        characteristic_value, entry_count, version
      )
    subjName <- subset(
      subjName,
      subject_name %in% input$subCompareAll &
        characteristic_value == input$subByAll &
        between(year, as.numeric(input$year_start), as.numeric(input$year_end))
    )

    subjName
  })

  output$downloadSubAll <- downloadHandler(
    filename = "aleve_subject_data.csv",
    content = function(file) {
      data <- selectedSubAll()
      write.csv(data, file, row.names = FALSE)
    }
  )


  # Download selected subjects and all cumulative grades

  selectedResAll <- reactive({
    subjName <- subjectByAll %>%
      select(
        year, time_period, subject_name,
        characteristic_value, entry_count, `A*-A`, `A*-B`,
        `A*-C`, `A*-D`, `A*-E`, version
      )
    subjName <- subset(
      subjName,
      subject_name %in% input$subCompareAll &
        characteristic_value == input$subByAll &
        between(year, as.numeric(input$year_start), as.numeric(input$year_end))
    )

    subjName
  })


  output$downloadResAll <- downloadHandler(
    filename = "aleve_subject_result_data.csv",
    content = function(file) {
      data <- selectedResAll()
      write.csv(data, file, row.names = FALSE)
    }
  )


  output$downloadSubjectAll <- downloadHandler(
    filename = "alevel_subject_all_data.csv",
    content = function(file) {
      data <- dfAlevelSubject
      write.csv(data, file, row.names = FALSE)
    }
  )


  # Download  subject entry and cumulative result for all subject from top panel

  output$downloadDataSubject <- downloadHandler(
    filename = "alevel_subject_all_data.csv",
    content = function(file) {
      data <- dfAlevelSubjectRaw
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$downloadDataSubjectFm <- downloadHandler(
    filename = "alevel_subject_all_data.csv",
    content = function(file) {
      data <- dfAlevelSubjectRaw
      write.csv(data, file, row.names = FALSE)
    }
  )


  # Download  selected subject entries by value
  selectedSubFm <- reactive({
    subjName <- subjectBySex %>%
      select(
        year, time_period, subject_name,
        characteristic_value, entry_count, version
      )
    subjName <- subset(
      subjName,
      subject_name == input$subjectFm &

        between(year, as.numeric(input$year_start_fm), as.numeric(input$year_end_fm))
    )

    subjName
  })

  output$downloadSubFm <- downloadHandler(
    filename = "alevel_subject_sex_data.csv",
    content = function(file) {
      data <- selectedSubFm()
      write.csv(data, file, row.names = FALSE)
    }
  )


  # Download selected subject entry and all cumulative result for sex

  selectedResFm <- reactive({
    subjName <- subjectBySex %>%
      select(
        year, time_period, subject_name,
        characteristic_value, entry_count, `A*-A`, `A*-B`,
        `A*-C`, `A*-D`, `A*-E`, version
      )

    subjName <- subset(
      subjName,
      subject_name == input$subjectFm &

        between(year, as.numeric(input$year_start_fm), as.numeric(input$year_end_fm))
    )

    subjName
  })


  output$downloadResFm <- downloadHandler(
    filename = "alevel_subject_and_allresults.csv",
    content = function(file) {
      data <- selectedResFm()
      write.csv(data, file, row.names = FALSE)
    }
  )


  # Data table for headline page
  output$tabApsAll <- renderDataTable({
    datatable(
      reactiveType() %>%
        select(
          Year = year,
          `Academic year` = time_period,
          `Establishment type group` = establishment_type_group,
          # `Institution group`  = establishment_type_group,
          `Establishment type` = establishment_type,
          sex = sex,
          `Entry count` = entry_count,
          `APS per entry` = aps_per_entry,
          `APS per entry grade` = aps_per_entry_grade,
          `APS per entry 2013-2015` = aps_2013_2015,
          `APS per entry grade 2013-2015` = aps_grade_2013_2015,
          Version = version
        ),
      extens = "Buttons",
      options = (
        list(
          infor = F, paging = F,
          searching = F,
          stripClasses = F,
          lengthChange = F,
          scrollY = "260px",
          scrollX = T,
          scrollCollapse = T,
          dom = "Bfrtip",
          buttons = (c("copy", "csv", "excel")),
          class = "display"
      )),
      rownames = FALSE
    )
  })


  output$resall <- renderDataTable({
    datatable(selected_subAll(),
      options = list(
        infor = T, paging = F,
        searching = F,
        stripClasses = F,
        lengthChange = F,
        scrollY = "260px",
        scrollX = T,
        scrollCollapse = T,
        rownames = FALSE,
        autoWidth = TRUE,
        server = TRUE
      )
    )
  })


  #######  Maths and science


  reactiveSubjectMs <- reactive({
    subjNameMs <- dfMs

    subjNameMs <- subset(
      subjNameMs,
      subject == input$subjectMs & # subjectMs

        between(year, as.numeric(input$year_start_ms), as.numeric(input$year_end_ms))
    )

    subjNameMs
  })

  output$plotSubjectMs <- renderPlotly({
    createTimeSeriesSubjectMs(reactiveSubjectMs(),
      subByMs = input$subjectMs
    )
  })


  observeEvent(input$year_end_ms, {
    updateSelectInput(session, "year_start_ms",
      label = NULL,
      choices = seq(2010, ifelse(input$year_end_ms == 2010, 2010, as.integer(2020)), 1),
      selected = input$year_start_ms
    )
  })

  observeEvent(input$resetSubMs, {
    updateSelectInput(session, "subjectMs", selected = "Maths")
    updateSelectInput(session, "year_start_ms", selected = 2010)
    updateSelectInput(session, "year_end_ms", selected = latest_year)
  })


  output$textSubMs <- renderText({
    val <- paste(input$subjectMs, collapse = ",")
    val1 <- paste(input$year_start_ms, collapse = ", ")
    val2 <- paste(input$year_end_ms, collapse = ", ")


    paste(
      "The line chart shows the proportion of A level students that entered   ", val, " from ", val1, " to ", val2, "for female, male and all students in England."
    )
  })


  # Download selected subjects


  # Download  subject entry and cumulative result for all subject from top panel


  output$downloadDataSubjectMs <- downloadHandler(
    filename = "alevel_maths_science_data.csv",
    content = function(file) {
      data <- dfMsRaw
      write.csv(data, file, row.names = FALSE)
    }
  )


  # Download  selected subject entries by sex
  selectedSubMs <- reactive({
    subjName <- dfMs %>%
      select(
        year, time_period, subject,
        breakdown, percent_entered, subject_count, version
      )
    subjName <- subset(
      subjName,
      subject_name == input$subjectMs &

        between(year, as.numeric(input$year_start_ms), as.numeric(input$year_end_ms))
    )

    subjName
  })


  selectedMsData <- reactive({
    # msData <- dfMs
    msData <- reactiveSubjectMs()
    msData <- subset(
      msData,
      subject == input$subjectMs
    ) %>%
      select(
        Year = year,
        `Time period` = time_period,
        breakdown = breakdown,
        Subject = subject,
        `Proportion of students` = percent_entered,
        `Subject count` = subject_count,
        Version = version
      )
  })


  output$tabMs <- renderDataTable({
    datatable(selectedMsData(),
      extens = "Buttons",
      options = (
        list(
          infor = F, paging = F,
          searching = F,
          stripClasses = F,
          lengthChange = F,
          scrollY = "260px",
          scrollX = T,
          scrollCollapse = T,
          dom = "Bfrtip",
          buttons = (c("copy", "csv", "excel")),
          class = "display"
      )),
      rownames = FALSE
    )
  })


  # Download the underlying data button
  output$downloadDataAps <- downloadHandler(
    filename = "attainment_data.csv",
    content = function(file) {
      if (input$tabsetpanels == "headline") {
        write.csv(headline_download, file, row.names = FALSE)
      } else {
        write.csv(alevel_attain_download, file, row.names = FALSE)
      }
    }
  )

  # Add input IDs here that are within the relevant drop down boxes to create dynamic text
  output$dropdown_label <- renderText({
    if (input$tabsetpanels == "headline") {
      paste0("Click to download full dataset") # )input$downloadDataAps)
    } else {
      paste0("Click to select further options and up to 4 institution types from the drop-down menu") # input$alevelInstitute, )
    }
  })


  # Navigation links for side bar-------------------------------------------------------------


  # Navigation with links
  # observeEvent(input$link_to_headline_tab, {
  #   updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  # })
  #
  #
  # observeEvent(input$link_to_alevelFmSubject_tab, {
  #   updateTabsetPanel(session, "navlistPanel", selected = "dashboard_fm")
  # })
  #
  #
  # observeEvent(input$link_to_alevelAllSubject_tab, {
  #   updateTabsetPanel(session, "navlistPanel", selected = "dashboard_alse")
  # })
  #
  # observeEvent(input$link_to_alevelMathsScience_tab, {
  #   updateTabsetPanel(session, "navlistPanel", selected = "dashboard_ms")
  # })


  # Footer links --------------------------------------------------------------

  observeEvent(input$dashboard, nav_select("pages", "dashboard"))
  observeEvent(input$support_and_feedback, {
    nav_select("pages", "support_panel")
  })
  observeEvent(input$accessibility_statement, {
    nav_select("pages", "accessibility_statement")
  })
  observeEvent(input$use_of_cookies, {
    nav_select("pages", "cookies_panel_ui")
  })

  ## Back links to main dashboard ---------------------------------------------
  observeEvent(input$footnotes_to_dashboard, nav_select("pages", "dashboard"))
  observeEvent(input$support_to_dashboard, nav_select("pages", "dashboard"))
  observeEvent(input$cookies_to_dashboard, nav_select("pages", "dashboard"))
  observeEvent(
    input$accessibility_to_dashboard,
    nav_select("pages", "dashboard")
  )


  shiny::observeEvent(input$external_link, {
    showModal(modalDialog(
      external_link(
        "https://shiny.posit.co/",
        "External Link",
        add_warning = FALSE
      ),
      easyClose = TRUE,
      footer = NULL
    ))

    # JavaScript to auto-click the link and close the modal
    shinyjs::runjs(
      "
      setTimeout(function() {
        var link = document.querySelector('.modal a');
        if (link) {
          link.click();
          setTimeout(function() {
            $('.modal').modal('hide');
          }, 20); // Extra delay to avoid any race conditions
        }
      }, 400);
    "
    )
  })
}

#   # Stop app ---------------------------------------------------------------------------------
#
#   session$onSessionEnded(function() {
#     stopApp()
#   })
# }
