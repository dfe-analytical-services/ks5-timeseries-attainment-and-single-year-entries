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

  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  
  
  
  
  # Reactive for headline attainment 
  
  reactiveHeadline<-reactive({
    headlineData<-dfAttainment
    headlineData<-subset(headlineData,
                         school_type==input$headlineAps)
  })
  
  
  output$plotHeadline <- renderPlot({
    createTimeSeriesHeadline(reactiveHeadline(), 
                        allAps=input$headlineAps
                        
    )
  })
  

  
  
  #  Disable top panel on institution type and group for headline
  #  Disable top panel on type of students for Female and Male
  
  observe({
    validate(need(!is.null(input$tabsetpanels), ""))
    if (input$tabsetpanels=="headline") {
      disable("alevelInstitute")
      disable("instituteGroup")
    }else{
      enable("alevelInstitute")
      enable("instituteGroup")
      
    }
    
  })
  
  observe({
    validate(need(!is.null(input$tabsetpanels), ""))
    if (input$tabsetpanels=="alevel_fm" || input$tabsetpanels=="headline" || input$tabsetpanels=="ggap") {
      disable("allGender")
    }else{
      enable("allGender")
      
    }
    
  })
  
  
  # Add value box for A level 
  output$headBox1<-renderUI({
    latest<-(reactiveHeadline() %>%
               filter(year==max(year), cert_type=="Alevel", school_type == input$headlineAps))$aps
    
    grade<-(reactiveHeadline() %>%
              filter(year==max(year), cert_type=="Alevel", school_type == input$headlineAps))$aps_grade
    req(grade)
    lapply(seq_along(latest), function(i) {
      fluidRow(
        valueBox(value=grade[i], subtitle=HTML(paste0('A level average result equivalent to ',  strong(latest[i],'points'),':', br(), input$headlineAps[i])), width=12,
                 color = "blue")
      )
    })
  })
  
  # Add value box for Applied general
  output$headBox2<-renderUI({
    latest<-(reactiveHeadline() %>%
               filter(year==max(year), cert_type=="Applied general", school_type == input$headlineAps))$aps
    grade<-(reactiveHeadline() %>%
              filter(year==max(year), cert_type=="Applied general", school_type == input$headlineAps))$aps_grade
    req(grade)
    lapply(seq_along(latest), function(i) {
      fluidRow(
        valueBox(value=grade[i], subtitle=HTML(paste0('Applied general average result equivalent to ',  strong(latest[i],'points'),': ', input$headlineAps[i])), width=12,
                 color = "blue")
      )
    })
  })
  
  
  #  Add Value box for Tech level
  
  output$headBox3<-renderUI({
    latest<-(reactiveHeadline() %>%
               filter(year==max(year), cert_type=="Tech level", school_type == input$headlineAps))$aps
    
    grade<-(reactiveHeadline() %>%
              filter(year==max(year), cert_type=="Tech level", school_type == input$headlineAps))$aps_grade
    req(grade)
    lapply(seq_along(grade), function(i) {
      fluidRow(
        valueBox(value=grade[i], subtitle=HTML(paste0('Tech level average result equivalent to ', strong(latest[i],'points'),':', br(), input$headlineAps[i])), width=12,
                 color = "blue")
      )
    })
  })
  
  # Create reactive for Headline data
  
  selectedHeadlineData<-reactive({
    headlineData<-dfAttainment
    headlineData<-subset(headlineData,
                         school_type==input$headlineAps) %>%
      select(
        Year                 = year,
        `Academic year`      = time_period,
        `Institution type`   = school_type,
        `Number of students` = number_of_students,
        `APS per entry`      = aps,
        `APS per entry grade`= aps_grade,
        Qualification        = cert_type,
        Version              = version)
  })
  
  
  
  # Data table for headline page
  output$tabHeadline <- renderDataTable({
    datatable(selectedHeadlineData(),
              extens ='Buttons',
              options=( 
                list(infor=F, paging=F,
                     searching=F,
                     stripClasses=F,
                     lengthChange=F,
                     scrollY='260px',
                     scrollX=T,
                     scrollCollapse=T,
                     rownames=F,
                     dom='Bfrtip',
                     buttons=(c('copy', 'csv', 'excel')),
                     class="display")
              )
    )
    
    
  })
  
  
  
  #  Reactive for A level APS ----------------------------------------------- 
  
  reactiveType<-reactive({
    inType<-dfAlevelAps 
      
    inType<-subset(
      inType,
      school_type %in% input$alevelInstitute &
        school_type_group %in% input$instituteGroup &
        characteristic_gender==input$allGender)
    
    
    inType
    
  })
  
  
  # output$alevelInstitute<-renderUI({
  #   inType<-dfAlevelAps
  #   
  #   inType<-subset(
  #     inType,
  #   
  #     
  #     school_type_group %in% input$instituteGroup  &
  #        characteristic_gender==input$allGender
  #      )
  #  
  #   # selectizeInput(inputId="alevelInstitute",
  #   #                label="Select up to 4 institution types",choices=unique(inType$school_type), multiple=T, options = list(maxItems = 4), 
  #   #                selected=c("All independent schools", "All schools and FE sector colleges"))
  #   # 
  # })  
  # 
  
  
  # observeEvent(input$instituteGroup, {
  #   freezeReactiveValue(input, "alevelInstitute")
  #   updateSelectizeInput(inputId =  "alevelInstitute", choices=(filter(dfAlevelAps, school_type %in% input$alevelInstitute, characteristic_gender==input$allGender)))
  # 
  # })

observeEvent(input$instituteGroup, {
# updateSelectizeInput(input, "alevelInstitute")
  updateSelectizeInput(session,"alevelInstitute", "Select up to 4 institution types", 
                       choices=(dfAlevelAps$school_type[dfAlevelAps$school_type_group %in% input$instituteGroup]), 
                       selected=c("All independent schools","All state-funded schools"))

})
  
  
  
  observeEvent(input$resetApsAll, {
    updateSelectizeInput(session, "instituteGroup", selected = c("All independent schools","All state-funded schools"))
    updateSelectizeInput(session, "alevelInstitute", selected=c("All independent schools","All state-funded schools"))
    updateSelectizeInput(session, "allGender", selected = "All students")
  })
  
  output$plotAlevelAps <- renderPlot({
    createApsTimeSeries(reactiveType(),#%>%
                          # mutate(
                          #   school_type=fct_reorder2(school_type, year_2013_2015, aps_2013_2015),
                          #   school_type=fct_reorder2(school_type, year_2016_2022, aps_2016_2022)
                                 
                        allGender=input$allGender, 
                            instType=input$alevelInstitute,
                        instGroup= input$instituteGroup
                        )
    })

    
  
  # Alevel Aps for female and male
  
  reactiveFmAps<-reactive({
    inType<-dfAlevelAps 
    
    inType<-subset(
      inType,
      school_type %in% input$alevelInstitute &
        school_type_group %in% input$instituteGroup)
    
    
    inType
  })
  
  
  output$plotFemaleAlAPS <-renderPlot ({
    createApsFmTimeSeries(reactiveFmAps() %>%
                             filter(characteristic_gender=="Female" & year>2015) ,#%>%
                            #mutate(school_type=fct_reorder2(school_type, year_2016_2022, aps_2016_2022)),
                          fmGender="Female",
                          instGroup= input$instituteGroup,
                          instType = input$alevelInstitute)
  })


  output$plotMaleAlAPS <-renderPlot ({
   createApsFmTimeSeries(reactiveFmAps() %>%
                              filter(characteristic_gender=="Male" & year>2015), #%>%
                           # mutate(school_type=fct_reorder2(school_type, year_2016_2022, aps_2016_2022)), 
                        fmGender="Male",
                         instGroup= input$instituteGroup,
                         instType = input$alevelInstitute)
  })
  
  
  
  
  # Reactive for difference between female and male  gender gap APS  A level
  # This is based on female male average result
  
  reactiveGgap<-reactive({
    gGap<-fmDiff 
    gGap<-subset(
      gGap,
      school_type %in% input$alevelInstitute &
        school_type_group %in% input$instituteGroup)
    gGap
  })
  
  
  output$plotGgap <- renderPlotly({
    createGenderGap(reactiveGgap(), 
                    # %>%
                    #   mutate(school_type=fct_reorder2(school_type, year, gender_gap)),  
                    instGroup= input$instituteGroup,
                    instType=input$alevelInstitute
    )
  })
  
  selectedGgapData<-reactive({
    gGap<-dfAlevelAps %>%
      filter(characteristic_gender != "All students" & year >=2016) 
    gGap<-subset(gGap, school_type %in% input$alevelInstitute &
                   school_type_group %in% input$instituteGroup)%>%
      select(
        Year                 = year,
        `Academic year`      = time_period,
        `Institution type`   = school_type,
        `Characteristic gender` = characteristic_gender,
        `Number of students` = number_of_students,
        `APS per entry`      = aps_2016_2022,
        `APS per entry grade`= aps_grade_2016_2022,
        Version              = version)
  })
  
  
  
  # Data table for headline page
  output$tabGap<- renderDataTable({
    datatable(selectedGgapData(),
              extens ='Buttons',
              options=( 
                list(infor=F, paging=F,
                     searching=F,
                     stripClasses=F,
                     lengthChange=F,
                     scrollY='260px',
                     scrollX=T,
                     scrollCollapse=T,
                     rownames=F,
                     dom='Bfrtip',
                     buttons=(c('copy', 'csv', 'excel')),
                     class="display")
              )
    )
    
    
  })
  
  
  
  
  # Create textOutput for APS Alevel
  
  output$textHeadline<-renderText({
    val<-paste(input$headlineAps, collapse=",")
    # val1<-paste(input$allGender, collapse=", ")
    paste("The boxes display the latest average results in 2021/22 for A level, applied general and tech level.
Bar chart shows the average results from 2015/16 to 2021/22 for ", val,  " in England. To view results, click on the drop-down box beside the bar chart and select one institution type. 
" )
  })
  
  output$textApsAll<-renderText({
    val<-glue::glue_collapse(input$alevelInstitute, ", ", last=" and ")
    val1<-paste(input$allGender, collapse=", ")
    paste("The line charts display the average points and grades achieved by students throughout 16 to 18 study for ", val," in England.  Point scale changed to a simpler scale in 2015/16, but grades remain unchanged. 
          The shaded area shows the Centre Assessment Grade (CAG) and Teacher Assessed Grade (TAG) 
          awarded in 2019/20 and 2020/21 respectively. To view charts, select student type,  
          select up to 4 institution groups and institution types from the drop-down menus at the top of the page.
          Care should be taken when comparing across institution types due to significant
                  differences in cohort sizes.")
  })
  
  output$textGgap<-renderText({
    val<-glue::glue_collapse(input$alevelInstitute, ", ", last=" and ")
   paste("The line chart shows the female - male average points difference (gender gap) from 2015/16 to 2021/22 in England  for ", val, ". To view chart, 
          select up to 4 institution groups and institution types from the drop-down menus at the top of the page.")
  })
  
  
  output$textApsFm<-renderText({
    val<-glue::glue_collapse(input$alevelInstitute, ", ", last=" and ") 
    val1<-paste(input$allGender, collapse=", ")
    paste("The line charts display the average points and grades achieved  by female and male students for ", val, " in England. 
          The shaded area shows the Centre Assessment Grade (CAG) and Teacher Assessed Grade (TAG) 
          awarded in 2019/20 and 2020/21 respectively.  To view charts, 
          select up to 4 institution groups and institution types from the drop-down menus at the top of the page.")
  })
  
  
#  A level single entry subject  
  
# create reactive for subject entries 
# create observe event and output for plot 
   

 
    
  reactiveSubject<-reactive({
    subjName<-subjectByAll
     
    subjName<-subset(
      subjName, 
      subject_name  %in% input$subCompareAll &
        characteristic_gender == input$subByAll &
       # grade == input$resByAll &
        between(year, as.numeric(input$year_start), as.numeric(input$year_end)))
    
    subjName
  })
  
  
  
  
  observeEvent(input$resetEntries, {
    updateSelectInput(session, "year_start", selected = 1996)
    updateSelectInput(session, "year_end", selected = latest_year)
    updateSelectizeInput(session, "subCompareAll", selected=c("Total Maths","Total English"))
    updateSelectInput(session, "subByAll", selected = "All students")
    updateSelectInput(session, "resByAll", selected= "A*-A")
  })
  
  # observeEvent(input$year_start, {updateSelectInput(session, "year_end", label = NULL,
  #                                                   choices = seq(ifelse(input$year_start == latest_year, latest_year, as.numeric(input$year_start)), latest_year, 1),
  #                                                   selected = input$year_end) })
  observeEvent(input$year_end, {updateSelectInput(session, "year_start", label = NULL, 
                                                  choices = seq(1996, ifelse(input$year_end == 1996, 1996, as.integer(2018)), 1),
                                                  selected = input$year_start) })
  
  
  output$plotAlevelSubject <- renderPlotly({
    createTimeSeriesSubject(reactiveSubject(),# %>%
                            #mutate(subject_name=fct_reorder2(subject_name, year, entry_count)), 
                            subName=input$subCompareAll,
                            subAll=input$subByAll
                        
    )
  })
  
  output$plotResultAll<-renderPlotly({
    createTimeSeriesResult(reactiveSubject(),
                         
                           subName=input$subCompareAll,
                           subAll=input$subByAll,
                           resAll=input$resByAll
                          
                           
    )
  })
 
 
  ## Reactive and output for A level subject by gender ###########
  
  
  
  
  
  reactiveSubjectFm<-reactive({
    subjNameFm<-subjectByGender
    
    subjNameFm<-subset(
      subjNameFm,
      subject_name  == input$subjectFm &
        
        between(year, as.numeric(input$year_start_fm), as.numeric(input$year_end_fm)))
    
    subjNameFm
  })
  
  output$plotSubjectFm<-renderPlotly({
    createTimeSeriesSubjectFm(reactiveSubjectFm(),
                           subByFm=input$subjectFm
    )
  })
  
  output$plotResultFm<-renderPlotly({
    createTimeSeriesResultFm(reactiveSubjectFm(),
                          
                             resByFm=input$resByFm,
                             subByFm=input$subjectFm
                             
    )
  })
  
  # observeEvent(input$year_start_fm, {updateSelectInput(session, "year_end_fm", label = NULL,
  #                                                   choices = seq(ifelse(input$year_start_fm == latest_year, latest_year, as.numeric(input$year_start_fm)), latest_year, 1),
  #                                                   selected = input$year_end_fm) })
  observeEvent(input$year_end_fm, {updateSelectInput(session, "year_start_fm", label = NULL, 
                                                  choices = seq(1996, ifelse(input$year_end_fm == 1996, 1996, as.integer(2018)), 1),
                                                  selected = input$year_start_fm) })
  
  observeEvent(input$resetSubFm, {
    updateSelectInput(session, "subjectFm", selected = "Total English")
    updateSelectInput(session, "year_start_fm", selected = 1996)
    updateSelectInput(session, "year_end_fm", selected = latest_year)
    updateSelectInput(session, "resByFm", selected= "A*-A")
    
    
  })
  
  
  # Text output for subject entries and results
  
  
  output$textSubAll<-renderText({
    val<-glue::glue_collapse(input$subCompareAll, ", ", last=" and ")
    val1<-paste(input$year_start, collapse=", ")
    val2<-paste(input$year_end, collapse=", ")
    val3<-paste(input$subByAll, collapse=",")
   
    paste("The line chart shows the A level exam entries for ", val, "from  ", val1, "to ", val2, "in England for ", val3, ".  To compare trend and changes over time, select student type, select up to 4 subjects and start year from the drop-down menus." )
  })
  
  
  output$textSubResultAll<-renderText({
    val<-glue::glue_collapse(input$subCompareAll, ", ", last=" and ")
    val1<-paste(input$year_start, collapse=", ")
    val2<-paste(input$year_end, collapse=", ")
    val3<-paste(input$subByAll, collapse=",")
    paste("The line chart shows the A level cumulative percentage grades for", val, "\n from " , val1, "to ", val2, " for ", 
          val3,  " in England.   The shaded area on chart shows the Centre Assessment Grade (CAG) and Teacher Assessed Grade (TAG) 
          awarded in 2019/20 and 2020/21 respectively. To compare trend and changes over time, select up to 4 subjects, select start year  and a cumulative grade from the drop-down menus.")
  })
  
  

  
  

  output$textSubFm<-renderText({
    val<-paste(input$subjectFm, collapse=",")
    val1<-paste(input$year_start_fm, collapse=", ")
    val2<-paste(input$year_end_fm, collapse=", ")
   
    paste("The line chart shows the A level  exam entries for female and male on ", val, " from " , val1, "to ", val2,  
          " in England. 
          To view changes over time, select required subject and start year from the drop-down menus at the top of the page. " )
  })
  
  
  output$textResFm<-renderText({
    val<-paste(input$subjectFm, collapse=", ")
    val1<-paste(input$year_start_fm, collapse=", ")
    val2<-paste(input$year_end_fm, collapse=", ")
  
    paste("The line chart shows the A level cumulative percentage grades for female and male on ", val, "\n from " , val1, " to ", val2, 
          " in England.  The shaded area on chart shows the Centre Assessment Grade (CAG) and Teacher Assessed Grade (TAG) 
          awarded in 2019/20 and 2020/21 respectively.
          To view changes over time, select one subject, start year and a cumulative grade from the drop-down menus.")
  })
  

  
  # Download selected subjects
  
  selectedSubAll<-reactive({
    subjName<-subjectByAll %>%
      select(year, time_period, subject_name,
             characteristic_gender, entry_count, version)
    subjName<-subset(
      subjName,
      subject_name  %in% input$subCompareAll &
        characteristic_gender == input$subByAll &
        between(year, as.numeric(input$year_start), as.numeric(input$year_end)))
    
    subjName
  })
  
  output$downloadSubAll<- downloadHandler(
    filename = "aleve_subject_data.csv",
    content = function(file) {
      data<-selectedSubAll()
      write.csv(data, file, row.names=FALSE)
    }
  )
 
 
  
  # Download selected subjects and all cumulative grades
  
  selectedResAll<-reactive({
    subjName<-subjectByAll %>%
      select(year, time_period, subject_name,
             characteristic_gender, entry_count, `A*-A`, `A*-B`,
             `A*-C`, `A*-D`, `A*-E`, version)
    subjName<-subset(
      subjName,
      subject_name  %in% input$subCompareAll &
        characteristic_gender == input$subByAll &
        between(year, as.numeric(input$year_start), as.numeric(input$year_end)))
    
    subjName
  })
  
  
  output$downloadResAll<- downloadHandler(
    filename = "aleve_subject_result_data.csv",
    content = function(file) {
      data<-selectedResAll()
      write.csv(data, file, row.names=FALSE)
    }
  ) 
  
  
  output$downloadSubjectAll <- downloadHandler(
    filename = "alevel_subject_all_data.csv",
    content = function(file) {
      data<-dfAlevelSubject
      write.csv(data, file, row.names=FALSE)
    }
  )
  
  
  
  
  
  # Download  subject entry and cumulative result for all subject from top panel 
  
  output$downloadDataSubject <- downloadHandler(
    filename = "alevel_subject_all_data.csv",
    content = function(file) {
      data<-dfAlevelSubjectRaw
      write.csv(data, file, row.names=FALSE)
    }
  )
  
  output$downloadDataSubjectFm <- downloadHandler(
    filename = "alevel_subject_all_data.csv",
    content = function(file) {
      data<-dfAlevelSubjectRaw
      write.csv(data, file, row.names=FALSE)
    }
  )
  
  
  
 # Download  selected subject entries by gender
  selectedSubFm<-reactive({
    subjName<-subjectByGender %>%
      select(year, time_period, subject_name,
             characteristic_gender, entry_count, version)
    subjName<-subset(
      subjName,
      subject_name  == input$subjectFm &
        
        between(year, as.numeric(input$year_start_fm), as.numeric(input$year_end_fm)))
    
    subjName
  })
  
  output$downloadSubFm <- downloadHandler(
    filename = "alevel_subject_gender_data.csv",
    content = function(file) {
      data<-selectedSubFm()
      write.csv(data, file, row.names=FALSE)
    }
  )
  
  
  # Download selected subject entry and all cumulative result for gender
  
  selectedResFm<-reactive({
    subjName<-subjectByGender %>%
      select(year, time_period, subject_name,
             characteristic_gender, entry_count, `A*-A`, `A*-B`,
             `A*-C`, `A*-D`, `A*-E`, version)
    
    subjName<-subset(
      subjName,
      subject_name == input$subjectFm &
       
        between(year, as.numeric(input$year_start_fm), as.numeric(input$year_end_fm)))
    
    subjName
  })
  
 
  output$downloadResFm <- downloadHandler(
    filename = "alevel_subject_and_allresults.csv",
    content = function(file) {
      data<-selectedResFm()
      write.csv(data, file, row.names=FALSE)
      
    }
  )
  

  
  
  # Data table for headline page
  output$tabApsAll <- renderDataTable({
    datatable(reactiveType()%>%
                select(
                  Year                 = year,
                  `Academic year`      = time_period,
                 # `Institution group`  = school_type_group,
                  `Institution type`   = school_type,
                  `Characteristic gender` = characteristic_gender,
                  `Number of students` = number_of_students,
                  `APS per entry 2016-2022`      = aps_2016_2022,
                  `APS per entry grade 2016-2022`= aps_grade_2016_2022,
                  `APS per entry 2013-2015`      = aps_2013_2015,
                  `APS per entry grade 2013-2015`= aps_grade_2013_2015,
                  Version                        = version),
              options=list(infor=F, paging=F,
                           searching=F,
                           stripClasses=F,
                           lengthChange=F,
                           scrollY='260px',
                           scrollX=T,
                           scrollCollapse=T,
                           rownames=F, 
                           dom='Bfrtip',
                           buttons=(c('copy', 'csv', 'excel')),
                           class="display")
              
    )
  })
  
  
  
  
  
  output$resall <- renderDataTable({
    datatable(selected_subAll(),

              options=list(infor=T, paging=F,
                           searching=F,
                           stripClasses=F,
                           lengthChange=F,
                           scrollY='260px',
                           scrollX=T,
                           scrollCollapse=T,
                           rownames=F,
                           autoWidth=TRUE,
                           server = TRUE)
                           
    )
  })

  
 
  
  
  # Navigation with links
  observeEvent(input$link_to_headline_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    
  })
  
  # observeEvent(input$link_to_alevel_all_tab, {
  #   updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  #   updateTabsetPanel(session, "tabsetpanels", selected = "Subject entry and cumulative result by all")
  # })
  # 
  # observeEvent(input$link_to_alevel_fm_tab, {
  #   updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  #   updateTabsetPanel(session, "tabsetpanels", selected = "A level by gender")
  # })
  # 
  observeEvent(input$link_to_alevelFmSubject_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard_fm")

  })


  observeEvent(input$link_to_alevelAllSubject_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard_alse")
  })
  
  
  
  # Download the underlying data button
  output$downloadDataAps <- downloadHandler(
    filename = "attainment_data.csv",
    content = function(file) {
      if(input$tabsetpanels=="headline") {
        write.csv(dfAttainment, file, row.names=FALSE)
        } else {
        write.csv(dfAlevelAps, file, row.names=FALSE)  }
      
    })
  
 

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
