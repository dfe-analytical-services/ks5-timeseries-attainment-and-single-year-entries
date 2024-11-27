#### contains plot functions


#  plot function for headline attainment for



createTimeSeriesHeadline <- function(dfAps, allAps) {
  fig <- dfAps %>%
    rename(Year = "year", APS = "aps", Qualification = "cert_type")

  fig <- ggplot(
    fig, aes(x = Year, y = APS, group = Qualification, color = Qualification)
  ) +
    geom_line(stat = "identity", linewidth = 1) +
    scale_color_manual(values = c("#12436D", "#28A197", "#801650")) +
    # scale_x_log10(breaks = seq(2016, 2023, 2)) +
    labs(x = "", y = "", color = "") +
    coord_cartesian(ylim = c(0, 60)) +
    scale_x_continuous(breaks = seq(2016, 2024, 1)) +
    theme_classic() +
    theme(
      legend.position = "",
      text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(size = 10),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(linewidth = 1.0)
    ) +
    expand_limits(x = 2016, y = 0) +
    ggtitle(paste0("\n Average point score (All students) :   ", allAps))

  ggplotly(fig, tooltip = c("x", "y", "colour")) %>%
    config(
      modeBarButtonsToRemove = c(
        "zoom2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d",
        "resetScale2d", "hoverCompareCartesian", "drawrect", "select2d", "lasso2d",
        "hoverClosestCartesian", "toggleSpikelines"
      ), displaylogo = FALSE,
      toImageButtonOptions = list(format = "svg", filename = "headlineAttain_image")
    ) %>%
    layout(
      xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE),
      legend = list(orientation = "h", x = .1, y = .2), #-.3),
      hovermode = "x"
    )
}




# Joint plot function for  Alevel Aps from 2013

createApsTimeSeries <- function(dfAps, instGroup, instType, allSex) {
  validate(need(dfAps$establishment_type, message = "To view charts select type of students and up to 4 institution types from the drop-down menus at the top page"))

  fig2 <- dfAps %>%
    filter(year >= 2016)


  fig1 <- ggplot(dfAps, aes(x = year_2013_2015, y = aps_2013_2015, color = establishment_type)) +
    geom_line(stat = "identity", linewidth = 1.5) +
    geom_curve(aes(x = 2015.5, y = 50, xend = 2015, yend = 45),
      arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
      color = "black", size = .05, angle = 45
    ) +
    geom_label(aes(label = aps_grade_2013_2015, y = aps_2013_2015), show.legend = F) +
    # ggtitle("\nAPS & grade\n2012/13 to 2014/15\n") +
    coord_cartesian(ylim = c(150, 300)) +
    scale_y_continuous(limits = c(150, 300)) +
    scale_x_continuous(breaks = seq(2013, 2015, 1)) +
    scale_colour_manual(
      "",
      breaks = unique(dfAps$establishment_type),
      values = gss_colour_pallette
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      text = element_text(size = 14),
      axis.text = element_text(size = 14),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line = element_line(size = 1.0)
    )


  fig2 <- ggplot(fig2, aes(x = year, y = aps_2016_2024, color = establishment_type)) +
    geom_line(stat = "identity", linewidth = 1.5) +
    geom_curve(aes(x = 2016.5, y = 48, xend = 2016, yend = 45),
      curvature = .3,
      arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
      color = "black", size = .05, angle = 45
    ) +
    geom_curve(aes(x = 2019.5, y = 54, xend = 2020, yend = 50),
      curvature = -.3,
      arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
      color = "black", size = .05, angle = 90
    ) +
    geom_curve(aes(x = 2020.5, y = 58, xend = 2021, yend = 52),
      curvature = -.3,
      arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
      color = "black", size = .05, angle = 90
    ) +
    geom_label(aes(label = aps_grade_2016_2024, y = aps_2016_2024), show.legend = F) +
    ggtitle(paste0("\n APS & grade 2015/16 to 2023/24:  ", allSex)) +
    coord_cartesian(ylim = c(10, 60)) +
    scale_y_continuous(limits = c(10, 60)) +
    scale_x_continuous(breaks = seq(2016, 2024, 1)) +
    scale_colour_manual(
      "",
      breaks = unique(dfAps$establishment_type),
      values = gss_colour_pallette
    ) +
    labs(x = "", y = "", color = "") +
    theme_classic() +
    annotate(geom = "text", x = 2016.5, y = 45, label = "Point scale changed in 2015/16 \nbut average grade remains consistent", color = "black", size = 4, vjust = -.3, hjust = 0) +
    annotate("rect", xmin = 2020, xmax = 2021, ymin = 25, ymax = 50, alpha = .2) +
    annotate(geom = "text", x = 2019, y = 56, label = "Centre assessment grade", color = "black", size = 4) +
    annotate(geom = "text", x = 2020, y = 60, label = "Teacher assessed grade", color = "black", size = 4) +
    theme(
      legend.position = c(.7, .15),
      legend.title = element_blank(),
      text = element_text(size = 12),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.line = element_line(linewidth = 1.0)
    )

  finalFig <- (fig1 + fig2 + plot_layout(widths = c(0.7, 2)) &
    ggtitle(paste0("APS and grade \n", allSex)))

  return(finalFig)
}


################################################## Plot function for APS by sex and sex gap ##################################


createApsFmTimeSeries <- function(dfApsFm, instGroup, instType, fmSex) {
  validate(need(dfApsFm$establishment_type, message = "To view chart select between 1 and 4 institution types from the drop-down menu at the top page."))

  fmFig <- ggplot(dfApsFm, aes(
    x = year, y = aps_2016_2024,
    color = establishment_type
  )) +
    geom_line(stat = "identity", linewidth = 1.5) +
    geom_curve(aes(x = 2019.5, y = 53, xend = 2020, yend = 50),
      curvature = -.4,
      arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
      color = "black", size = .05, angle = 90
    ) +
    geom_curve(aes(x = 2020.5, y = 56, xend = 2021, yend = 53),
      curvature = -.4,
      arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
      color = "black", size = .05, angle = 90
    ) +
    geom_label(aes(label = aps_grade_2016_2024, y = aps_2016_2024), show.legend = F) +
    ggtitle(paste0("Average point score and grade:  ", fmSex)) +
    coord_cartesian(ylim = c(10, 60)) +
    scale_x_continuous(breaks = seq(2016, 2024, 1)) +
    scale_colour_manual(
      # "establishment type",
      breaks = unique(dfApsFm$establishment_type),
      values = gss_colour_pallette
    ) +
    labs(x = "", y = "", color = "") +
    theme_classic() +
    annotate("rect", xmin = 2020, xmax = 2021, ymin = 25, ymax = 50, alpha = .2) +
    annotate(geom = "text", x = 2018.5, y = 55, label = "Centre assessment grade", color = "black", size = 4) +
    annotate(geom = "text", x = 2019.5, y = 58, label = "Teacher assessed grade", color = "black", size = 4) +
    theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title = element_blank(),
      text = element_text(size = 12),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.line = element_line(linewidth = 1.0) +
        expand_limits(x = 0, y = 0)
    )
  fmFig
}



createSexGap <- function(dfApsGap, instGroup, instType) {
  validate(need(dfApsGap$establishment_type, message = "To view chart select between 1 and 4 institution types from the drop-down menu at the top page."))

  fig <- dfApsGap %>%
    rename(Year = "year", Sex_gap = "sex_gap", Institution_type = "establishment_type")

  fig <- ggplot(fig, aes(
    x = Year,
    y = Sex_gap,
    color = Institution_type
  )) +
    geom_line(stat = "identity", linewidth = 1) +
    scale_x_continuous(breaks = seq(2016, 2024, 1)) +
    scale_y_continuous(labels = scales::comma) +
    scale_colour_manual(
      # "establishment type",
      # breaks = unique(dfApsGap$establishment_type),
      values = gss_colour_pallette
    ) +
    labs(x = "", y = "", color = "") +
    geom_hline(yintercept = 0, linetype = "dot", col = "navy") +
    ggtitle(paste0("\nFemale-male average points difference (Sex gap)")) +
    theme_classic() +
    theme( # legend.position ="bottom",
      text = element_text(size = 12),
      legend.direction = "vertical",
      plot.title = element_text(size = 10),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.line = element_line(linewidth = 1.0)
    )
  ggplotly(fig, tooltip = c("x", "y", "colour")) %>%
    config(
      modeBarButtonsToRemove = c(
        "zoom2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d",
        "resetScale2d", "hoverCompareCartesian",
        "hoverClosestCartesian", "toggleSpikelines"
      ), displaylogo = FALSE,
      toImageButtonOptions = list(format = "svg", filename = "apsGgap_image")
    ) %>%
    layout(
      legend = list(orientation = "h", x = .1, y = -.5), hovermode = "x",
      xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
    )
}



##################################################### Plot function for A level subject entries and results - for all #############################################

createTimeSeriesSubject <- function(dfSubjectA, subAll, subName) {
  validate(need(dfSubjectA$subject_name, message = " To view chart select type of students and up to 4 subjects from the drop-down menus. Finally select a start year."))

  fig <- dfSubjectA %>%
    rename(Year = "year", Entry_count = "entry_count", Subject = "subject_name")

  fig <- ggplot(
    fig,
    aes(
      x = Year,
      y = Entry_count,
      color = Subject,
      shape = Subject
    )
  ) +
    geom_line(stat = "identity", linewidth = 1) +
    # geom_point(stat="identity", size=1.5, show.legend=F)+

    scale_x_log10(breaks = seq(1996, 2024, 2)) +
    scale_y_continuous(labels = scales::comma) +
    scale_colour_manual(
      values = gss_colour_pallette
    ) +
    labs(x = "", y = "", color = "") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 10),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.line = element_line(linewidth = 1.0)
    ) +
    expand_limits(x = 0, y = 0) +
    ggtitle(paste0("\nA level entry count:  ", subAll))
  ggplotly(fig, tooltip = c("x", "y", "colour")) %>%
    config(
      modeBarButtons = list(list("toImage")), displaylogo = FALSE,
      toImageButtonOptions = list(format = "svg", filename = "subjectAll_image")
    ) %>%
    layout(
      legend = list(orientation = "h", x = .1, y = -.5),
      hovermode = "x",
      xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
    )
}




##  plot for A level subject  cumulative results



createTimeSeriesResult <- function(dfSubjectA, subAll, resAll, subName) {
  validate(need(dfSubjectA$subject_name, message = "To view chart select type of students, select up to 4 subjects and start year from the drop-down menus at the top of the page.  Finally select cumulative grade"))



  fig <- dfSubjectA %>%
    rename(Year = "year", Subject = "subject_name")


  fig <- ggplot(fig, aes_string(x = "Year", y = resAll, color = "Subject")) +
    geom_line(stat = "identity", linewidth = 1, show.legend = F) +
    #   geom_point(stat= "identity", size=1.5, show.legend=F)+
    scale_x_log10(breaks = seq(1996, 2024, 2)) +
    scale_y_continuous(labels = scales::comma) +
    scale_colour_manual(
      # breaks = unique(dfSubjectA$Subject),
      values = gss_colour_pallette
    ) +
    labs(x = "", y = "", color = "") +
    theme_classic() +
    annotate("rect", xmin = 2020, xmax = 2021, ymin = 5, ymax = 100, alpha = .2) +
    theme( # legend.position ="centre",
      legend.title = element_blank(),
      text = element_text(size = 12),
      plot.title = element_text(size = 10),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.line = element_line(linewidth = 1.0)
    ) +
    expand_limits(x = 0, y = 0) +
    ggtitle(paste0("\nA level cumulative percentage: ", resAll, "\n", subAll))

  ggplotly(fig, tooltip = c("x", "y", "colour")) %>%
    config(
      modeBarButtonsToRemove = c(
        "zoom2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d",
        "resetScale2d", "hoverCompareCartesian", "drawrect", "select2d", "lasso2d",
        "hoverClosestCartesian", "toggleSpikelines"
      ), displaylogo = FALSE,
      toImageButtonOptions = list(format = "svg", filename = "resultAll_image")
    ) %>%
    layout(
      legend = list(orientation = "h", x = .1, y = -.5),
      hovermode = "x",
      xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
    )
}



############################################# Plot for subject entries and results A level sex #####################################################

createTimeSeriesSubjectFm <- function(dfSubjectG, subByFm) {
  validate(need(dfSubjectG$subject_name, message = "To view chart select one subject and a start year from the drop-down menus at the top of the page."))


  fig <- dfSubjectG %>%
    rename(Year = "year", Entry_count = "entry_count", Subject = "subject_name", Sex = "characteristic_value")
  fig <- ggplot(
    fig, aes(
      x = Year, y = Entry_count,
      color = Sex
    )
  ) +
    geom_line(stat = "identity", linewidth = 1) +
    # geom_point(size=1.5)+
    scale_x_log10(breaks = seq(1996, 2024, 2)) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(
      # breaks = (dfSubjectG$charateristic_sex),
      values = c("#3D3D3D", "#F46A25", "#12436D")
    ) +
    labs(x = "", y = "", color = "") +
    theme_classic() +
    theme(
      legend.position = "",
      text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(size = 10),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(linewidth = 1.0)
    ) +
    expand_limits(x = 0, y = 0) +
    ggtitle(paste0(" \n A level entry count:  ", subByFm))

  ggplotly(fig, tooltip = c("x", "y", "colour")) %>%
    config(
      modeBarButtonsToRemove = c(
        "zoom2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d",
        "resetScale2d", "hoverCompareCartesian", "drawrect", "select2d", "lasso2d",
        "hoverClosestCartesian", "toggleSpikelines"
      ), displaylogo = FALSE,
      toImageButtonOptions = list(format = "svg", filename = "subjectFm_image")
    ) %>%
    layout(
      xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE),
      legend = list(orientation = "h", x = .4, y = -.5),
      hovermode = "x"
    )
}


createTimeSeriesResultFm <- function(dfSubjectG, subByFm, resByFm) {
  validate(need(dfSubjectG$subject_name, message = " To view chart select one subject and a start year from the drop-down menus and finally select cumulative grade"))
  fig <- dfSubjectG %>%
    rename(Year = "year", Subject = "subject_name", Sex = "characteristic_value")

  fig <- ggplot(fig, aes_string("Year", y = resByFm, color = "Sex")) +
    geom_line(stat = "identity", linewidth = 1) +
    # geom_point(size=1.5)+
    scale_color_manual(values = c("#3D3D3D", "#F46A25", "#12436D")) +
    scale_x_log10(breaks = seq(1996, 2024, 2)) +
    labs(x = "", y = "", color = "") +
    theme_classic() +
    annotate("rect", xmin = 2020, xmax = 2021, ymin = 5, ymax = 100, alpha = .2) +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.text.x = element_text(angle = 300),
      legend.text = element_text(size = 10), legend.title = element_text(size = 10)
    ) +
    expand_limits(x = 0, y = 0) +
    ggtitle(paste0("\n A level cumulative percentage: ", resByFm, "\n ", subByFm))

  ggplotly(fig, tooltip = c("x", "y", "colour")) %>%
    config(
      modeBarButtonsToRemove = c(
        "zoom2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d",
        "resetScale2d", "hoverCompareCartesian", "drawrect", "select2d", "lasso2d",
        "hoverClosestCartesian", "toggleSpikelines"
      ), displaylogo = FALSE,
      toImageButtonOptions = list(format = "svg", filename = "resultFm_image")
    ) %>%
    layout(
      xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE),
      legend = list(orientation = "h", x = .4, y = -.5),
      hovermode = "x"
    )
}


############################ Plot maths and science  ###############################################

createTimeSeriesSubjectMs <- function(dfSubjectMs, subByMs) {
  validate(need(dfSubjectMs$subject, message = "To view chart select one subject and a start year from the drop-down menus at the top of the page."))


  fig <- dfSubjectMs %>%
    rename(Year = "year", Percent = "percent_entered", Subject = "subject", Sex = "characteristic_value")
  fig <- ggplot(
    fig, aes(
      x = Year, y = Percent,
      color = Sex
    )
  ) +
    geom_line(stat = "identity", linewidth = 1) +
    # geom_point(size=1.5)+
    scale_x_log10(breaks = seq(2010, 2024, 1)) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(
      # breaks = (dfSubjectG$charateristic_sex),
      values = c("#3D3D3D", "#F46A25", "#12436D")
    ) +
    labs(x = "", y = "", color = "") +
    theme_classic() +
    theme(
      legend.position = "",
      text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(size = 10),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(linewidth = 1.0)
    ) +
    expand_limits(x = 0, y = 0) +
    ggtitle(paste0(" \n A level percent entry: ", subByMs))

  ggplotly(fig, tooltip = c("x", "y", "colour")) %>%
    config(
      modeBarButtonsToRemove = c(
        "zoom2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d",
        "resetScale2d", "hoverCompareCartesian", "drawrect", "select2d", "lasso2d",
        "hoverClosestCartesian", "toggleSpikelines"
      ), displaylogo = FALSE,
      toImageButtonOptions = list(format = "svg", filename = "subjectMs_image")
    ) %>%
    layout(
      xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE),
      legend = list(orientation = "h", x = .2, y = -.3),
      hovermode = "x"
    )
}
