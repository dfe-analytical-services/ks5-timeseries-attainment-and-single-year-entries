#### contains plot functions


#  plot function for headline attainment for 

createTimeSeriesHeadline <- function(dfAps, allAps){
 
  fig<-ggplot(
    
    dfAps, aes(x=year, y=aps, group=cert_type)) +
    geom_col(aes(fill=cert_type), width=0.9, position=position_dodge())+  
    
    geom_text(aes(label=aps_grade, group=cert_type),  position=position_dodge(0.9), vjust=.4, hjust="top",  size=5, color="white", angle=90, show.legend=F)+
    
    ggtitle("Average point score and grade \n ", allAps ) +
    #  coord_cartesian(ylim=c(10,60)) +
    scale_x_continuous(breaks=seq(2016,2022,1)) +
    scale_fill_manual(values =c('#12436D','#28A197','#801650'))+
    labs(x="", y="")+
    
    theme_classic() +
    theme(legend.position = 'bottom', #c(.2,.85),
          legend.direction="horizontal",
          text = element_text(size = 12),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          # plot.margin=margin(rep(15,4)),
          axis.text.x = element_text(angle=45, hjust=1), legend.text=element_text(size=12), legend.title=element_blank()) + 
    expand_limits(x=2016, y=0)
  
  fig
  
  
}


# Joint plot function for  Alevel Aps from 2013  

createApsTimeSeries <- function(dfAps, instGroup, instType, allGender){
  validate(need(instType, message="To view charts select required student type from the drop-down menus at the top of the page, then choose 
  between 1 and 4 institution groups followed by institution types."))
  
  fig1<-ggplot(dfAps, aes(x=year_2013_2015, y=aps_2013_2015,
                             color=school_type)) +
    geom_line(stat="identity", size=1.5) +
    geom_curve(aes(x=2015.5, y=50, xend=2015, yend=45),
               arrow = arrow(length=unit(0.03, "npc"), type="closed"),
               color="black", size=.05, angle=45 )+
    
    geom_label(aes(label=aps_grade_2013_2015, y=aps_2013_2015), show.legend=F)+
    ggtitle("\nAPS & grade\n2012/13 to 2014/15\n") +
    coord_cartesian(ylim=c(150,300)) +
    scale_x_continuous(breaks=seq(2013,2015,1)) +
    scale_colour_manual(
      #"school type",
      breaks = unique(dfAps$school_type),
      values = gss_colour_pallette) +
    theme_classic()+
    labs(x="", y="")+
    theme(legend.position= "none",
          text = element_text(size = 12),
          axis.text.x = element_text(angle = 300),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(r = 12)),
          axis.line = element_line( size = 1.0)
          )

  fig2<-ggplot(dfAps,  aes(x= year_2016_2022, y= aps_2016_2022, color= school_type)) +
    geom_line(stat="identity", size=1.5) +
    geom_curve(aes(x=2016.5, y=50, xend=2016, yend=45),  curvature=.3,
               arrow = arrow(length=unit(0.03, "npc"), type="closed"),
               color="black", size=.05, angle=45 )+

    geom_curve(aes(x=2020.5, y=59, xend=2021, yend=52),  curvature=-.3,
               arrow = arrow(length=unit(0.03, "npc"), type="closed"),
               color="black", size=.05, angle=90 )+

    geom_curve(aes(x=2019.5, y=55, xend=2020, yend=50),  curvature=-.3,
               arrow = arrow(length=unit(0.03, "npc"), type="closed"),
               color="black", size=.05, angle=90 )+
    geom_label(aes(label=aps_grade_2016_2022, y=aps_2016_2022), show.legend=F)+
    ggtitle(paste0("\n APS & grade 2015/16 to 2021/22 \n", allGender)) + 
    coord_cartesian(ylim=c(10,60)) +
    scale_x_continuous(breaks=seq(2016,2022,1)) +
    scale_colour_manual(
      "",
      breaks = unique(dfAps$school_type),
      values = gss_colour_pallette) +
  
    labs(x="", y="", color="")+
    theme_classic() +
    annotate (geom="text", x= 2016.5, y=45, label="Average point score (APS) - point scale \n changed in 2015/16 but APS as grades remain unchanged ", color="black",size=4, vjust=-.3, hjust=0)+
    annotate ("rect", xmin= 2020, xmax=2021, ymin=25, ymax=50, alpha=.2)+
    annotate (geom="text", x= 2019, y=55, label="Centre assessment grade 2019/20 ", color="black",size=4, vjust=-.3, hjust=0)+
    annotate (geom="text", x= 2019.5, y=60, label="Teacher assessed grade 2020/21 ", color="black",size=4, vjust=-.3, hjust=0)+
    theme(
      legend.position = c(.8,.15),
      legend.title=element_blank(),
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line( size = 1.0)
            
    )

  finalFig<-grid.arrange(fig1,fig2, widths=c(0.7,2), ncol=2)

  return(finalFig)

}


##### Plot function for APS by gender ##################################


createApsFmTimeSeries <- function(dfAps, instGroup, instType, fmGender){
  validate(need(instType, message="To view charts select between 1 and 4 institution groups and institution types from the drop-down menus at the top page."))
  # 
  # fmFig<-ggplot(dfAps, aes(x=year_2016_2022, y=aps_2016_2022,
  #                         color=school_type)) +
  
  
  fmFig<-ggplot(dfAps, aes(x=year_2016_2022, y=aps_2016_2022,
                           color=fct_reorder2(school_type, year_2016_2022, aps_2016_2022 ))) +
    geom_line(stat="identity", size=1.5) +
    geom_curve(aes(x=2020.5, y=59, xend=2021, yend=52),  curvature=-.3,
                 arrow = arrow(length=unit(0.03, "npc"), type="closed"),
                 color="black", size=.05, angle=90 )+
      
      geom_curve(aes(x=2019.5, y=55, xend=2020, yend=50),  curvature=-.3,
                 arrow = arrow(length=unit(0.03, "npc"), type="closed"),
                 color="black", size=.05, angle=90 )+
      
      geom_label(aes(label=aps_grade_2016_2022, y=aps_2016_2022), show.legend=F)+
      
      ggtitle(paste0("Average point score and grade \n", fmGender)) + #from 2016 to 2021") +
      coord_cartesian(ylim=c(10,60)) +
      #scale_x_continuous(breaks=seq(2016,2022,1)) +
      scale_colour_manual(
      #"school type",
      breaks = unique(dfAps$school_type),
      values = gss_colour_pallette) +
      labs(x="", y="", color="")+
      theme_classic() +
      annotate ("rect", xmin= 2020, xmax=2021, ymin=25, ymax=50, alpha=.2)+
      annotate (geom="text", x= 2017, y=55, label="Centre assessment grade 2019/20 ", color="black",size=4, vjust=-.3, hjust=0)+
      annotate (geom="text", x= 2018, y=60, label="Teacher assessed grade 2020/21 ", color="black",size=4, vjust=-.3, hjust=0)+
      
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            legend.title=element_blank(),
            text = element_text(size = 12),
            axis.text.x = element_text(angle = 300),
            axis.title.x = element_blank(),
            axis.title.y = element_text(margin = margin(r = 12)),
            axis.line = element_line( size = 1.0)+
             expand_limits(x=0, y=0)) 
  fmFig
  
}



createGenderGap<- function(dfGgap, instGroup, instType){
  validate(need(dfGgap$school_type, message="To view chart select between 1 and 4 institution groups and institution types from the drop-down menus at the top page."))
  
 
  fig<- ggplot(dfGgap, aes(x=year,
                           y=gender_gap,
                           color=school_type
                           )) +
    geom_line(stat="identity", size=1) +
    # geom_point(size=1)+
    scale_x_log10() +
    scale_y_continuous(labels=scales::comma) +
    scale_colour_manual(
     # "school type",
     #breaks = unique(dfGgap$school_type),
     values = gss_colour_pallette) +
    labs(x="", y="", color="")+
    geom_hline(yintercept= 0, linetype='dot', col="navy", vjust=.80)+
    
  #  geom_text(aes(label=gender_gap, y=gender_gap), hjust="top", vjust=-0.5, size=5,  angle=45, show.legend=F)+
    
    ggtitle(paste0("\nFemale-male average points difference (gender gap)")) +
    theme_classic() +
    theme(#legend.position ="bottom",
          text = element_text(size = 12),
          legend.direction = "vertical",
          plot.title=element_text(size=10),
          axis.text.x = element_text(angle = 300),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.line = element_line( size = 1.0)) 
   # expand_limits(x=0, y=0)

    ggplotly(fig)%>%
      layout(legend=list(orientation="v", x=.99, y=.5))
    
    fig
  
  
}



##### Plot function for A level subject entries - for all ########################

createTimeSeriesSubject<- function(dfSubject, subAll){
  validate(need(dfSubject$subject_name, message=" To view chart select type of students and up to 4 subjects from the drop-down menus. Finally select a start year."))

  fig<-ggplot(dfSubject, aes(x=year,
                         y=entry_count,
                         color=subject_name
                        #color=fct_reorder2(subject_name, year, entry_count)
                        )) +
   
    geom_line(stat="identity", size=1) +


    scale_x_log10() +
    scale_y_continuous(labels=scales::comma) +
    scale_colour_manual(
      "",
     # breaks = unique(dfSubject$subject_name),
      values = gss_colour_pallette) +


    labs(x="", y="", color="")+
    theme_classic() +

    theme(legend.position ="bottom",
          text = element_text(size = 12),
          legend.direction = "vertical",
          plot.title=element_text(size=10),
          axis.text.x = element_text(angle = 300),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.line = element_line( size = 1.0)) +
   expand_limits(x=0, y=0) +
   ggtitle(paste0("\nEntry count:\n ",  subAll))

 ggplotly(fig)%>%
    layout(legend=list(orientation="v", x=.99, y=.5))


}
  


##  plot for A level subject  cumulative results

createTimeSeriesResult<- function(dfResult, resAll, subAll, subName){
  validate(need(dfResult$subject_name, message="To view chart select type of students and up to 4 subjects from the drop-down menus. Select a cumulative percentage grade and finally select a start year."))
 
  fig<-ggplot(
    dfResult, aes_string(x="year", y=resAll, color="subject_name")) +
    geom_line(size=1)+
    scale_x_log10() +
    scale_y_continuous(labels=scales::comma) +
    scale_colour_manual( 
      "",
    #breaks = unique(dfResult$subject_name),
    values = gss_colour_pallette) +
    labs(x="", y="", color="")+
    theme_classic() +
    annotate ("rect", xmin= 2019.5, xmax=2021, ymin=5, ymax=100, alpha=.2)+
    theme(#legend.position ="centre",
      text = element_text(size = 12),
          plot.title=element_text(size=10),
          axis.text.x = element_text(angle = 300),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.line = element_line( size = 1.0)) +
    expand_limits(x=0, y=0) +
    ggtitle(paste0("\nCumulative percentage: ", resAll, "\n", subAll ))
  ggplotly(fig)  %>%
    layout(legend=list(orientation="v", x=.99, y=.5),
           hovermode="x, unified")
  
 # fig
  
  
}





  
 ######### Plot for subject entries A level gender ########



createTimeSeriesSubjectFm<- function(dfSubjectFm, subByFm){
  validate(need(subByFm, message="To view chart select one subject and a start year from the drop-down menus at the top of the page."))
  
  fig<-ggplot(
    dfSubjectFm, aes(x=year, y=entry_count, 
                   color=characteristic_gender)) +
    geom_line(stat="identity", size=1)+
    #geom_point(size=1)+
    scale_x_log10() +
    scale_y_continuous(labels=scales::comma) +
    
    scale_color_manual(
    #breaks = (dfSubjectFm$charateristic_gender),
    values =c("#A285D1", "#3D3D3D"))+
    labs(x="", y="", color="")+
    #geom_vline(xintercept= 2000, linetype='dot', col="navy", vjust=.80)+
    #geom_vline(xintercept= 2004, linetype='dot', col="navy", vjust=.80) +
    theme_classic() +
    theme(legend.position= "",
          
        
          legend.box.background=element_blank(),
          legend.key= element_blank(),
          legend.justification = "right",
          
           plot.title=element_text(size=10),
            axis.text.x = element_text(angle = 300),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(margin = margin(r = 12)),
            axis.line = element_line( size = 1.0))+
    expand_limits(x=0, y=0) +
    ggtitle(paste0(" \n Entry count\n ",  subByFm))
  
  ggplotly(fig)  %>%
    layout(legend=list(orientation="v", x=.99, y=.5),
           hovermode="x, unified")
  
 # fig


} 



createTimeSeriesResultFm <- function(dfSubjectFm, subByFm,resByFm){
  
 validate(need(resByFm, message=" To view chart select one subject and cumulative percentage grade from the drop-down menus and finally select a start year"))
  
 
  fig<-ggplot(
    dfSubjectFm, aes_string(x="year", y=resByFm, color="characteristic_gender")) +
  #  dfSubjectFm, aes(x=.data[[year]], y=.data[[resByFm]], color=.data[[characteristic_gender]])) +
    geom_line(stat="identity", size=1)+
   # geom_point(size=1)+
    
    scale_color_manual(values=c("#A285D1", "#3D3D3D"))+
    
    
    scale_x_log10() +
    labs(x="", y="", color="")+
    theme_classic() +
    annotate ("rect", xmin= 2020, xmax=2021, ymin=5, ymax=100, alpha=.2)+
    # annotate (geom="text", x=2015, y=2, 
    #           label="Centre assessment grade 2019/20 \n & teacher assessed grade 2020/21", 
    #           color="black", size=3, vjust=-.3, hjust=0)+
    
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=10),
          plot.title=element_text(size=10),
          axis.text.x = element_text(angle=45, hjust=1),
          legend.text=element_text(size=10),legend.title=element_text(size=10)) +
    
    expand_limits(x=0, y=0) +
    ggtitle(paste0("\n Cumulative percentage: ",resByFm, "\n ", subByFm ))
  
  ggplotly(fig) %>%
    layout(legend=list(orientation="v", x=.99, y=.5),
           hovermode="x, unified")
  
  #fig
  
} 



