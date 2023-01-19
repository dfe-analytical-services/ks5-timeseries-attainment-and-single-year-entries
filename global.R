# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------


# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(shiny))
#shhh(library(shinya11y))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinytest))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(shinycssloaders))
shhh(library(dplyr))
shhh(library(tidyr))
shhh(library(ggplot2))
shhh(library(plotly))
shhh(library(DT))
shhh(library(xfun))
shhh(library(hrbrthemes))
shhh(library(gridExtra))
shhh(library(forcats))
shhh(library(ggrepel))





# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# tidy_code_function -------------------------------------------------------------------------------
# Code to tidy up the scripts.

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, test_scripts)
  return(script_changes)
}


# Code to convert data to numeric


to_numeric=function(df, variables) {
  for (variable in variables) {
    df[[variable]]<-(suppressWarnings(as.numeric(df[[variable]])))
  }
  return(df)
}

# Code to convert data to factor
to_factor=function(df, variables) {
  for (variable in variables) {
    df[[variable]]<-(suppressWarnings(as.factor(df[[variable]])))
  }
  return(df)
}

# Code to convert first character to capital letter
first_xter_up<-function(x) {
  x<-tolower(x)
  substr(x,1, 1) <- toupper (substr(x,1,1))
  x
}

# 
# spinner<-function(output){
#   shinycssloaders::withSpinner(output, color.background = "#e9f0ff",color="#1d70b8",  size = 1)
# }



# Source scripts ---------------------------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")


# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

site_primary <- "https://department-for-education.shinyapps.io/dfe-shiny-template/"
site_overflow <- "https://department-for-education.shinyapps.io/dfe-shiny-template-overflow/"

source("R/support_links.R")
source("R/read_data.R")


############################  Read A level subject entries - single academic year ####################################################
# Read in A level data for subject entries and results
# Filter out 'Total subjects' and data greater than 7 years
# Create new column for thousand entries


latest_year <-2022

dfAlevelSubjectRaw<-read_alevel_subject_data()

#dfAlevelSubject$subject_name<-first_xter_up(dfAlevelSubject$subject_name)

dfAlevelSubject<- dfAlevelSubjectRaw%>%
  rename(characteristic_gender="characteristic_value", `A*`="perc_astar_grade_achieved",
         A="perc_a_grade_achieved", B="perc_b_grade_achieved", C="perc_c_grade_achieved",
         D="perc_d_grade_achieved", E="perc_e_grade_achieved", `A*-A`="perc_astar_a_grade_achieved",
         `A*-B`="perc_astar_b_grade_achieved", `A*-C` ="perc_astar_c_grade_achieved",
         `A*-D`="perc_astar_d_grade_achieved", `A*-E` = "perc_astar_e_grade_achieved", version="data_version") %>%
  select(time_period, year, subject_name, characteristic_gender, `A*-A`, 
         `A*-B`, `A*-C`, `A*-D`, `A*-E`,  entry_count, version) %>%
  mutate(entry_count = as.numeric(entry_count),
         thousand_entries=as.numeric(entry_count/1000), 
         subject_name = as.factor(subject_name),
         characteristic_gender = as.factor(characteristic_gender),
         version =as.factor(version))
  



subjectByAll <-dfAlevelSubject %>%
  group_by(subject_name, characteristic_gender) %>%
  arrange (year, .by_group=TRUE) %>%
  filter(!subject_name %in% c("Total subjects", "Other communication studies", "Other social studies", "Home economics"), n()>7)%>% 
  ungroup()

# Filter out female and male

subjectByGender<-subjectByAll %>%
  filter(characteristic_gender != "All students") 

# Filter home economics for all students 
homeEconomics<- dfAlevelSubject %>%
  filter(subject_name=="Home economics" & characteristic_gender =="All students")

# Bind home economics all students to a level subjects 

# subjectBind <- rbind(subjectByAll, homeEconomics)
# 
# subjectPivot<-subjectBind %>%
#   pivot_longer(
#     cols = starts_with("A*"), 
#     names_to="grade",
#     #names_prefix="A*",
#     values_to="percent"
#   )
#  
# subjectAll<-subjectPivot %>%
#   #filter(characteristic_gender=="All students") %>%
#   group_by(subject_name, characteristic_gender, grade) %>%
#   arrange (year, .by_group=TRUE) %>%
#   ungroup()






#####################################################################################################


# Read in  A level aggregate APS and grade by institution type


data<-read_alevel_aps_data()
dfAlevelAps<-data %>% 
  rename(aps_2016_2022="aps_per_entry_2016_2022", aps_grade_2016_2022="aps_per_entry_grade_2016_2022", aps_2013_2015 = "aps_per_entry_2013_2015",
         aps_grade_2013_2015="aps_per_entry_grade_2013_2015") %>%
  select(time_period,	year, school_type, school_type_group, number_of_students, aps_2016_2022,	aps_2013_2015, aps_grade_2016_2022,	aps_grade_2013_2015,
         characteristic_gender,	time_period,	year,	year_2013_2015,	year_2016_2022, version) %>%
  filter(year >=2013 & !school_type %in% c("Converter Academies", "Sponsored Academies", "State-funded special schools",
                             "Academies and Free Schools", "All Maintained Schools", "State-funded mainstream schools", 
                             "Mainstream free schools, free school 16-19, university technical colleges (UTCs), city technology colleges (CTCs) and studio schools",
                             "Free Schools, University Technical Colleges (UTCs) and Studio Schools", "All special schools", "Comprehensive Schools", 
                             "Mainstream free schools, university technical colleges (UTCs) and studio schools", "Modern Schools",
                             "Selective Schools")) %>%
  # filter(year>=2013) %>%
  mutate(aps_grade_2016_2022=as.factor(aps_grade_2016_2022),
         aps_grade_2013_2015=as.factor(aps_grade_2013_2015),
         aps_2016_2022 = round(as.numeric(aps_2016_2022),2),
         aps_2013_2015 = round(as.numeric(aps_2013_2015),2),
         characteristic_gender=as.factor(characteristic_gender),
         school_type=as.factor(school_type), 
         school_type_group=as.factor(school_type_group),
         version=as.factor(version)) 
 
choicesSchool_type_group <- unique(dfAlevelAps$school_type_group)
choicesSchool_type<- unique(dfAlevelAps$school_type_type)
choicesGender<-unique(dfAlevelAps$characteristic_gender)


# Create gender gap between female and male using long width
# Expect similar result on gender gap column for female an male
# Use data from 2015/16




fmGap<-dfAlevelAps %>%
  select(year, time_period, school_type, school_type_group, characteristic_gender, number_of_students, aps_2016_2022, version) 
  

female_male<- fmGap %>%
  filter(characteristic_gender!="All students" & year >=2016) %>%
  group_by(school_type_group, school_type, year) %>%
  arrange(school_type, .by_group = TRUE) %>%
  mutate(aps_2016_2022,
         gender_gap =  aps_2016_2022[characteristic_gender =="Female"] - aps_2016_2022[characteristic_gender=="Male"])%>%
  ungroup()


fmDiff<- female_male%>%
  filter(characteristic_gender=="Female") %>%
  select(year, time_period, school_type, school_type_group, number_of_students, aps_2016_2022, gender_gap, version) %>%
  mutate(
    gender_gap=round(gender_gap,1)
  )





########################################################################################################


# Read in attainment data for alevel, applied general and techlevel

dfAttainment<-read_all_attainment_data()


# Select  all students and data to factors and numeric
dfAttainment<-dfAttainment %>%
  filter(characteristic_gender=="All students" & number_of_students >1) %>%
  mutate(school_type=as.factor(school_type),
         school_type_group=as.factor(school_type_group),
         number_of_students=as.integer(number_of_students),
         aps=round(as.numeric(aps), 2),
         aps_grade=as.factor(aps_grade),
         characteristic_gender=as.factor(characteristic_gender),
         version=as.factor(version),
         cert_type=as.factor(cert_type)) %>%
  group_by(school_type, cert_type) %>%
  arrange(year, .by_group = TRUE) %>%
  ungroup()




