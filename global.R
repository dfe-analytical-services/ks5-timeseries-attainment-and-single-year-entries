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
# shhh(library(shinya11y))
shhh(library(shinyjs))
# shhh(library(tools))
shhh(library(testthat))
shhh(library(shinytest2))
shhh(library(shinydashboard))
shhh(library(shinytitle))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(shinycssloaders))
shhh(library(shinyBS))
shhh(library(dplyr))
shhh(library(tidyr))
shhh(library(ggplot2))
shhh(library(plotly))
shhh(library(DT))
shhh(library(xfun))
shhh(library(hrbrthemes))
shhh(library(forcats))
shhh(library(patchwork))
shhh(library(readr))
shhh(library(dfeshiny))


# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}
# Code to convert data to numeric
to_numeric <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- (suppressWarnings(as.numeric(df[[variable]])))
  }
  return(df)
}

# Code to convert data to factor
to_factor <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- (suppressWarnings(as.factor(df[[variable]])))
  }
  return(df)
}

# Code to convert first character to capital letter
first_xter_up <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


spinner <- function(output) {
  shinycssloaders::withSpinner(output, size = getOption("spinner.size", default = 1))
}


# round data
round2 <- function(x, n) {
  posneg <- sign(x)
  z <- abs(x) * 10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^n
  z * posneg
}


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

site_title <- "16 to 18 time series attainment and single year entry"
site_primary <- "https://department-for-education.shinyapps.io/ks5-timeseries-attainment-and-single-year-entries/"
site_overflow <- "https://department-for-education.shinyapps.io/ks5-timeseries-attainment-and-single-year-entries-overflow/"
sites_list <- c(site_primary, site_overflow) # We can add further mirrors where necessary. Each one can generally handle about 2,500 users simultaneously
ees_pub_name <- "A level and other 16 to 18 results" # Update this with your parent publication name (e.g. the EES publication)
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results" # Update with parent publication link
google_analytics_key <- "C9DQ5CPBCR"
source("R/read_data.R")


############################  Read A level subject entries - single academic year ####################################################
# Read in A level data for subject entries and results

#  The latest year is mainly used by A level subject entries and Maths and Science to allow selection of year

latest_year <- 2025

dfAlevelSubjectRaw <- read_alevel_subject_data()


dfAlevelSubject <- dfAlevelSubjectRaw %>%
  rename(
    characteristic_value = "characteristic_value", `A*` = "perc_astar_grade_achieved",
    `A*-A` = "perc_astar_a_grade_achieved",
    `A*-B` = "perc_astar_b_grade_achieved", `A*-C` = "perc_astar_c_grade_achieved",
    `A*-D` = "perc_astar_d_grade_achieved", `A*-E` = "perc_astar_e_grade_achieved", version = "data_version"
  ) %>%
  select(
    time_period, year, subject_name, characteristic_value, `A*-A`,
    `A*-B`, `A*-C`, `A*-D`, `A*-E`, entry_count, version
  ) %>%
  mutate(
    entry_count = as.numeric(entry_count),
    thousand_entries = as.numeric(entry_count / 1000),
    `A*-A` = as.numeric(`A*-A`),
    `A*-B` = as.numeric(`A*-B`),
    `A*-C` = as.numeric(`A*-C`),
    `A*-D` = as.numeric(`A*-D`),
    `A*-E` = as.numeric(`A*-E`),
    subject_name = as.factor(subject_name),
    characteristic_value = as.factor(characteristic_value),
    version = as.factor(version)
  )

## intention here is to select timeseries data from 1996 to date.
# Some subjects have filtered out inlcuding subjects greater or equal to 10 years
subjectByAll <- dfAlevelSubject %>%
  group_by(subject_name, characteristic_value) %>%
  arrange(year, .by_group = TRUE) %>%
  filter(!subject_name %in% c("Other communication studies", "Other social studies", "Home economics", "ICT"), n() > 10) %>%
  ungroup()

# Filter out female and male

subjectBySex <- subjectByAll # %>%
# filter(characteristic_sex != "All students")

# Filter home economics for all students
homeEconomics <- dfAlevelSubject %>%
  filter(subject_name == "Home economics" & characteristic_value == "All students")


#####################################################################################################


# Read in  A level aggregate APS and grade by institution type


alevel_attain_download <- read_alevel_aps_data()

data <- read_alevel_aps_data()
dfAlevelAps <- data %>%
  rename(
    aps_2013_2015 = "aps_per_entry_2013_2015",
    aps_grade_2013_2015 = "aps_per_entry_grade_2013_2015"
  ) %>%
  select(
    time_period, year, establishment_type, establishment_type_group, number_of_students, aps_per_entry, aps_2013_2015, aps_per_entry_grade, aps_grade_2013_2015,
    characteristic_sex, time_period, year, year_2013_2015, version
  ) %>%
  mutate(establishment_type = case_match(
    establishment_type, "All state-funded schools and colleges" ~ "All state-funded",
    .default = establishment_type
  )) %>%
  mutate(
    aps_2013_2015 = round2(as.numeric(aps_2013_2015), 2),
    aps_grade_2013_2015 = as.factor(aps_grade_2013_2015),
    aps_per_entry = round2(as.numeric(aps_per_entry), 2),
    aps_per_entry_grade = as.factor(aps_per_entry_grade),
    characteristic_sex = as.factor(characteristic_sex),
    establishment_type = as.factor(establishment_type),
    establishment_type_group = as.factor(establishment_type_group),
    version = as.factor(version)
  )


# Create sex gap between female and male using long width
# Expect similar result on sex gap column for female an male
# Use data from 2015/16


dfApsSexGap <- read_alevel_aps_sexgap_data() %>%
  mutate(establishment_type = case_match(
    establishment_type, "All state-funded schools and colleges" ~ "All state-funded",
    .default = establishment_type
  ))

fmDiff <- dfApsSexGap %>%
  mutate(
    sex_gap = round2(as.numeric(sex_gap), 1),
    establishment_type = as.factor(establishment_type)
    #  "All schools and FE sector colleges" = "All institutions",
    # "All state-funded schools and colleges" = "All state-funded"
  )


########################################################################################################


# Read in attainment data for alevel, applied general and techlevel


headline_download <- read_all_attainment_data()

headline_download <- headline_download %>%
  mutate(
    time_identifier = "Academic year",
    geographic_level = "National",
    country_code = "E92000001",
    country_name = "England"
  ) %>%
  select(time_period, time_identifier, geographic_level, country_code, country_name, version,
    establishment_type, establishment_type_group, characteristic_sex, number_of_students,
    aps_per_entry = aps,
    aps_per_entry_grade = aps_grade, version, qualification_type = cert_type, year
  ) %>%
  arrange(desc(time_period))


dfAttainmentRaw <- read_all_attainment_data() %>%
  mutate(establishment_type = case_match(
    establishment_type, "All state-funded schools and colleges" ~ "All state-funded",
    .default = establishment_type
  ))

# Select  all students and data to factors and numeric
dfAttainment <- dfAttainmentRaw %>%
  filter(characteristic_sex == "All students") %>%
  mutate(
    establishment_type = as.factor(establishment_type),
    establishment_type_group = as.factor(establishment_type_group),
    number_of_students = as.integer(number_of_students),
    aps = as.numeric(aps),
    aps = round2(aps, 2),
    aps_grade = as.factor(aps_grade),
    characteristic_sex = as.factor(characteristic_sex),
    version = as.factor(version),
    cert_type = as.factor(cert_type),
    cert_type = as.factor(recode(cert_type, "Alevel" = "A level"))
  ) %>%
  group_by(establishment_type, cert_type) %>%
  arrange(desc(year), .by_group = TRUE) %>%
  ungroup()


#############################################################################################################
# Maths Science attainment
#
#
dfMsRaw <- read_all_ms_data()

dfMs <- dfMsRaw %>%
  mutate(
    subject = as.factor(subject),
    characteristic_value = as.factor(characteristic_value),
    version = as.factor(version)
  ) %>%
  filter(!subject %in% c("Five Maths/Science subjects", "Six Maths/Science subjects", "Total Students"))
