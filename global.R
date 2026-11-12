# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.

# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------

# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
# Core shiny and R packages
shhh(library(shiny))
shhh(library(bslib))
shhh(library(rstudioapi))
# Custom packages
shhh(library(dfeR))
shhh(library(dfeshiny))
shhh(library(shinyGovstyle))
# Data and string manipulation
shhh(library(readr))
shhh(library(dplyr))
shhh(library(stringr))

# Creating charts and tables
shhh(library(ggplot2))
shhh(library(plotly))
shhh(library(ggiraph))
shhh(library(sf))
shhh(library(DT))
shhh(library(tidyr))
shhh(library(hrbrthemes))
shhh(library(forcats))
shhh(library(patchwork))
shhh(library(htmltools))
shhh(library(reactable))
shhh(library(svglite))
shhh(library(afcharts))

shhh(library(ggrepel))
shhh(library(showtext))
shhh(library(openxlsx))


# shiny extensions
shhh(library(shinyjs))
shhh(library(shinytitle))
shhh(library(xfun))
shhh(library(metathis))
shhh(library(shinyalert))
shhh(library(shinyWidgets))
shhh(library(shinycssloaders))
shhh(library(shinyBS))

# Dependencies needed for testing or CI but not for the app ---------------------------------
# Including them here keeps them in renv but avoids the app needlessly loading
# them, saving on load time.
if (FALSE) {
  shhh(library(rsconnect))
  shhh(library(shinytest2))
  shhh(library(chromote))
  shhh(library(testthat))
}


# Source scripts ----------------------------------------------------------------------------
# Source script for loading in data

source("R/read_data.R")

# Source custom function script

source("R/helper_functions.R")

# gbp <- enc2utf8("\u00A3")

# Set global variables --------------------------------------------------------------------

site_title <- "16 to 19 time series attainment and single year entry"
site_primary <- "https://department-for-education.shinyapps.io/ks5-timeseries-attainment-and-single-year-entries/"
site_overflow <- "https://department-for-education.shinyapps.io/ks5-timeseries-attainment-and-single-year-entries-overflow/"
sites_list <- c(site_primary, site_overflow) # We can add further mirrors where necessary. Each one can generally handle about 2,500 users simultaneously
ees_pub_name <- "A level and other 16 to 19 results" # Update this with your parent publication name (e.g. the EES publication)
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results" # Update with parent publication link

# Set the key for Google Analytics tracking
google_analytics_key <- "C9DQ5CPBCR"


# Enable bookmarking so that input choices are shown in the url ------------------------------
enableBookmarking("url")

# Fonts for charts ---------------------------------------------------------------------------
font_add("dejavu", "www/fonts/DejaVuSans.ttf")
register_font(
  "dejavu",
  plain = "www/fonts/DejaVuSans.ttf",
  bold = "www/fonts/DejaVuSans-Bold.ttf",
  italic = "www/fonts/DejaVuSans-Oblique.ttf",
  bolditalic = "www/fonts/DejaVuSans-BoldOblique.ttf"
)
showtext_auto()


# A level subject entries - single academic year -------------------------------------------
# Read in A level data for subject entries and results

#  The latest year is mainly used by A level subject entries and Maths and Science to allow selection of year

# update to latest year
latest_year <- 2026

dfAlevelSubjectRaw <- read_alevel_subject_data()


dfAlevelSubject <- dfAlevelSubjectRaw %>%
  rename(
    characteristic_value = "characteristic_value",
    `A*` = "perc_astar_grade_achieved",
    `A*-A` = "perc_astar_a_grade_achieved",
    `A*-B` = "perc_astar_b_grade_achieved",
    `A*-C` = "perc_astar_c_grade_achieved",
    `A*-D` = "perc_astar_d_grade_achieved",
    `A*-E` = "perc_astar_e_grade_achieved",
    version = "data_version"
  ) %>%
  select(
    time_period,
    year,
    subject_name,
    characteristic_value,
    `A*-A`,
    `A*-B`,
    `A*-C`,
    `A*-D`,
    `A*-E`,
    entry_count,
    version
  ) %>%
  mutate(
    entry_count = as.numeric(entry_count, na.rm = TRUE),
    thousand_entries = as.numeric(entry_count / 1000),
    `A*-A` = suppressWarnings(as.numeric(`A*-A`)),
    `A*-B` = suppressWarnings(as.numeric(`A*-B`)),
    `A*-C` = suppressWarnings(as.numeric(`A*-C`)),
    `A*-D` = suppressWarnings(as.numeric(`A*-D`)),
    `A*-E` = suppressWarnings(as.numeric(`A*-E`)),
    subject_name = as.factor(subject_name),
    characteristic_value = as.factor(characteristic_value),
    version = as.factor(version)
  )

## intention here is to select time-series data from 1996 to date.
# Some subjects have filtered out inlcuding subjects greater or equal to 10 years
subjectByAll <- dfAlevelSubject %>%
  group_by(subject_name, characteristic_value) %>%
  arrange(year, .by_group = TRUE) %>%
  filter(
    !subject_name %in%
      c(
        "Other communication studies",
        "Other social studies",
        "Home economics",
        "ICT"
      ),
    n() > 11
  ) %>%
  ungroup()

# Filter out female and male

subjectBySex <- subjectByAll # %>%
# filter(characteristic_sex != "All students")

# Filter home economics for all students
homeEconomics <- dfAlevelSubject %>%
  filter(
    subject_name == "Home economics" & characteristic_value == "All students"
  )


# A level attainment  by institution type ----------------------------------------

# Read in

alevel_attain_download <- read_alevel_aps_data()

data <- read_alevel_aps_data()
dfAlevelAps <- data %>%
  rename(
    aps_2013_2015 = "aps_per_entry_2013_2015",
    aps_grade_2013_2015 = "aps_per_entry_grade_2013_2015",
    entry_count = "aps_per_entry_student_count"
  ) %>%
  select(
    time_period,
    year,
    establishment_type,
    establishment_type_group,
    entry_count,
    aps_per_entry,
    aps_2013_2015,
    aps_per_entry_grade,
    aps_grade_2013_2015,
    sex,
    time_period,
    year,
    year_2013_2015,
    version
  ) %>%
  mutate(
    establishment_type = recode(
      establishment_type,
      "All state-funded schools and colleges" = "All state-funded"
    )
  ) %>%
  mutate(
    aps_2013_2015 = round2(as.numeric(aps_2013_2015), 2),
    aps_grade_2013_2015 = as.factor(aps_grade_2013_2015),
    aps_per_entry = round2(as.numeric(aps_per_entry), 2),
    aps_per_entry_grade = as.factor(aps_per_entry_grade),
    sex = as.factor(sex),
    establishment_type = as.factor(establishment_type),
    establishment_type_group = as.factor(establishment_type_group),
    version = as.factor(version)
  )


# Create sex gap between female and male using long width
# Expect similar result on sex gap column for female an male
# Use data from 2015/16

dfApsSexGap <- read_alevel_aps_sexgap_data() %>%
  mutate(
    establishment_type = recode(
      establishment_type,
      "All state-funded schools and colleges" = "All state-funded"
    )
  )

fmDiff <- dfApsSexGap %>%
  mutate(
    sex_gap = round2(as.numeric(sex_gap), 1),
    establishment_type = as.factor(establishment_type)
    #  "All schools and FE sector colleges" = "All institutions",
    # "All state-funded schools and colleges" = "All state-funded"
  )


# Headline attainment --------------------------------------------------------------------------------------------

# Read in attainment data for alevel, applied general and techlevel

headline_download <- read_all_attainment_data()

headline_download <- headline_download %>%
  rename(
    entry_count = "aps_per_entry_student_count",
    aps = "aps_per_entry",
    aps_grade = "aps_per_entry_grade"
  ) %>%
  mutate(
    time_identifier = "Academic year",
    geographic_level = "National",
    country_code = "E92000001",
    country_name = "England"
  ) %>%
  select(
    time_period,
    time_identifier,
    geographic_level,
    country_code,
    country_name,
    version,
    establishment_type,
    establishment_type_group,
    sex,
    entry_count,
    aps, # = aps_per_entry,
    aps_grade, # = aps_per_entry_grade,
    version,
    exam_cohort,
    year
  ) %>%
  arrange(desc(time_period))


dfAttainmentRaw <- read_all_attainment_data() %>%
  rename(
    entry_count = "aps_per_entry_student_count",
    aps = "aps_per_entry",
    aps_grade = "aps_per_entry_grade"
  ) %>%
  mutate(
    establishment_type = recode(
      establishment_type,
      "All state-funded schools and colleges" = "All state-funded"
    )
  )

# Select  all students and data to factors and numeric
dfAttainment <- dfAttainmentRaw %>%
  filter(sex == "All students") %>%
  mutate(
    establishment_type = as.factor(establishment_type),
    establishment_type_group = as.factor(establishment_type_group),
    entry_count = as.integer(entry_count),
    aps = as.numeric(aps),
    aps = round2(aps, 2),
    aps_grade = as.factor(aps_grade),
    sex = as.factor(sex),
    version = as.factor(version),
    exam_cohort = as.factor(exam_cohort),
    exam_cohort = as.factor(recode(exam_cohort, "Alevel" = "A level"))
  ) %>%
  group_by(establishment_type, exam_cohort) %>%
  arrange(desc(year), .by_group = TRUE) %>%
  ungroup()


# Maths Science attainment -----------------------------------------------------------------
#
#
dfMsRaw <- read_all_ms_data()

dfMs <- dfMsRaw %>%
  mutate(
    subject = as.factor(subject),
    breakdown = as.factor(breakdown),
    version = as.factor(version)
  ) %>%
  filter(
    !subject %in%
      c(
        "Five Maths/Science subjects",
        "Six Maths/Science subjects",
        "Total Students"
      )
  )
