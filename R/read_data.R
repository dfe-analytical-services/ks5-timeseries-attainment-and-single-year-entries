# Script where we provide functions to read in the data file(s).

# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.

# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# text or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.

# This data is for a level subject single entry - academic year
# Select the columns to be used
# Filter out total subjects

read_alevel_subject_data <- function(
  file = "data/all_alevel_timeseries_data_1996_to_2025.csv"
) {
  dfSubject <- read_csv(file, show_col_types = FALSE)
  dfSubject <- dfSubject

  return(dfSubject)
}

# This data is for Aggregate A level APS and grade by institution type
read_alevel_aps_data <- function(
  file = "data/alevel_attainment_with_2025.csv"
) {
  dfAlevelAps <- read_csv(file, show_col_types = FALSE)
  dfAlevelAps <- dfAlevelAps

  return(dfAlevelAps)
}

# This data is for A level APS gender gap by institution type
read_alevel_aps_sexgap_data <- function(
  file = "data/aps_gender_gap_with_2025.csv"
) {
  dfApsGenderGap <- read_csv(file, show_col_types = FALSE)
  dfApsGenderGap <- dfApsGenderGap

  return(dfApsGenderGap)
}

# This is the full attainment dataset  comprising of Alevel, applied general and tech level

read_all_attainment_data <- function(
  file = "data/all_attainment_with_2025.csv"
) {
  dfAttainment <- read_csv(file, show_col_types = FALSE)
  dfAttainment <- dfAttainment

  return(dfAttainment)
}

read_all_ms_data <- function(file = "data/maths_science_2010_2025.csv") {
  dfMsRaw <- read_csv(file, show_col_types = FALSE)
  dfMsRaw <- dfMsRaw
}
