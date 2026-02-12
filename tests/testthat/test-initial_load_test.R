library(shinytest2)

test_that("Migrated shinytest test: initial_load_test.R", {
  inputs <- c(
    "alevelInstitute",
    "allGender",
    "cookieAccept",
    "cookieLink",
    "cookieReject",
    "ees_1",
    "ees_2a",
    "ees_2b",
    "headlineAps",
    "hideAccept",
    "hideReject",
    "link_to_alevelAllSubject_tab",
    "link_to_alevelFmSubject_tab",
    "link_to_headline_tab",
    "navlistPanel",
    "resByAll",
    "resByFm",
    "resetApsAll",
    "resetEntries",
    "resetSubFm",
    "subByAll",
    "subCompareAll",
    "subjectFm",
    "tabsetpanels",
    "tabsetpanels2a",
    "tabsetpanels2b",
    "year_end",
    "year_end_fm",
    "year_start",
    "year_start_fm"
  )


  outputs <- c(
    "dropdown_label",
    "headBox1",
    "headBox2",
    "headBox3",
    "plotHeadline",
    "plotSubjectFm",
    "tabHeadline",
    "textHeadline",
    "textSubAll",
    "textSubFm"
  )

  app <- AppDriver$new(load_timeout = 64000)
  app$expect_values(input = inputs, output = outputs)

  app$set_inputs(navlistPanel = "dashboard")
  app$expect_values(input = inputs, output = outputs)

  app$set_inputs(navlistPanel = "dashboard_alse")
  app$expect_values(input = inputs, output = outputs)

  app$set_inputs(navlistPanel = "dashboard_fm")
  app$expect_values(input = inputs, output = outputs)
})
