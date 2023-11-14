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
  "plotAlevelSubject",
  "plotHeadline",
  "plotSubjectFm",
  "tabHeadline",
  "textHeadline",
  "textSubAll",
  "textSubFm"
)

app <- ShinyDriver$new("../../", loadTimeout = 6.4e4)
app$snapshotInit("initial_load_test", screenshot = FALSE)
app$snapshot(list(input = inputs, output = outputs))

app$setInputs(navlistPanel = "dashboard")
app$snapshot(list(input = inputs, output = outputs))

app$setInputs(navlistPanel = "dashboard_alse")
app$snapshot(list(input = inputs, output = outputs))

app$setInputs(navlistPanel = "dashboard_fm")
app$snapshot(list(input = inputs, output = outputs))
