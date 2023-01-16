library(shinytest)
expect_pass(testApp("server.R/", compareImages = grepl("^macOS", utils::osVersion)))
