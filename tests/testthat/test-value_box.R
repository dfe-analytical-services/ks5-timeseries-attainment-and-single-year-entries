# Unit tests are easy to write and quick to run, make use of them where you can
# For more information, look at the testthat package documentation.
# -----------------------------------------------------------------------------
# create output for testing

test_that("Value box errors when no value is given", {
  # Note the path to the file is adjusted so that it works
  # when running shinytest2::test_app()
  source("../../server.R")

  # Expect an error if no value argument is given to value_box() function
  expect_error(value_box())
})
