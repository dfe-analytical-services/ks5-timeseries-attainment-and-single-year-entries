# Unit tests are easy to write and quick to run, make use of them where you can
# For more information, look at the testthat package documentation.
# -----------------------------------------------------------------------------
# create output for testing

output <- in_line_nav_link("lt", "tl")

test_that("outputs are as expected", {
  expect_equal(
    class(output)[1],
    "html"
  )

  expect_equal(
    class(output)[2],
    "character"
  )
})
