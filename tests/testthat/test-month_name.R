# Test month_name function
testthat::test_that("month_name function works correctly", {
  # Test for numeric to month name transformation in English
  expect_equal(as.character(month_name(1, "en")), "January")
  # Test for numeric to month name transformation in French
  expect_equal(as.character(month_name(1, "fr")), "Janvier")
  # Test for numeric to month name transformation in German
  expect_equal(as.character(month_name(1, "de")), "Januar")
})
