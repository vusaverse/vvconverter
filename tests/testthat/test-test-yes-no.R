# Test translate_yes_no function
testthat::test_that("translate_yes_no function works correctly", {
  expect_equal(tolower(translate_yes_no("yes", source_language = "en")), "yes")
  expect_equal(tolower(translate_yes_no("no", source_language = "en")), "no")
  expect_equal(tolower(translate_yes_no("ja", source_language = "nl")), "yes")
  expect_equal(tolower(translate_yes_no("nee", source_language = "nl")), "no")
})


# Test test_yes_no function
testthat::test_that("test_yes_no function works correctly", {
  expect_true(test_yes_no("yes"))
  expect_true(test_yes_no("no"))
  expect_true(test_yes_no("y"))
  expect_true(test_yes_no("n"))
  expect_false(test_yes_no("maybe"))
})
