# Test for error case
testthat::test_that("transform_logical_yes_no function returns error for invalid input", {
  expect_error(transform_logical_yes_no(1:3, "en"), "Invalid input. Please provide a logical or character/factor vector.")
})

# Test for equality case
testthat::test_that("transform_logical_yes_no function returns same result for logical input", {
  x <- c(TRUE, FALSE, TRUE)
  expect_equal(as.character(transform_logical_yes_no(x, "en")), c("Yes", "No", "Yes"))
  expect_equal(as.character(transform_logical_yes_no(x, "fr")), c("Oui", "Non", "Oui"))
})
