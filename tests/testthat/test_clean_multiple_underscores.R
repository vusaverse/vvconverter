testthat::test_that("clean_multiple_underscores function works correctly", {
  result <- clean_multiple_underscores("hello___world")
  expect_equal(result, "hello_world")
  
  result <- clean_multiple_underscores("this__is__a__test")
  expect_equal(result, "this_is_a_test")
  
  result <- clean_multiple_underscores("no_underscores")
  expect_equal(result, "no_underscores")
  
  result <- clean_multiple_underscores("multiple_____underscores")
  expect_equal(result, "multiple_underscores")
})
