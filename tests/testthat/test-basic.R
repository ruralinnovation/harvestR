test_that("package loads correctly", {
  expect_no_error(library(harvestR))
})

test_that("basic functions exist", {
  expect_true(exists("get_table"))
  expect_true(exists("get_request"))
  expect_true(exists("check_date_format"))
})