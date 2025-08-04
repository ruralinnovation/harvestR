test_that("environment variables are set for testing", {
  skip_if_not(Sys.getenv("HARVEST_ACCOUNT_ID") != "", "HARVEST_ACCOUNT_ID not set")
  skip_if_not(Sys.getenv("HARVEST_PAT") != "", "HARVEST_PAT not set")
  
  expect_true(nchar(Sys.getenv("HARVEST_ACCOUNT_ID")) > 0)
  expect_true(nchar(Sys.getenv("HARVEST_PAT")) > 0)
  expect_false(startsWith(Sys.getenv("HARVEST_PAT"), "Bearer "))
})

test_that("get_table works with time_entries endpoint", {
  skip_if_not(Sys.getenv("HARVEST_ACCOUNT_ID") != "", "HARVEST_ACCOUNT_ID not set")
  skip_if_not(Sys.getenv("HARVEST_PAT") != "", "HARVEST_PAT not set")
  
  result <- get_table(table = 'time_entries', 
                     user = Sys.getenv('HARVEST_ACCOUNT_ID'), 
                     token = Sys.getenv('HARVEST_PAT'),
                     query = list(from = '2025-07-01', to = '2025-07-18'))
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
  expect_true("id" %in% names(result))
  expect_true("hours" %in% names(result))
})

test_that("get_table works with projects endpoint", {
  skip_if_not(Sys.getenv("HARVEST_ACCOUNT_ID") != "", "HARVEST_ACCOUNT_ID not set")
  skip_if_not(Sys.getenv("HARVEST_PAT") != "", "HARVEST_PAT not set")
  
  result <- get_table(table = 'projects', 
                     user = Sys.getenv('HARVEST_ACCOUNT_ID'), 
                     token = Sys.getenv('HARVEST_PAT'))
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
  expect_true("id" %in% names(result))
  expect_true("name" %in% names(result))
})

test_that("get_table works with users endpoint", {
  skip_if_not(Sys.getenv("HARVEST_ACCOUNT_ID") != "", "HARVEST_ACCOUNT_ID not set")
  skip_if_not(Sys.getenv("HARVEST_PAT") != "", "HARVEST_PAT not set")
  
  result <- get_table(table = 'users', 
                     user = Sys.getenv('HARVEST_ACCOUNT_ID'), 
                     token = Sys.getenv('HARVEST_PAT'))
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
  expect_true("id" %in% names(result))
  expect_true("first_name" %in% names(result))
})

test_that("get_table works with user_assignments endpoint", {
  skip_if_not(Sys.getenv("HARVEST_ACCOUNT_ID") != "", "HARVEST_ACCOUNT_ID not set")
  skip_if_not(Sys.getenv("HARVEST_PAT") != "", "HARVEST_PAT not set")
  
  result <- get_table(table = 'user_assignments', 
                     user = Sys.getenv('HARVEST_ACCOUNT_ID'), 
                     token = Sys.getenv('HARVEST_PAT'))
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
  expect_true("id" %in% names(result))
  expect_true("project.id" %in% names(result))
  expect_true("user.id" %in% names(result))
})

test_that("get_table works with users/{user_id}/project_assignments endpoint", {
  skip_if_not(Sys.getenv("HARVEST_ACCOUNT_ID") != "", "HARVEST_ACCOUNT_ID not set")
  skip_if_not(Sys.getenv("HARVEST_PAT") != "", "HARVEST_PAT not set")
  
  result <- get_table(table = 'users/3678483/project_assignments', 
                     user = Sys.getenv('HARVEST_ACCOUNT_ID'), 
                     token = Sys.getenv('HARVEST_PAT'))
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
  expect_true("id" %in% names(result))
  expect_true("project.id" %in% names(result))
  expect_true("is_project_manager" %in% names(result))
})

test_that("get_table works with users/me/project_assignments endpoint", {
  skip_if_not(Sys.getenv("HARVEST_ACCOUNT_ID") != "", "HARVEST_ACCOUNT_ID not set")
  skip_if_not(Sys.getenv("HARVEST_PAT") != "", "HARVEST_PAT not set")
  
  result <- get_table(table = 'users/me/project_assignments', 
                     user = Sys.getenv('HARVEST_ACCOUNT_ID'), 
                     token = Sys.getenv('HARVEST_PAT'))
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
  expect_true("id" %in% names(result))
  expect_true("project.id" %in% names(result))
  expect_true("is_project_manager" %in% names(result))
})

test_that("Bearer prefix is added automatically", {
  skip_if_not(Sys.getenv("HARVEST_ACCOUNT_ID") != "", "HARVEST_ACCOUNT_ID not set")
  skip_if_not(Sys.getenv("HARVEST_PAT") != "", "HARVEST_PAT not set")
  
  # Test with token without Bearer prefix
  result1 <- get_table(table = 'users', 
                      user = Sys.getenv('HARVEST_ACCOUNT_ID'), 
                      token = Sys.getenv('HARVEST_PAT'))
  
  # Test with token with Bearer prefix
  result2 <- get_table(table = 'users', 
                      user = Sys.getenv('HARVEST_ACCOUNT_ID'), 
                      token = paste("Bearer", Sys.getenv('HARVEST_PAT')))
  
  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
  expect_equal(nrow(result1), nrow(result2))
})