# Tests for validation functions: check_this_var, check_this_file

set_verbosity(0)

test_that("check_named_list validates names, NULL, and empty lists", {
  expect_silent(check_named_list(list()))
  expect_silent(check_named_list(list(first = 1, second = 2)))
  expect_silent(check_named_list(NULL, null_accepted = TRUE))

  expect_error(check_named_list(NULL))
  expect_error(check_named_list(1))
  expect_error(check_named_list(list(1, 2)))
  expect_error(check_named_list(
    structure(list(1, 2), names = c("first", ""))
  ))
  expect_error(check_named_list(
    structure(list(1, 2), names = c("first", "first"))
  ))
  expect_error(check_named_list(list(), empty_accepted = FALSE))
})

test_that("check_this_var function works correctly", {
  
  set_verbosity(0)

  # Test NULL input - should throw an error
  expect_error(check_this_var(NULL))

  # Test length > 1 - should throw an error
  expect_error(check_this_var(x=c("bla", "foo"), type="int", null_accepted = TRUE))

  # Test NA input - should throw an error
  expect_error(check_this_var(NA))

  # Test NaN input - should throw an error
  x <- NaN
  expect_error(check_this_var(x))

  # Test Infinite input - should throw an error
  x <- Inf
  expect_error(check_this_var(x))

  # Test empty string input - should throw an error
  x <- ""
  expect_error(check_this_var(x))

  # Test character input - should work
  x <- "hello"
  expect_silent(check_this_var(x, type = "char"))

  # Test numeric input - should work
  expect_silent(check_this_var(42, type = "num"))

  # Test logical input - should work
  expect_silent(check_this_var(TRUE, type = "bool"))

  # Test integer input - should work
  expect_silent(check_this_var(5L, type = "int"))

  # Test integer as numeric - should work
  expect_silent(check_this_var(5, type = "int"))

  # Test unknown format input - should throw an error
  expect_error(check_this_var("unknown", type = "unknown"))

  # Test character input with NaN - should throw an error
  x <- NaN
  expect_error(check_this_var(x, type = "char"))

  # Test vector with NA - should throw an error (length > 1)
  x <- c(1, NA, 3)
  expect_error(check_this_var(x, type = "num"))

  # Test logical TRUE - should work
  x <- TRUE
  expect_silent(check_this_var(x, type = "bool"))
  
  # Test NULL with null_accepted = TRUE - should work
  expect_silent(check_this_var(NULL, type = "char", null_accepted = TRUE))

})

test_that("check_range validates inclusive numeric ranges", {
  expect_silent(check_range(0, min = 0, max = 1))
  expect_silent(check_range(1, min = 0, max = 1))
  expect_silent(check_range(0.5, min = 0, max = 1))

  expect_error(check_range(-0.1, min = 0, max = 1))
  expect_error(check_range(1.1, min = 0, max = 1))
  expect_error(check_range(0.5, min = 1, max = 0))
  expect_error(check_range("0.5", min = 0, max = 1))
  expect_error(check_range(0.5, min = NA_real_, max = 1))
  expect_error(check_range(c(0.25, 0.75), min = 0, max = 1))
})


test_that("check_this_file function works correctly", {

  set_verbosity(0)

  expect_error(check_this_file(NULL))

  temp_existing_file_write <- tempfile(fileext = ".txt")
  out <- file.create(temp_existing_file_write)
  expect_error(check_this_file(temp_existing_file_write, mode = "write"))

  expect_silent(check_this_file(temp_existing_file_write, mode = "write", force = TRUE))

  temp_new_file_write <- tempfile(fileext = ".txt")
  expect_no_error(check_this_file(temp_new_file_write, mode = "write"))

})
