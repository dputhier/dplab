# Tests for verbosity functions: set_verbosity, get_verbosity, print_msg

set_verbosity(0)

test_that("Checking set_verbosity() and get_verbosity()", {
  # Test setting verbosity to 0
  set_verbosity(0)
  expect_equal(get_verbosity(), 0)
  
  # Test setting verbosity to 1
  set_verbosity(1)
  expect_equal(get_verbosity(), 1)
  
  # Test that invalid verbosity values throw errors
  expect_error(set_verbosity(-1))
  expect_error(set_verbosity("not a number"))
  expect_error(set_verbosity(1.5))
  expect_error(set_verbosity(c(1, 2)))
})

test_that("print_msg executes without errors", {
  # Test that print_msg doesn't error with different verbosity levels
  set_verbosity(0)
  expect_no_error(print_msg("Hello world!", "INFO"))
  expect_no_error(print_msg("Debug message", "DEBUG"))
  
  set_verbosity(1)
  expect_no_error(print_msg("Hello world!", "INFO"))
  expect_no_error(print_msg("Debug message", "DEBUG"))
  
  set_verbosity(2)
  expect_no_error(print_msg("Hello world!", "INFO"))
  expect_no_error(print_msg("Debug message", "DEBUG"))
  
  # Test with default msg_type (INFO)
  expect_no_error(print_msg("Default message"))
})

test_that("print_msg handles error types correctly", {
  set_verbosity(1)
  
  # WARNING should not error but should produce output
  expect_no_error(print_msg("Warning message", "WARNING"))
  
})

