# Tests for stats utility functions: print_stat, control_list

set_verbosity(0)

test_that("Just some check about print_stat", {
  set.seed(123)

  data <- rnorm(3)
  # print_stat should complete without error
  expect_no_error(
    print_stat(
      "Summary statistics for data",
      data,
      round_val = 2,
      msg_type = "WARNING"
    )
  )

  data <- rnorm(100)
  expect_no_error(
    print_stat(
      "Summary statistics for data",
      data,
      round_val = 1,
      msg_type = "WARNING"
    )
  )
  
  # Test that print_stat works with INFO type
  set_verbosity(1)
  data <- rnorm(10)
  expect_no_error(
    print_stat(
      "Test data",
      data,
      round_val = 2,
      msg_type = "INFO"
    )
  )

})

test_that("check control_list works correctly", {
  set_verbosity(0)

  set.seed(123)
  a <- sort(c(rnorm(100, mean=0), rnorm(100, mean=4)))
  names(a) <- paste0("gene_", 1:length(a))
  b <- names(a[sample(1:100, size = 10, replace = FALSE)])
  control <- control_list(a, b)

  expect_equal(length(control), length(b))
  expect_true(all(c("gene_24", "gene_66", "gene_8", "gene_62", "gene_40", "gene_93",
                     "gene_32", "gene_72", "gene_37", "gene_21") %in% control))
  expect_true(all(!b %in% control))
  expect_error(control_list(a, c("", "a")))
  expect_error(control_list(a, c("", "a")))
  expect_error(control_list(a, c(" ", "a")))
  expect_error(control_list(a, names(a)))
  expect_true(all(control_list(a, names(a)[1:10]) %in% c("gene_11", "gene_12", "gene_13", "gene_14",
                                                         "gene_15", "gene_16",
                                                         "gene_17", "gene_18", "gene_19", "gene_20")))
})
