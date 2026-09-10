# Tests for file utility functions: mkdir_p, make_tmp_file, count_lines

set_verbosity(0)

# -------------------------------------------------------------------------
# Tests for mkdir_p -------------------------------------------------------
# -------------------------------------------------------------------------

test_that("mkdir_p creates a new directory", {
  test_dir <- file.path(tempdir(), paste0("test_mkdir_", create_rand_str()))
  
  # Ensure directory doesn't exist
  expect_false(dir.exists(test_dir))
  
  # Create directory
  expect_no_error(mkdir_p(test_dir))
  
  # Check that directory now exists
  expect_true(dir.exists(test_dir))
  
  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

test_that("mkdir_p creates nested directories recursively", {
  test_dir <- file.path(tempdir(), 
                        paste0("test_mkdir_", create_rand_str()),
                        "level1",
                        "level2",
                        "level3")
  
  # Ensure directory doesn't exist
  expect_false(dir.exists(test_dir))
  
  # Create nested directories
  expect_no_error(mkdir_p(test_dir))
  
  # Check that directory now exists
  expect_true(dir.exists(test_dir))
  
  # Cleanup
  unlink(dirname(dirname(dirname(test_dir))), recursive = TRUE)
})

test_that("mkdir_p handles existing directory without error", {
  test_dir <- file.path(tempdir(), paste0("test_mkdir_", create_rand_str()))
  
  # Create directory first time
  dir.create(test_dir)
  expect_true(dir.exists(test_dir))
  
  # Call mkdir_p on existing directory - should not error
  expect_no_error(mkdir_p(test_dir))
  
  # Directory should still exist
  expect_true(dir.exists(test_dir))
  
  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

test_that("mkdir_p validates input path", {
  # Invalid path types should error
  expect_error(mkdir_p(123))
  expect_error(mkdir_p(c("path1", "path2")))
  expect_error(mkdir_p(""))
  expect_error(mkdir_p(NULL))
})


# -------------------------------------------------------------------------
# Tests for make_tmp_file -------------------------------------------------
# -------------------------------------------------------------------------

test_that("make_tmp_file creates a temporary file", {
  tmp_file <- make_tmp_file()
  
  # Check that file exists
  expect_true(file.exists(tmp_file))
  
  # Check that filename contains package name
  expect_match(basename(tmp_file), "^denlabutils_")
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("make_tmp_file uses custom prefix", {
  tmp_file <- make_tmp_file(prefix = "myprefix")
  
  # Check that file exists
  expect_true(file.exists(tmp_file))
  
  # Check that filename contains custom prefix
  expect_match(basename(tmp_file), "denlabutils_myprefix_")
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("make_tmp_file uses custom suffix", {
  tmp_file <- make_tmp_file(suffix = ".csv")
  
  # Check that file exists
  expect_true(file.exists(tmp_file))
  
  # Check that filename has correct suffix
  expect_match(tmp_file, "\\.csv$")
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("make_tmp_file uses custom package name", {
  tmp_file <- make_tmp_file(pkgname = "testpkg")
  
  # Check that file exists
  expect_true(file.exists(tmp_file))
  
  # Check that filename contains custom package name
  expect_match(basename(tmp_file), "^testpkg_")
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("make_tmp_file stores files in option when store=TRUE", {
  # Clear any existing temp files
  options(denlabutils_temp_files = NULL)
  
  tmp_file1 <- make_tmp_file(store = TRUE)
  tmp_file2 <- make_tmp_file(store = TRUE)
  
  # Check that files are stored in options
  stored_files <- getOption("denlabutils_temp_files")
  expect_true(tmp_file1 %in% stored_files)
  expect_true(tmp_file2 %in% stored_files)
  expect_equal(length(stored_files), 2)
  
  # Cleanup
  file.remove(tmp_file1, tmp_file2)
  options(denlabutils_temp_files = NULL)
})

test_that("make_tmp_file does not store files when store=FALSE", {
  # Clear any existing temp files
  options(denlabutils_temp_files = NULL)
  
  tmp_file <- make_tmp_file(store = FALSE)
  
  # Check that file is not stored in options
  stored_files <- getOption("denlabutils_temp_files")
  expect_true(is.null(stored_files) || !(tmp_file %in% stored_files))
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("make_tmp_file creates file in specified directory", {
  # Create a custom temp directory
  custom_dir <- file.path(tempdir(), paste0("custom_", create_rand_str()))
  dir.create(custom_dir)
  
  tmp_file <- make_tmp_file(dir = custom_dir)
  
  # Check that file is in the custom directory
  expect_equal(dirname(tmp_file), custom_dir)
  expect_true(file.exists(tmp_file))
  
  # Cleanup
  file.remove(tmp_file)
  unlink(custom_dir, recursive = TRUE)
})

test_that("make_tmp_file validates inputs", {
  # Invalid prefix
  expect_error(make_tmp_file(prefix = 123))
  expect_error(make_tmp_file(prefix = c("a", "b")))
  
  # Invalid pkgname
  expect_error(make_tmp_file(pkgname = 123))
  expect_error(make_tmp_file(pkgname = c("a", "b")))
  
  # Invalid suffix
  expect_error(make_tmp_file(suffix = 123))
  expect_error(make_tmp_file(suffix = c("a", "b")))
  
  # Invalid store
  expect_error(make_tmp_file(store = "TRUE"))
  expect_error(make_tmp_file(store = c(TRUE, FALSE)))
  
  # Invalid dir
  expect_error(make_tmp_file(dir = 123))
  expect_error(make_tmp_file(dir = c("a", "b")))
  expect_error(make_tmp_file(dir = "/nonexistent/directory/path"))
})


# -------------------------------------------------------------------------
# Tests for count_lines ---------------------------------------------------
# -------------------------------------------------------------------------

test_that("count_lines counts lines in a file", {
  # Create a test file with known number of lines
  tmp_file <- make_tmp_file(suffix = ".txt", store = FALSE)
  writeLines(c("line 1", "line 2", "line 3", "line 4", "line 5"), tmp_file)
  
  # Count lines
  n_lines <- count_lines(tmp_file)
  
  # Check result
  expect_equal(n_lines, 5)
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("count_lines handles empty file", {
  # Create an empty file
  tmp_file <- make_tmp_file(suffix = ".txt", store = FALSE)
  writeLines(character(0), tmp_file)
  
  # Count lines
  n_lines <- count_lines(tmp_file)
  
  # Check result
  expect_equal(n_lines, 0)
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("count_lines handles single line file", {
  # Create a file with one line
  tmp_file <- make_tmp_file(suffix = ".txt", store = FALSE)
  writeLines("single line", tmp_file)
  
  # Count lines
  n_lines <- count_lines(tmp_file)
  
  # Check result
  expect_equal(n_lines, 1)
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("count_lines handles large files", {
  # Create a file with many lines
  tmp_file <- make_tmp_file(suffix = ".txt", store = FALSE)
  writeLines(rep("test line", 15000), tmp_file)
  
  # Count lines
  n_lines <- count_lines(tmp_file)
  
  # Check result
  expect_equal(n_lines, 15000)
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("count_lines works with connection object", {
  # Create a test file
  tmp_file <- make_tmp_file(suffix = ".txt", store = FALSE)
  writeLines(c("line 1", "line 2", "line 3"), tmp_file)
  
  # Open connection and count lines
  con <- file(tmp_file, "r")
  n_lines <- count_lines(con)
  close(con)
  
  # Check result
  expect_equal(n_lines, 3)
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("count_lines handles files with blank lines", {
  # Create a file with blank lines
  tmp_file <- make_tmp_file(suffix = ".txt", store = FALSE)
  writeLines(c("line 1", "", "line 3", "", "", "line 6"), tmp_file)
  
  # Count lines
  n_lines <- count_lines(tmp_file)
  
  # Check result - should count blank lines too
  expect_equal(n_lines, 6)
  
  # Cleanup
  file.remove(tmp_file)
})

test_that("count_lines validates input", {
  # Invalid input types
  expect_error(count_lines(123))
  expect_error(count_lines(c("file1.txt", "file2.txt")))
  expect_error(count_lines(NULL))
  
  # Non-existent file
  expect_error(count_lines("/nonexistent/file.txt"))
})
