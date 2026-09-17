test_that("get_example returns the path of a cached Zenodo dataset", {
  home_dir <- tempfile("dplab-home-")
  cache_dir <- file.path(home_dir, ".dplab", "datasets")
  dir.create(cache_dir, recursive = TRUE)
  old_home <- Sys.getenv("HOME", unset = NA_character_)
  on.exit({
    if (is.na(old_home)) {
      Sys.unsetenv("HOME")
    } else {
      Sys.setenv(HOME = old_home)
    }
    unlink(home_dir, recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(HOME = home_dir)

  example_object <- data.frame(value = 1)
  cache_file <- file.path(cache_dir, "example_object.rda")
  save(example_object, file = cache_file)

  expect_identical(
    get_example("123/files/example_object"),
    cache_file
  )
})

test_that("get_example validates Zenodo dataset identifiers", {
  expect_error(get_example(""))
  expect_error(get_example("123/files/"))
  expect_error(get_example("https://example.com/123/files/example"))
})

test_that("get_example caches downloads without changing directory", {
  original_wd <- getwd()
  home_dir <- tempfile("dplab-home-")
  old_home <- Sys.getenv("HOME", unset = NA_character_)
  dir.create(home_dir)
  on.exit({
    if (is.na(old_home)) {
      Sys.unsetenv("HOME")
    } else {
      Sys.setenv(HOME = old_home)
    }
    unlink(home_dir, recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(HOME = home_dir)

  downloaded_file <- NULL
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, quiet) {
      downloaded_file <<- destfile
      example_object <- list()
      save(example_object, file = destfile)
      0L
    },
    .package = "utils"
  )

  result <- get_example("123/files/example_object")
  cache_file <- file.path(home_dir, ".dplab", "datasets", "example_object.rda")

  expect_identical(getwd(), original_wd)
  expect_identical(result, cache_file)
  expect_identical(dirname(downloaded_file), dirname(cache_file))
  expect_true(file.exists(cache_file))
})

test_that("get_example does not download an existing dataset", {
  home_dir <- tempfile("dplab-home-")
  cache_dir <- file.path(home_dir, ".dplab", "datasets")
  dir.create(cache_dir, recursive = TRUE)
  old_home <- Sys.getenv("HOME", unset = NA_character_)
  on.exit({
    if (is.na(old_home)) {
      Sys.unsetenv("HOME")
    } else {
      Sys.setenv(HOME = old_home)
    }
    unlink(home_dir, recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(HOME = home_dir)

  cache_file <- file.path(cache_dir, "example_object.rda")
  writeBin(charToRaw("existing file"), cache_file)
  testthat::local_mocked_bindings(
    download.file = function(...) {
      fail("download.file should not be called for an existing dataset")
    },
    .package = "utils"
  )

  expect_identical(get_example("123/files/example_object"), cache_file)
})

test_that("get_example preserves dataset names that include an rda extension", {
  home_dir <- tempfile("dplab-home-")
  cache_dir <- file.path(home_dir, ".dplab", "datasets")
  dir.create(cache_dir, recursive = TRUE)
  old_home <- Sys.getenv("HOME", unset = NA_character_)
  on.exit({
    if (is.na(old_home)) {
      Sys.unsetenv("HOME")
    } else {
      Sys.setenv(HOME = old_home)
    }
    unlink(home_dir, recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(HOME = home_dir)

  cache_file <- file.path(cache_dir, "lymph_node_tiny_2.rda")
  writeBin(charToRaw("existing file"), cache_file)

  expect_identical(
    get_example("22705530/files/lymph_node_tiny_2.rda"),
    cache_file
  )
})

test_that("load_example loads the downloaded dataset into the caller environment", {
  dataset_file <- tempfile(fileext = ".rda")
  on.exit(unlink(dataset_file), add = TRUE)
  example_object <- data.frame(value = 1)
  save(example_object, file = dataset_file)

  environment <- new.env(parent = globalenv())
  environment$dataset_file <- dataset_file
  loaded_names <- with(environment, load_example(dataset_file))

  expect_identical(loaded_names, "example_object")
  expect_equal(environment$example_object, example_object)
})

test_that("load_example validates its path and target environment", {
  expect_error(load_example(""))
  expect_error(load_example(tempfile()))
  expect_error(load_example(tempfile(), envir = list()))
})
