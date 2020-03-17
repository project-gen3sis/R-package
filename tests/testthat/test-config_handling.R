# Copyright (c) 2020, ETH Zurich

update_reference <- FALSE


test_that("prepare_directories: Error on missing config file and input directory", {
  skip("skip until confoig handling settled")
  expect_error(prepare_directories(),
               "invalid config/input_directory combinations:
       if no config file is specified the input directory must be provided.",
               fixed = TRUE)
})

test_that("prepare_directories: Error on non-existing config file", {
  expect_error(prepare_directories(config_file = "data/config/tests_config_handling_0d/tests_run/not_exisiting.R"),
               "config file does not exist!",
               fixed = TRUE)
})

test_that("prepare_directories: Error on non-existing input directory", {
  skip("skip until confoig handling settled")
  expect_error(prepare_directories(input_directory = "data/input_folder/does/not/exists"),
               "input directory does not exist!",
               fixed = TRUE)
})

test_that("derive input/output directories from config path", {
  skip("skip for now")
  new_dirs <- list()
  local_mock(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )
  config_file <- "data/config/tests_config_handling_0d/tests_run/config_empty.R"
  dirs <- evaluate_promise(prepare_directories(config_file = config_file))
  expect_true(all(new_dirs %in% dirs$result))
  expect_true("data/input/tests_config_handling_0d" %in% dirs$result)
})

test_that("prepare_directories: default config, input_directory given, output directory derived", {
  skip("skip until config handling settled")
  new_dirs <- list()
  local_mock(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )
  input_directory <- "data/input"
  dirs <- evaluate_promise(prepare_directories(input_directory = input_directory))
  expect_true(all(new_dirs %in% dirs$result))
  expect_true("data/input" %in% dirs$result)
  expect_true("data/output/default_config" %in% dirs$result)
})

test_that("prepare_directories: config, input-, and output- directory given", {
  skip("skip for now")
  new_dirs <- list()
  local_mock(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )
  config_file <- "data/config/tests_config_handling_0d/tests_run/config_empty.R"
  input_directory <- "data/input"
  output_directory <- "data/output"
  dirs <- evaluate_promise(prepare_directories(config_file = config_file,
                                               input_directory = input_directory,
                                               output_directory = output_directory))
  expect_true(all(new_dirs %in% dirs$result))
  expect_true("data/input" %in% dirs$result)
  expect_true("data/output/config_empty" %in% dirs$result)
})

test_that("no user config provided, using default config" , {
  skip("skip until devtools updated in docker image")
  local_mock(dir.create = function(new_dir, ...) { } )
  empty_config <- "data/config/tests_config_handling_0d/tests_run/config_empty.R"
  dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
  expect_known_output(create_config(empty_config, dirs$result),
                      file = "data/output/reference_test_empty_config.txt",
                      update = update_reference)
})

test_that("partial user config provided" , {
  skip("skip until devtools updated in docker image")
  local_mock(dir.create = function(new_dir, ...) { } )
  empty_config <- "data/config/tests_config_handling_0d/tests_run/config_partial.R"
  dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
  expect_known_output(create_config(empty_config, dirs$result),
                      file = "data/output/reference_test_partial_config.txt",
                      update = update_reference)
})

test_that("full user config provided" , {
  skip("skip until devtools updated in docker image")
  local_mock(dir.create = function(new_dir, ...) { } )
  empty_config <- "data/config/tests_config_handling_0d/tests_run/config_complete.R"
  dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
  expect_known_output(create_config(empty_config, dirs$result),
                      file = "data/output/reference_test_complete_config.txt",
                      update = update_reference)
})

test_that("user config provided with extra values" , {
  skip("skip until devtools updated in docker image")
  local_mock(dir.create = function(new_dir, ...) { } )
  empty_config <- "data/config/tests_config_handling_0d/tests_run/config_additional_user_values.R"
  dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
  expect_known_output(create_config(empty_config, dirs$result),
                      file = "data/output/reference_test_additional_user_values.txt",
                      update = update_reference)
})
