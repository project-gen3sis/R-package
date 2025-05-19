# Copyright (c) 2020, ETH Zurich

base_dir <- file.path("/home/yogh/Documentos/projects/testing_gen3sis/test_files") # TODO set a dir test files within package

update_reference <- FALSE


# Test if the function identifies the missing config 
test_that("prepare_directories: Error on missing config file", {
  # With no config
  expect_error(prepare_directories(),
               "no config file provided!",
               fixed = TRUE)
  
  # With a config file that does not exist
  expect_error(prepare_directories(config_file = "not_exisiting.R"),
               "Config file does not exist!",
               fixed = TRUE)
})

# Test if the function identifies missing inputs 
test_that("prepare_directories: Error on non-existing input directory", {
  config_file <- file.path(base_dir,"configs","test_config.R") # TODO set a dir test files within package
  
  expect_error(
    prepare_directories(
      config_file = config_file,
      input_directory = "input_folder/does/not/exists"),
    "input directory does not exist!",
    fixed = TRUE
  )
})

# Test if the function creates the output directory
test_that("derive input/output directories from config path", {
  skip("skip for now")
  new_dirs <- list()
  #mockr::with_mock(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )
  config_file <- file.path(base_dir,"configs","test_config.R")
  dirs <- evaluate_promise(prepare_directories(config_file = config_file))
  expect_true(all(new_dirs %in% dirs$result))
  expect_true("data/input/tests_config_handling_0d" %in% dirs$result)
})
# TODO make it work  
# test_that("derive input/output directories from config path", {
#   new_dirs <- character()
#   
#   result <- mockr::with_mock(
#     `gen3sis2::dir.create` = function(new_dir, ...) {
#       new_dirs <<- c(new_dirs, new_dir)
#       TRUE # importante retornar TRUE, como faz o dir.create normalmente
#     },
#     {
#       config_file <- file.path(base_dir, "configs", "test_config.R")
#       evaluate_promise(prepare_directories(config_file = config_file))
#     }
#   )
#   
#   expected_results <- list(
#     input = base_dir,
#     output = file.path(base_dir, "outputs",create_input_config(config_file)$gen3sis$general$config_name),
#     output_plots = file.path(base_dir, "outputs", create_input_config(config_file)$gen3sis$general$config_name,"plots")
#   )
#   
#   expect_equal(expected_results,result$result) # TODO check if it is the correct ideia
#   
#   #expect_true("data/input/tests_config_handling_0d" %in% new_dirs) 
#   #expect_true(all(new_dirs %in% result$result)) # TODO is this necessary?
# })

# TODO default config does not exist anymore 
# test_that("prepare_directories: default config, input_directory given, output directory derived", {
#   skip("skip until config handling settled")
#   new_dirs <- list()
#   local_mock(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )
#   input_directory <- "data/input"
#   dirs <- evaluate_promise(prepare_directories(input_directory = input_directory))
#   expect_true(all(new_dirs %in% dirs$result))
#   expect_true("data/input" %in% dirs$result)
#   expect_true("data/output/default_config" %in% dirs$result)
# })

test_that("prepare_directories: config, input-, and output- directory given", {
  skip("skip for now")
  new_dirs <- list()
  local_mocked_bindings(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )
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

test_that("prepare_directories: config, input-, and output- directory given", {
  #skip("skip for now")
  #new_dirs <- list()
  #local_mocked_bindings(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )
  
  config_file <- file.path(base_dir,"configs","test_config.R")
  
  input_directory <- file.path(base_dir)
  output_directory <- file.path(base_dir, "outputs")
  
  expected_results <- list(
    input = input_directory,
    output = file.path(output_directory, create_input_config(config_file)$gen3sis$general$config_name),
    output_plots = file.path(output_directory, create_input_config(config_file)$gen3sis$general$config_name,"plots")
  )
  
  dirs <- evaluate_promise(prepare_directories(config_file = config_file,
                                               input_directory = input_directory,
                                               output_directory = output_directory))
  # TODO necessary?
  expect_true(all(expected_results %in% dirs$result,
                  expected_results$input %in% dirs$result,
                  expected_results$output %in% dirs$result,
                  expected_results$output_plots %in% dirs$result))
  #
  expect_equal(expected_results, dirs$result)
})

# TODO default config does not exist anymore 
# test_that("no user config provided, using default config" , {
#   skip("skip until devtools updated in docker image")
#   local_mock(dir.create = function(new_dir, ...) { } )
#   empty_config <- "data/config/tests_config_handling_0d/tests_run/config_empty.R"
#   dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
#   expect_known_output(create_input_config(empty_config, dirs$result),
#                       file = "data/output/reference_test_empty_config.txt",
#                       update = update_reference)
# })

test_that("partial user config provided" , {
  skip("skip until devtools updated in docker image")
  local_mock(dir.create = function(new_dir, ...) { } )
  empty_config <- "data/config/tests_config_handling_0d/tests_run/config_partial.R"
  dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
  expect_known_output(create_input_config(empty_config, dirs$result),
                      file = "data/output/reference_test_partial_config.txt",
                      update = update_reference)
})

test_that("full user config provided" , {
  skip("skip until devtools updated in docker image")
  local_mock(dir.create = function(new_dir, ...) { } )
  empty_config <- "data/config/tests_config_handling_0d/tests_run/config_complete.R"
  dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
  expect_known_output(create_input_config(empty_config, dirs$result),
                      file = "data/output/reference_test_complete_config.txt",
                      update = update_reference)
})

test_that("user config provided with extra values" , {
  skip("skip until devtools updated in docker image")
  local_mock(dir.create = function(new_dir, ...) { } )
  empty_config <- "data/config/tests_config_handling_0d/tests_run/config_additional_user_values.R"
  dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
  expect_known_output(create_input_config(empty_config, dirs$result),
                      file = "data/output/reference_test_additional_user_values.txt",
                      update = update_reference)
})

