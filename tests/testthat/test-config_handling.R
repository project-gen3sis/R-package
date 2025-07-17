# Copyright (c) 2020, ETH Zurich
# Tests are organized per function, in the same order of appearance in config_handling.R
base_dir <- system.file(file.path("extdata/TestConfigs"), package="gen3sis2")

update_reference <- FALSE

# prepare_directories()
## Test if the function identifies the missing config 
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

## Test if the function identifies missing inputs 
test_that("prepare_directories: Error on non-existing input directory", {
  config_file <- file.path(base_dir,"TestConfig.R")
  
  expect_error(
    capture.output({
      prepare_directories(
        config_file = config_file,
        input_directory = "input_folder/does/not/exists")
    }),
    "input directory does not exist!",
    fixed = TRUE
  )
})


## Test if the function creates the output directory
test_that("derive input/output directories from config path", {
  expected_directories <- list(
    input = sub("/TestConfigs","",file.path(base_dir)),
    output = file.path(gsub("[Cc]onfig","output",base_dir),"TestConfig"),
    output_plots = file.path(gsub("[Cc]onfig","output",base_dir),"TestConfig","plots")
  )
  
  config_file <- file.path(base_dir,"TestConfig.R")
  dirs <- evaluate_promise(prepare_directories(config_file = config_file))
  
  expect_equal(dirs$result$input, expected_directories$input)
  expect_equal(dirs$result$output, expected_directories$output)
  expect_equal(dirs$result$output_plots, expected_directories$output_plots)
})

## Test if the function deals with user-determined directories
test_that("prepare_directories: config, input-, and output- directory given", {
  config_file <- file.path(base_dir,"TestConfig.R")
  
  input_directory <- gsub("TestConfigs","TestSpaces/geodynamic_spaces/raster",base_dir)
  output_directory <- file.path(tempdir(),"output")
  
  dirs <- evaluate_promise(prepare_directories(config_file = config_file,
                                               input_directory = input_directory,
                                               output_directory = output_directory))
  
  expect_equal(dirs$result$input, input_directory)
  expect_equal(dirs$result$output, file.path(output_directory,"TestConfig"))
  expect_equal(dirs$result$output_plots, file.path(output_directory,"TestConfig","plots"))
})

# create_input_config()
## Test if the returned objct is invisible
test_that("returns invisible object",{
  expect_invisible(create_input_config(file.path(base_dir,"TestConfig.R")))
})

## Test if the properly identifies and create the config for different inputs
test_that("returned object is gen3sis_config",{
  # NA
  config <- create_input_config()
  expect_s3_class(config, "gen3sis_config")
  
  # Existing file
  config <- create_input_config(file.path(base_dir,"TestConfig.R"))
  expect_s3_class(config, "gen3sis_config")
})

test_that("properly create input config",{
  # with config_file = NA
  config <- create_input_config()
  config$gen3sis$general$config_name <- NULL
  empty_config <- create_empty_config()
  empty_config$gen3sis$general$config_name <- NULL
  
  expect_equal(config,empty_config)
  
  # with existing config file
  config <- create_input_config(config_file = file.path(base_dir,"TestConfig.R"))
  expect_in(names(config), c("gen3sis", "user", "directories"))
})

## Test if function correctly atributes the config name
### with config_file = NA and config_name = NULL
test_that("properly writes config_name with config_file NA and config_name NULL",{
  # with config_file = NA and config_name = NULL
  fake_time <- as.POSIXct("2025-05-21 10:00:00")
  fake_sample <- 1313
  
  mockery::stub(create_input_config, 'Sys.time', fake_time)
  mockery::stub(create_input_config, 'sample', fake_sample)
  
  config <- create_input_config(config_name = NULL)
  
  expected_name <- paste0(format(fake_time, "%Y-%m-%d"), "-", 
                          formatC(fake_sample, digits = 4, flag = "0"))
  
  expect_equal(config$gen3sis$general$config_name, expected_name)
})

### in other scenarios
test_that("properly writes config_name",{
  # with config_file = NA and config_name defined
  config <- create_input_config(config_name = "RightName")
  expect_equal(config$gen3sis$general$config_name, "RightName")
  
  # with existing config file and config_name = NULL
  config <- create_input_config(config_file = file.path(base_dir,"TestConfig.R"))
  expect_equal(config$gen3sis$general$config_name, "TestConfig")
  
  # with existing config file and config_name defined
  config <- create_input_config(config_file = file.path(base_dir,"TestConfig.R"), config_name = "RightName")
  expect_equal(config$gen3sis$general$config_name, "RightName")
})

## Test wrong usages
test_that("wrong usage of create_input_config arguments",{
  # Config file does not exist
  expect_error(create_input_config(config_file = "this/file/is/a/lie.R"),
               "config file: this/file/is/a/lie.R does not exist")
  
  # config_name is not a string
  expect_error(create_input_config(config_name = c("this","is","not","a","string")),
               "config_name must be length 1 or NULL, length 5 provided instead.")
  expect_error(create_input_config(config_name = list("this","is","not","a","string")),
               "config_name must be length 1 or NULL, length 5 provided instead.")
  expect_error(create_input_config(config_name = 13),
               "config_name must be 'character' or NULL, 'numeric' object provided instead.")
  
})

# populate_config()
## Test if the function returns invisible object
test_that("returns teh correct object",{
  new_config <- create_empty_config()
  
  expect_invisible(populate_config(new_config, file.path(base_dir,"TestConfig.R")))
  
  y <- populate_config(new_config, file.path(base_dir,"TestConfig.R"))
  expect_s3_class(y, "gen3sis_config")
})

## Teste if the funcition returns 

# verify_config()
## Test if function correctly identifies a complete config
test_that("verify_config: complete config",{
  config <- create_input_config(file.path(base_dir,"TestConfig.R"))
  expect_true(verify_config(config))
})

## Test if function correctly identifies an incomplete config
test_that("verify_config: incomplete config",{
  config <- create_input_config(file.path(base_dir,"TestConfig.R"))
  
  config$gen3sis$general$config_name <- NULL
  config$gen3sis$initialization <- NULL
  config$gen3sis$speciation$divergence_threshold <- NULL
  config$gen3sis$dispersal$max_dispersal <- NULL
  
  expect_false(verify_config(config))
})

## Test if function correctly identifies a config with missing values
test_that("verify_config: config with missing values",{
  # Deleting values from the config
  config <- create_input_config(file.path(base_dir,"TestConfig.R"))
  
  config$gen3sis$general$config_name <- NULL
  config$gen3sis$initialization <- NULL
  config$gen3sis$speciation$divergence_threshold <- NULL
  config$gen3sis$dispersal$max_dispersal <- NULL
  
  expected_message <- paste0(
    "Missing settings in the configuration from the following categories:\n",
    "general\n",
    "- config_name\n",
    "initialization\n",
    "- initial_abundance\n",
    "- create_ancestor_species\n",
    "dispersal\n",
    "- max_dispersal\n",
    "speciation\n",
    "- divergence_threshold"
  )
  
  expect_message(verify_config(config),
                 expected_message)
  
  # With empty config all values are missing
  config <- create_empty_config()
  
  expected_message <- paste0(
    "These settings must be set in the configuration:\n",
    "general\n",
    "- random_seed\n",
    "- start_time\n",
    "- end_time\n",
    "- max_number_of_species\n",
    "- max_number_of_coexisting_species\n",
    "- end_of_timestep_observer\n",
    "- trait_names\n",
    "- environmental_ranges\n",
    "- verbose\n",
    "- config_name\n",
    "initialization\n",
    "- initial_abundance\n",
    "- create_ancestor_species\n",
    "dispersal\n",
    "- max_dispersal\n",
    "- get_dispersal_values\n",
    "speciation\n",
    "- divergence_threshold\n",
    "- get_divergence_factor\n",
    "mutation\n",
    "- apply_evolution\n",
    "ecology\n",
    "- apply_ecology"
  )
  
  expect_message(verify_config(config),
                 expected_message)
})

# write_config_skeleton()
## Test if the function correctly writes the skeleton
test_that("Correctly writes the skeleton",{
  withr::with_tempdir({
    # Should write the file
    write_config_skeleton("config_skeleton.R")
    
    # and it should be equal to the preset skeleton
    new_file <- file("preset.R", open = "w")
    writeLines(skeleton_config(), new_file)
    close(new_file)
    
    expect_equal(
      readLines("config_skeleton.R"),
      readLines("preset.R")
    )
    expect_true(file.exists("config_skeleton.R"))
  })
})

## Test if the overwriting feature
test_that("Correctly overwrites the file",{
  withr::with_tempdir({
    # Trying to create the file twice
    write_config_skeleton("config_skeleton.R")
    expect_warning(write_config_skeleton("config_skeleton.R", overwrite = FALSE),
                   "config_skeleton.R exists, file not written.")
    
    expect_true(write_config_skeleton("config_skeleton.R", overwrite = TRUE))
  })
})

## Test boolean returned values
test_that("boolean values of write_config_skeleton",{
  withr::with_tempdir({
    expect_true(write_config_skeleton("config_skeleton.R"))
    expect_false(
      suppressWarnings(write_config_skeleton("config_skeleton.R", overwrite = FALSE)))
    expect_true(write_config_skeleton("config_skeleton.R", overwrite = TRUE))
  })
})

## Testing bad usage
test_that("bad usage of write_config_skeleton",{
  withr::with_tempdir({
    # File name is not a length 1 string
    expect_error(write_config_skeleton(1),
                 "file_path must be a character string containing the path to write the skeleton.")
    expect_error(write_config_skeleton(c("file","path")),
                 "file_path must be a character string containing the path to write the skeleton.")
    expect_error(write_config_skeleton(TRUE),
                 "file_path must be a character string containing the path to write the skeleton.")
    expect_error(write_config_skeleton(data.frame(x=1,y=1)),
                 "file_path must be a character string containing the path to write the skeleton.")
    
    # Overwrite is not a length 1 boolean
    expect_error(write_config_skeleton(file_path = "config_skeleton.R", overwrite = c(TRUE, FALSE)),
                 "overwrite must be a logical value.")
    expect_error(write_config_skeleton(file_path = "config_skeleton.R", overwrite = 1),
                 "overwrite must be a logical value.")
    expect_error(write_config_skeleton(file_path = "config_skeleton.R", overwrite = data.frame(x=1,y=1)),
                 "overwrite must be a logical value.")
    expect_error(write_config_skeleton(file_path = "config_skeleton.R", overwrite = "yes, please"),
                 "overwrite must be a logical value.")
    
    # File extension is wrong
    expect_error(write_config_skeleton(file_path = "config_skeleton.txt"),
                 "File path must end with .R")
    expect_error(write_config_skeleton(file_path = "config_skeleton.weirdstuff"),
                 "File path must end with .R")
    expect_error(write_config_skeleton(file_path = "config_skeleton"),
                 "File path must end with .R")
  })
})


# Deprecated Tests
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
# 
# test_that("partial user config provided" , {
#   skip("skip until devtools updated in docker image")
#   local_mock(dir.create = function(new_dir, ...) { } )
#   empty_config <- "data/config/tests_config_handling_0d/tests_run/config_partial.R"
#   dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
#   expect_known_output(create_input_config(empty_config, dirs$result),
#                       file = "data/output/reference_test_partial_config.txt",
#                       update = update_reference)
# })
# 
# test_that("full user config provided" , {
#   skip("skip until devtools updated in docker image")
#   local_mock(dir.create = function(new_dir, ...) { } )
#   empty_config <- "data/config/tests_config_handling_0d/tests_run/config_complete.R"
#   dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
#   expect_known_output(create_input_config(empty_config, dirs$result),
#                       file = "data/output/reference_test_complete_config.txt",
#                       update = update_reference)
# })
# 
# test_that("user config provided with extra values" , {
#   skip("skip until devtools updated in docker image")
#   local_mock(dir.create = function(new_dir, ...) { } )
#   empty_config <- "data/config/tests_config_handling_0d/tests_run/config_additional_user_values.R"
#   dirs <- evaluate_promise(prepare_directories(config_file = empty_config))
#   expect_known_output(create_input_config(empty_config, dirs$result),
#                       file = "data/output/reference_test_additional_user_values.txt",
#                       update = update_reference)
# })

