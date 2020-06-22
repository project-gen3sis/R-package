# Copyright (c) 2020, ETH Zurich


test_that("run_simulation works", {
  skip_on_cran()
  #get correct path or correct input objects
  #take data from case study 1
  datapath <- system.file(file.path("extdata", "CaseStudy1"), package="gen3sis")
  config <- create_input_config(file.path(datapath,"config/config_rte.R"))
  #run only the latest timesteps
  config$gen3sis$general$start_time <- 5
  #re-set call of observer
  config$gen3sis$general$end_of_timestep_observer <- function(data, vars, config){}
  s <- run_simulation(config = config, 
                      landscape = file.path(datapath,"landscape"), output_directory = tempdir())
  expect_true(!is.null(s))
})
