# Copyright (c) 2020, ETH Zurich


test_that("run_simulation works", {
  skip_on_cran()
  #get correct path or correct input objects
  #take data from case study 1 (Hagen et al. 2020)
  datapath <- system.file(file.path("extdata", "CaseStudy1"), package="gen3sis")
  config <- create_input_config(file.path(datapath,"config/config_rte.R"))
  #run only the latest time-steps
  config$gen3sis$general$start_time <- 5
  #re-set call of observer
  config$gen3sis$general$end_of_timestep_observer <- function(data, vars, config){}
  tmp_output <- tempdir()
  s <- run_simulation(config = config, 
                      landscape = file.path(datapath,"landscape"), output_directory = tmp_output)
  ref_summary <- readRDS(file.path(datapath, "reference_saves", "sgen3sis_summary.rds"))
  expect_true(all.equal(ref_summary, s$summary))
  expect_true(tools::md5sum(file.path(s$parameters$directories$output, "phy.nex")) == tools::md5sum(file.path(datapath, "reference_saves", "phy.nex")))
  expect_true(tools::md5sum(file.path(s$parameters$directories$output, "phy.txt")) == tools::md5sum(file.path(datapath, "reference_saves", "phy.txt")))
})
