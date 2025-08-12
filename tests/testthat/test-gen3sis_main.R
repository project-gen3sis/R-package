# Copyright (c) 2020, ETH Zurich
base_dir <- system.file(file.path("extdata"), package="gen3sis2") 

# run_simulation()
## Test if the simulation runs without errors
test_that("run_simulation works with config loaded in RAM", {
  skip_on_cran()
  config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))

  withr::with_tempdir({
    # geostatic
    ## raster
    input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
    capture.output({
      s <- run_simulation(
        config = config,
        space = input_variables,
        output_directory = "~/Documentos/projects/testing_gen3sis/dump"
      )
    }) |> suppressWarnings()
    
    ref_s <- readRDS(file.path(base_dir, "TestReferences", "Outputs","raster_geostat.rds"))
    
    expect_true(all.equal(names(s), names(ref_s)))
    expect_true(all.equal(ref_s$summary, s$summary))
    
    ## h3
    input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","h3") 
    capture.output({
      s <- run_simulation(
        config = config,
        space = input_variables,
        output_directory = getwd()
      )
    }) |> suppressWarnings()
    
    ref_s <- readRDS(file.path(base_dir, "TestReferences", "Outputs","h3_geostat.rds"))
    
    expect_true(all.equal(names(s), names(ref_s)))
    expect_true(all.equal(ref_s$summary, s$summary))
    
    ## points
    input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","points") 
    capture.output({
      s <- run_simulation(
        config = config,
        space = input_variables,
        output_directory = getwd()
      )
    }) |> suppressWarnings()
    
    ref_s <- readRDS(file.path(base_dir, "TestReferences", "Outputs","points_geostat.rds"))
    
    expect_true(all.equal(names(s), names(ref_s)))
    expect_true(all.equal(ref_s$summary, s$summary))
    
    # geodynamic
    ## raster
    input_variables <- file.path(base_dir,"TestSpaces","geodynamic_spaces","raster") 
    capture.output({
      s <- run_simulation(
        config = config,
        space = input_variables,
        output_directory = getwd()
      )
    }) |> suppressWarnings()
    
    ref_s <- readRDS(file.path(base_dir, "TestReferences", "Outputs","raster_geodyn.rds"))
    
    expect_true(all.equal(names(s), names(ref_s)))
    expect_true(all.equal(ref_s$summary, s$summary))
    
    ## h3
    input_variables <- file.path(base_dir,"TestSpaces","geodynamic_spaces","h3") 
    capture.output({
      s <- run_simulation(
        config = config,
        space = input_variables,
        output_directory = getwd()
      )
    }) |> suppressWarnings()
    
    ref_s <- readRDS(file.path(base_dir, "TestReferences", "Outputs","h3_geodyn.rds"))
    
    expect_true(all.equal(names(s), names(ref_s)))
    expect_true(all.equal(ref_s$summary, s$summary))
    
    ## points
    input_variables <- file.path(base_dir,"TestSpaces","geodynamic_spaces","points") 
    capture.output({
      s <- run_simulation(
        config = config,
        space = input_variables,
        output_directory = getwd()
      )
    }) |> suppressWarnings()
    
    ref_s <- readRDS(file.path(base_dir, "TestReferences", "Outputs","points_geodyn.rds"))
    
    expect_true(all.equal(names(s), names(ref_s)))
    expect_true(all.equal(ref_s$summary, s$summary))
  })
  #expect_true(tools::md5sum(file.path(s$parameters$directories$output, "phy.nex")) == tools::md5sum(file.path(datapath, "reference_saves", "phy.nex")))
})

## Test if the simulation runs with config loaded from file
test_that("run_simulation works with config loaded from file", {
  skip_on_cran()
  
  withr::with_tempdir({
    input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
    
    capture.output({
      s <- run_simulation(
        config = file.path(base_dir, "TestConfigs/TestConfig.R"),
        space = input_variables,
        output_directory = getwd()
      )
    }) |> suppressWarnings()
    
    
    ref_s <- readRDS(file.path(base_dir, "TestReferences", "Outputs","raster_geostat.rds"))
    
    expect_true(all.equal(names(s), names(ref_s)))
    expect_true(all.equal(ref_s$summary, s$summary))
  })
  
  
  #expect_true(tools::md5sum(file.path(s$parameters$directories$output, "phy.nex")) == tools::md5sum(file.path(datapath, "reference_saves", "phy.nex")))
})

## Tests for simulation saving
test_that("run_simulation saves correctly",{
  skip_on_cran()
  config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
  
  input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
  
  withr::with_tempdir({
    # For saving all
    capture.output({
      s <- run_simulation(config = config,
                          space = input_variables, 
                          output_directory = getwd(),
                          save_state = "all")
    }) |> suppressWarnings()
    
    expected_files <- c("val_t_0.rds", "val_t_1.rds", "val_t_2.rds", "val_t_3.rds")
    got_files <- list.files(file.path(getwd(),"TestConfig","val"))
    unlink(list.files(file.path(getwd(),"TestConfig","val"),full.names = T))
    
    expect_true(all(expected_files %in% got_files))
    
    # For saving some
    capture.output({
      s <- run_simulation(config = config,
                          space = input_variables, 
                          output_directory = getwd(),
                          save_state = c(1,3))
    }) |> suppressWarnings()
    
    expected_files <- c("val_t_1.rds", "val_t_3.rds")
    got_files <- list.files(file.path(getwd(),"TestConfig","val"))
    unlink(list.files(file.path(getwd(),"TestConfig","val"),full.names = T))
    
    expect_true(all(expected_files %in% got_files))
    
    # For saving last
    capture.output({
      s <- run_simulation(config = config,
                          space = input_variables, 
                          output_directory = getwd(),
                          save_state = "last")
    }) |> suppressWarnings()
    
    expected_files <- c("val_t_0.rds")
    got_files <- list.files(file.path(getwd(),"TestConfig","val"))
    unlink(list.files(file.path(getwd(),"TestConfig","val"),full.names = T))
    
    expect_true(all(expected_files %in% got_files))
  })
})

## Test for simulation restarting
test_that("simulation restart properly",{
  skip_on_cran()
  config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
  
  input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 

  withr::with_tempdir({
    # For saving all
    capture.output({
      s <- run_simulation(config = config,
                          space = input_variables, 
                          output_directory = getwd(),
                          save_state = "all")
    }) |> suppressWarnings()
    
    # Restart simulation from 3
    expect_message(
      capture.output({
        run_simulation(config = config,
                       space = input_variables, 
                       output_directory = getwd(),
                       save_state = NA,
                       timestep_restart = 3)
      }) |> suppressWarnings(),
      regexp = "\\[!] Restarting at time-step: 3\\s*$"
    ) 
    
    # Restart simulation from 2
    expect_message(
      capture.output({
        run_simulation(config = config,
                       space = input_variables, 
                       output_directory = getwd(),
                       save_state = NA,
                       timestep_restart = 2)
      }) |> suppressWarnings(),
      regexp = "\\[!] Restarting at time-step: 2\\s*$"
    )
    
    # Restart simulation from 1
    expect_message(
      capture.output({
        run_simulation(config = config,
                       space = input_variables, 
                       output_directory = getwd(),
                       save_state = NA,
                       timestep_restart = 1)
      }) |> suppressWarnings(),
      regexp = "\\[!] Restarting at time-step: 1\\s*$"
    )
  })
})

## Tests for config bad usage
test_that("wrong config usage",{
  skip_on_cran()
  config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
  
  input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
  
  withr::with_tempdir({
    # config = NA # TODO these wont work because the conditionals at the beginning of the function are redundant
    # expect_error({
    #   capture.output({
    #     run_simulation(config = NA,
    #                    space = input_variables, 
    #                    output_directory = getwd(),
    #                    save_state = "all")
    #   }) |> suppressWarnings()
    # }, "please provide either a config file or a config object")
    # 
    
    # config = numeric
    # expect_error({
    #   capture.output({
    #     run_simulation(config = 2,
    #                    space = input_variables,
    #                    output_directory = getwd(),
    #                    save_state = "all")
    #   }) |> suppressWarnings()
    # }, "this is not a known config, please provide either a config file or a config object")

    # with broken config
    config$gen3sis$initialization <- NULL
    
    expect_error({
      capture.output({
        run_simulation(config = config,
                       space = input_variables, 
                       output_directory = getwd(),
                       save_state = NA)
      }) |> suppressWarnings()
    }, "config verification failed")
  })
})

## Tests for save_state bad usage 
test_that("wrong save_state usage",{
  skip_on_cran()
  config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
  
  input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
  
  withr::with_tempdir({
    expect_warning(
      capture.output({
        run_simulation(config = config,
                       space = input_variables, 
                       output_directory = getwd(),
                       save_state = "non-sense value")
      }),
      "save_state must be either NA, 'all', 'last' or a numeric vector"
    )
    
    expect_warning(
      capture.output({
        run_simulation(config = config,
                       space = input_variables, 
                       output_directory = getwd(),
                       save_state = c(-1,3))
      }),
      "save_state negative timesteps are meaningless, simulation will not save."
    )
  })
})

# Test for timestep_restart bad usage
test_that("wrong timestep_restart usage",{
  skip_on_cran()
  config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
  input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
  
  withr::with_tempdir({
    suppressWarnings({
      s <- run_simulation(config = config,
                          space = input_variables, 
                          output_directory = getwd(),
                          save_state = "all")
    }) |> capture.output()
    
    expect_message(
      suppressWarnings({
        run_simulation(config = config,
                       space = input_variables, 
                       output_directory = getwd(),
                       save_state = NA,
                       timestep_restart = 0)
      }) |> capture.output(),
      regexp = "\\[!] Simulation already completed\\s*$"
    )
  })
  
  # TODO test for starting from a non-existing timestep
})

## Test for call_observer
test_that("run_simulation call_observer works",{
  skip_on_cran()
  config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
  input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
  
  withr::with_tempdir({
    observer_counter <- 0
    mockery::stub(where= run_simulation, 
                  what = "call_main_observer", 
                  how = function(data, vars, config) {
                    end_of_timestep_seed <- .GlobalEnv$.Random.seed
                    config$gen3sis$general$end_of_timestep_observer(data, vars, config)
                    .GlobalEnv$.Random.seed <- end_of_timestep_seed 
                    
                    observer_counter <<- observer_counter + 1
                  })
    
    # call_observer = "all"
    observer_counter <- 0
    suppressWarnings({
      s <- run_simulation(config = config,
                          space = input_variables, 
                          output_directory = getwd(),
                          save_state = NA,
                          verbose=0,
                          call_observer = "all")
    }) |> capture.output()
    
    expect_equal(observer_counter, 5)
    
    # call_observer = NA
    observer_counter <- 0
    suppressWarnings({
      s <- run_simulation(config = config,
                          space = input_variables, 
                          output_directory = getwd(),
                          save_state = NA,
                          verbose=0,
                          call_observer = NA)
    }) |> capture.output()
    
    expect_equal(observer_counter, 2)
    
    # call_observer = 1
    observer_counter <- 0
    suppressWarnings({
      s <- run_simulation(config = config,
                          space = input_variables, 
                          output_directory = getwd(),
                          save_state = NA,
                          verbose=0,
                          call_observer = 1)
    }) |> capture.output()
    
    expect_equal(observer_counter, 3)
  })
})

## Tests for species limit
test_that("simulation loop breaks if the species limits are reached",{
  skip_on_cran()
  
  # set species limit to 2
  config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
  config$gen3sis$general$max_number_of_species <- 2
  
  input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
  
  withr::with_tempdir({
      the_function_said <- capture.output({
        s <- run_simulation(config = config,
                       space = input_variables, 
                       output_directory = getwd(),
                       save_state = NA,
                       verbose=0,
                       call_observer = NA)
      }) |> suppressWarnings()
      
      expect_true("[!] Max number of species reached, breaking loop " %in% the_function_said)
  })
  
  # set species limit to 2
  config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
  config$gen3sis$general$max_number_of_coexisting_species <- 2
  
  input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
  
  withr::with_tempdir({
    the_function_said <- capture.output({
      s <- run_simulation(config = config,
                          space = input_variables, 
                          output_directory = getwd(),
                          save_state = NA,
                          verbose=0,
                          call_observer = NA)
    }) |> suppressWarnings()
    
    expect_true("[!] Max number of coexisting species reached, breaking loop " %in% the_function_said)
  })
})

## Tests for function verbose
# TODO rework verbose tests
# test_that("run_simulation verbose works",{
#   skip_on_cran() # TODO necessary?
#   config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
#   input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
#   
#   withr::with_tempdir({
#     # verbose = 0 
#     expected_console <- c(
#       "Config found: using config object "                                                                              
#       ,"space found: /home/yogh/Documentos/gitRepos/gen3sis_rf/inst/extdata/TestSpaces/geostatic_spaces/raster "
#       ,paste0("Output directory is: ",getwd(),"/TestConfig ")                                                
#       ,""                                                                                                                
#       ,"Using config: TestConfig "                                                                                       
#       ,"[1] \"i 1 yls 14 n_sites 2\""                                                                                    
#       ,"[1] \"i 2 yls -14 n_sites 3\""                                                                                   
#       ,"[1] \"i 3 yls -14 n_sites 3\""                                                                                   
#       ,"[1] \"i 4 yls 14 n_sites 2\""                                                                                    
#       ,"[1] \"i 5 yls -14 n_sites 3\""                                                                                   
#       ,"Simulation finished. All OK "    
#     )
#     
#     suppressWarnings({
#       s <- run_simulation(config = config,
#                           space = input_variables, 
#                           output_directory = getwd(),
#                           save_state = NA,
#                           verbose=0)
#     }) |> capture.output() -> got_console
#     
#     expect_equal(got_console, expected_console)
#     
#     # verbose = 1
#     expected_console <- c(
#       "Config found: using config object "                                                                              
#       ,"space found: /home/yogh/Documentos/gitRepos/gen3sis_rf/inst/extdata/TestSpaces/geostatic_spaces/raster "
#       ,paste0("Output directory is: ",getwd(),"/TestConfig ")                                                
#       ,""                                                                                                                
#       ,"Using config: TestConfig "
#       ,"--- Initializing --- "  
#       ,"[1] \"i 1 yls 14 n_sites 2\""                                                                                    
#       ,"[1] \"i 2 yls -14 n_sites 3\""                                                                                   
#       ,"[1] \"i 3 yls -14 n_sites 3\""                                                                                   
#       ,"[1] \"i 4 yls 14 n_sites 2\""                                                                                    
#       ,"[1] \"i 5 yls -14 n_sites 3\""
#       ,"--- Running simulation --- "                                                                                     
#       ,"step = 3 , species alive = 5 , species total = 5 "                                                               
#       ,"step = 2 , species alive = 5 , species total = 5 "                                                               
#       ,"step = 1 , species alive = 5 , species total = 5 "                                                               
#       ,"step = 0 , species alive = 5 , species total = 5 "    
#       ,"Simulation finished. All OK "
#     )
#     
#     suppressWarnings({
#       s <- run_simulation(config = config,
#                           space = input_variables, 
#                           output_directory = getwd(),
#                           save_state = NA,
#                           verbose=1)
#     }) |> capture.output() -> got_console
#     
#     ## simulation time varies
#     expect_equal(got_console[-18], expected_console)
#     
#     # verbose = 2
#     expected_console <- c(
#        "Config found: using config object "                                                                              
#       ,"space found: /home/yogh/Documentos/gitRepos/gen3sis_rf/inst/extdata/TestSpaces/geostatic_spaces/raster "
#       ,paste0("Output directory is: ",getwd(),"/TestConfig ")                                                  
#       ,""                                                                                                                
#       ,"Using config: TestConfig "                                                                                       
#       ,"--- Initializing --- "                                                                                           
#       ,"[1] \"i 1 yls 14 n_sites 2\""                                                                                    
#       ,"[1] \"i 2 yls -14 n_sites 3\""                                                                                   
#       ,"[1] \"i 3 yls -14 n_sites 3\""                                                                                   
#       ,"[1] \"i 4 yls 14 n_sites 2\""                                                                                    
#       ,"[1] \"i 5 yls -14 n_sites 3\""                                                                                   
#       ,"--- Running simulation --- "                                                                                     
#       ,"loop setup "                                                                                                     
#       ,"speciation "                                                                                                     
#       ,"dispersal "                                                                                                      
#       ,"evolution "                                                                                                      
#       ,"ecology "                                                                                                        
#       ,"end of loop updates "                                                                                            
#       ,"step = 3 , species alive = 5 , species total = 5 "                                                               
#       ,"--"                                                                                                              
#       ,"loop setup "                                                                                                     
#       ,"speciation "                                                                                                     
#       ,"dispersal "                                                                                                      
#       ,"evolution "                                                                                                      
#       ,"ecology "                                                                                                        
#       ,"end of loop updates "                                                                                            
#       ,"step = 2 , species alive = 5 , species total = 5 "                                                               
#       ,"--"                                                                                                              
#       ,"loop setup "                                                                                                     
#       ,"speciation "                                                                                                     
#       ,"dispersal "                                                                                                      
#       ,"evolution "                                                                                                      
#       ,"ecology "                                                                                                        
#       ,"end of loop updates "                                                                                            
#       ,"step = 1 , species alive = 5 , species total = 5 "                                                               
#       ,"--"                                                                                                              
#       ,"loop setup "                                                                                                     
#       ,"speciation "                                                                                                     
#       ,"dispersal "                                                                                                      
#       ,"evolution "                                                                                                      
#       ,"ecology "                                                                                                        
#       ,"end of loop updates "                                                                                            
#       ,"step = 0 , species alive = 5 , species total = 5 "                                                               
#       ,"--"                                                                                                              
#       ,"Simulation finished. All OK "
#     )
#     
#     suppressWarnings({
#       s <- run_simulation(config = config,
#                           space = input_variables, 
#                           output_directory = getwd(),
#                           save_state = NA,
#                           verbose=2)
#     }) |> capture.output() -> got_console
#     
#     ## simulation time varies
#     expect_equal(got_console[-46], expected_console)
#     
#     # verbose = 3
#     expected_console <- c(
#       "Config found: using config object "                                                                              
#       ,"space found: /home/yogh/Documentos/gitRepos/gen3sis_rf/inst/extdata/TestSpaces/geostatic_spaces/raster "
#       ,paste0("Output directory is: ",getwd(),"/TestConfig ")                                                  
#       ,""                                                                                                                
#       ,"Using config: TestConfig "                                                                                       
#       ,"--- Initializing --- "                                                                                           
#       ,"[1] \"i 1 yls 14 n_sites 2\""                                                                                    
#       ,"[1] \"i 2 yls -14 n_sites 3\""                                                                                   
#       ,"[1] \"i 3 yls -14 n_sites 3\""                                                                                   
#       ,"[1] \"i 4 yls 14 n_sites 2\""                                                                                    
#       ,"[1] \"i 5 yls -14 n_sites 3\""                                                                                   
#       ,"--- Running simulation --- "
#       ,"loop setup "
#       ,"speciation "
#       ,"entering speciation module "
#       ,"exiting speciation module "
#       ,"dispersal "
#       ,"entering dispersal module "
#       ,"exiting dispersal module "
#       ,"evolution "
#       ,"entering mutation module "
#       ,"exiting mutation module "
#       ,"ecology "
#       ,"entering ecology module @ time 3 "
#       ,"exiting ecology module @ time 3 "
#       ,"end of loop updates "
#       ,"step = 3 , species alive = 5 , species total = 5 "
#       ,"--"
#       ,"loop setup "
#       ,"speciation "
#       ,"entering speciation module "
#       ,"exiting speciation module "
#       ,"dispersal "
#       ,"entering dispersal module "
#       ,"exiting dispersal module "
#       ,"evolution "
#       ,"entering mutation module "
#       ,"exiting mutation module "
#       ,"ecology "
#       ,"entering ecology module @ time 2 "
#       ,"exiting ecology module @ time 2 "
#       ,"end of loop updates "
#       ,"step = 2 , species alive = 5 , species total = 5 "
#       ,"--"
#       ,"loop setup "
#       ,"speciation "
#       ,"entering speciation module "
#       ,"exiting speciation module "
#       ,"dispersal "
#       ,"entering dispersal module "
#       ,"exiting dispersal module "
#       ,"evolution "
#       ,"entering mutation module "
#       ,"exiting mutation module "
#       ,"ecology "
#       ,"entering ecology module @ time 1 "
#       ,"exiting ecology module @ time 1 "
#       ,"end of loop updates "
#       ,"step = 1 , species alive = 5 , species total = 5 "
#       ,"--"
#       ,"loop setup "
#       ,"speciation "
#       ,"entering speciation module "
#       ,"exiting speciation module "
#       ,"dispersal "
#       ,"entering dispersal module "
#       ,"exiting dispersal module "
#       ,"evolution "
#       ,"entering mutation module "
#       ,"exiting mutation module "
#       ,"ecology "
#       ,"entering ecology module @ time 0 "
#       ,"exiting ecology module @ time 0 "
#       ,"end of loop updates "
#       ,"step = 0 , species alive = 5 , species total = 5 "
#       ,"--"
#       ,"Simulation finished. All OK "
#     )
#     
#     
#     suppressWarnings({
#       s <- run_simulation(config = config,
#                           space = input_variables, 
#                           output_directory = getwd(),
#                           save_state = NA,
#                           verbose=3)
#     }) |> capture.output() -> got_console
#     
#     ## simulation time varies
#     expect_equal(got_console[-78], expected_console)
#   })
# })

# Deprecated tests
# test_that("run_simulation works", {
#   skip_on_cran()
#   #get correct path or correct input objects
#   #take data from case study 1 (Hagen et al. 2020)
#   datapath <- system.file(file.path("extdata", "CaseStudy1"), package="gen3sis")
#   config <- create_input_config(file.path(datapath,"config/config_rte.R"))
#   #run only the latest time-steps
#   config$gen3sis$general$start_time <- 5
#   #re-set call of observer
#   config$gen3sis$general$end_of_timestep_observer <- function(data, vars, config){}
#   tmp_output <- tempdir()
#   
#   # TODO find an alternative to use spaces.rds
#   # spac3tools::space_to_space(dir_input = file.path(datapath,"space"),
#   #                                duration = list(from = 139, to = 0, by = -1, unit = "Ma"))
#   
#   
#   s <- run_simulation(config = config,
#                       space = file.path(datapath,"space"), output_directory = tmp_output)
#   ref_summary <- readRDS(file.path(datapath, "reference_saves", "sgen3sis_summary.rds"))
#   expect_true(all.equal(ref_summary, s$summary))
#   expect_true(tools::md5sum(file.path(s$parameters$directories$output, "phy.nex")) == tools::md5sum(file.path(datapath, "reference_saves", "phy.nex")))
# })
