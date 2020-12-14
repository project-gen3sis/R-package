# Copyright (c) 2020, ETH Zurich

#' @title gen3sis: General Engine for Eco-Evolutionary Simulations
#' @name gen3sis
#' @description Contains an engine for spatially-explicit eco-evolutionary mechanistic models with a modular implementation and several support functions. It allows exploring the consequences of ecological and macroevolutionary processes across realistic or theoretical spatio-temporal landscapes on biodiversity patterns as a general term.
#' @references O. Hagen, B. Flück, F. Fopp, J.S. Cabral, F. Hartig, M. Pontarp, T.F. Rangel, L. Pellissier. (2020). gen3sis: the GENeral Engine for Eco-Evolutionary SImulationS on the origins of biodiversity. (in prep)
#' @details Gen3sis is implemented in a mix of R and C++ code, and wrapped into an R-package. All high-level functions that the user may interact with are written in R, and are documented via the standard R / Roxygen help files for R-packages. Runtime-critical functions are implemented in C++ and coupled to R via the Rcpp framework. Additionally, the package provides several convenience functions to generate input data, configuration files and plots, as well as tutorials in the form of vignettes that illustrate how to declare models and run simulations.
#' @seealso \code{\link{create_input_config}}   \code{\link{create_input_landscape}}  \code{\link{run_simulation}}  \code{\link{plot_summary}}
#' @keywords programming IO iteration methods utilities
#' @concept gen3sis modeling eco-evolutionary macroevolution macroecology mechanisms
#' @examples
#' \dontrun{
#' 
#' # 1. Load gen3sis and all necessary input data is set (landscape and config).
#' 
#' library(gen3sis)
#' 
#' # get path to example input inside package
#' datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")
#' path_config <- file.path(datapath, "config/config_worldcenter.R")
#' path_landscape <- file.path(datapath, "landscape")
#' 
#' # 2. Run simulation
#' 
#'sim <- run_simulation(config = path_config, landscape = path_landscape)
#'
#' # 3. Visualize the outputs
#' 
#' # plot summary of entire simulation
#' plot_summary(sim)
#' 
#' # plot richness at a given time-step 
#' # this only works if species is saved for this time-step
#' landscape_t_150 <- readRDS(file.path(datapath, 
#' "output", "config_worldcenter", "landscapes", "landscape_t_150.rds"))   
#' species_t_150 <- readRDS(file.path(datapath, 
#' "output", "config_worldcenter", "species", "species_t_150.rds"))   
#' plot_richness(species_t_150, landscape_t_150)   
#'
#' }
#' @docType package
#' @useDynLib gen3sis, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import Matrix
NULL


#' Run a simulation in gen3sis and return a summary object possibly saving outputs and plots to the output folder 
#' 
#' @details This function runs a simulation with defined landscape and config objects. 
#' Possibly plot and save specified outputs as defined in the end_of_timestep_observer function inside the config object
#' @param config configuration file for the simulation or configuration object derived from a config file
#' @param landscape directory where the all_geo_hab and distance_matrices reside
#' @param output_directory directory for the simulation output
#' @param timestep_restart set the start time time-step. 
#' If timestep_restart=NA (default), start at the oldest available landscape. 
#' If timestep_restart="ti", start from the last available time-step. 
#' If a number "x", start at time-step x (e.g. timestep_restartstart=6)
#' @param save_state save the internal state of the simulation for restarts.
#' If save_state=NA (default), do not save any internal state of the simulation.
#' If save_state="all", save all time-step. 
#' If save_state="last", saves only last time-step.
#' If a vector, saves the desired time-steps (e.g. save_state=c(1,3,5))
#' @param call_observer call observer functions.
#' If call_observer="all" (default), call all time-steps.
#' If call_observer=NA, calls the start and end times.
#' If a number "X", call call_observer at x time-steps equally spaced between start and end steps. 
#' For example, on a simulation with start time of 1 and end time of 20, call_observer=1 calls the observer function at time-steps 1, 11 and 20.
#' @param enable_gc enable gc in case of memory shortages
#' @param verbose integer value (i.e. 0, 1 ,2 or 3). 
#' If verbose=0, no printed statement.
#' If verbose=1 (default), print time-step progress.
#' If verbose=2, enable additional progress outputs regarding current time-step.
#' If verbose=3, enable additional information from within modules
#'
#' @return a summary object containing a minimal summary on simulation and dynamics progress (alive, speciations, extinctions) as well as useful simulation data
#'
#' @importFrom utils packageVersion write.table
#' 
#' @example inst/examples/run_simulation_help.R
#' @seealso \code{\link{plot_summary}}   \code{\link{create_input_config}}   \code{\link{create_input_landscape}} 
#' @export
run_simulation <- function(config = NA,
                          landscape = NA,
                          output_directory = NA, 
                          timestep_restart = NA,
                          save_state = NA,
                          call_observer = "all",
                          enable_gc = FALSE,
                          verbose = 1){
  
  #----------------------------------------------------#
  ####### User defined variables (config.R) ############
  #----------------------------------------------------#

  system_time_start <- Sys.time() #Starting timer

  directories <- prepare_directories(config_file = config,
                                     input_directory = landscape,
                                     output_directory = output_directory)

  if(is.na(config)[1]){
    stop("please provide either a config file or a config object")
  } else if (class(config)=="gen3sis_config"){
    config[["directories"]] <- directories
  } else if (class(config)=="character"){
    file.copy(config, directories$output)
    config <- create_input_config(config_file = config)
    config[["directories"]] <- directories
  } else {
    stop("this is not a known config, please provide either a config file or a config object")
  }
  if(!verify_config(config)){
    stop("config verification failed")
  }

  val <- list("data" = list(),
              "vars" = list(),
              "config" = config)

  val$config <- complete_config(val$config)

  val$config$gen3sis$general$verbose <- verbose

  #val$config$gen3sis$version <- "1.1"
  #val$config$gen3sis$nickname <- "Quintessenced"

  # #---------------------------------------------------------#
  # ###### ATTRIBUTE ANCESTOR DISTRIBUTION (simulation.R) #####
  # #---------------------------------------------------------#
  val <- setup_inputs(val$config, val$data, val$vars)
  val <- setup_variables(val$config, val$data, val$vars)
  val <- setup_landscape(val$config, val$data, val$vars)
  val <- init_attribute_ancestor_distribution(val$config, val$data, val$vars)

  # #---------------------------------------------------#
  # #####               SIMULATION START            #####
  # #---------------------------------------------------#
  # #---------------------------------------------------#
  # #####               Init simulation             #####
  # #---------------------------------------------------#
  val <- init_simulation(val$config, val$data, val$vars)
  
  val <- init_summary_statistics(val$data, val$vars , val$config)
  

  #--------------------------------------------#
  ######## Call observer to plot or save #######
  #--------------------------------------------#

  ### call observer
  if (is.na(call_observer)){
    save_steps <- c(val$config$gen3sis$general$start_time,val$config$gen3sis$general$end_time)
  } else if (call_observer=="all"){
    save_steps <- val$config$gen3sis$general$start_time:val$config$gen3sis$general$end_time
  } else {
    steps <- as.integer(call_observer) + 2
    save_steps <- ceiling(seq(val$config$gen3sis$general$start_time,
                              val$config$gen3sis$general$end_time,
                              length.out = steps))
  }
  # # when to save the species data. +1 is added to tf for matters of timeps jumps between ti and tn
  val$vars$save_steps <- save_steps

  val$vars$steps <- val$config$gen3sis$general$start_time:val$config$gen3sis$general$end_time
  #
  #
  #
  if(!is.na(timestep_restart)){
   val <- restore_state(val, timestep_restart)
  }

  for(ti in val$vars$steps){ #loop over time steps
    # set to zero every new time-step!
    val$vars$n_new_sp_ti <- 0
    val$vars$n_ext_sp_ti <- 0
    val$vars$n_sp_added_ti <- 0
    # update ti inside val$vars
    val$vars$ti <- ti
    #
    #
    #
    if( val$vars$n_sp_alive >= val$config$gen3sis$general$max_number_of_species ) {
      val$vars$flag <- "max_number_species"
      print("max number of species reached, breaking loop")
      break
    }
    #
    #
    #     #----------------------------------------#
    #     ######## loop setup (simulation.R) #######
    #     #----------------------------------------#
    if(verbose>=2){
      cat("loop setup \n")
    }
    val <- setup_landscape(val$config, val$data, val$vars)
    val <- restrict_species(val$config, val$data, val$vars)

    #val <- loop_setup_geo_dist_m_ti(val$config, val$data, val$vars)
    val <- setup_distance_matrix(val$config, val$data, val$vars)


    #     #----------------------------------------------------------#
    #     ######## loop speciation (simulation.R) #######
    #     #----------------------------------------------------------#
    if(verbose>=2){
      cat("speciation \n")
    }
    val <- loop_speciation(val$config, val$data, val$vars)

    # updates to take into account new species
    val <- update1.n_sp.all_geo_sp_ti(val$config, val$data, val$vars)
    val <- update2.n_sp_alive.geo_sp_ti(val$config, val$data, val$vars)


    #    #---------------------------------------------#
    #    ######## loop dispersal                 #######
    #    #---------------------------------------------#
    if(verbose>=2){
      cat("dispersal \n")
    }
    val <- loop_dispersal(val$config, val$data, val$vars)


    #     #----------------------------------------------------------#
    #     ######## loop evolution                              #######
    #     #----------------------------------------------------------#
    if(verbose>=2){
      cat("evolution \n")
    }
    val <- loop_evolution(val$config, val$data, val$vars)
    
    #     #--------------------------------------------------------#
    #     ######## loop ecology                              #######
    #     #--------------------------------------------------------#
    if(verbose>=2){
      cat("ecology \n")
    }
    val <- loop_ecology(val$config, val$data, val$vars)
    if( val$vars$flag == "max_number_coexisting_species") {
      print("max number of coexisting species reached, breaking loop")
      break
    }
    
    
    # #     #-------------------------------------------------------------------#
    # #     ######## end of time-step loop variable update (simulation.R) #######
    # #     #-------------------------------------------------------------------#
    
    if(verbose>=2){
      cat("end of loop updates \n")
    }
    #
    #
    #   #------------------------------------------------------------------#
    #   ########     update loop steps variable (simulation.R)       #######
    #   ######## !!! check with end of time-step variable update !!! #######
    #   #------------------------------------------------------------------#
    val$vars$n_sp_alive <- sum(sapply(val$data$all_species, function(sp){ifelse(length(sp[["abundance"]]), 1, 0) }))
    val$vars$n_sp <- length(val$data$all_species)

    val <- update_loop_steps_variable(val$config, val$data, val$vars)


    if(val$vars$ti %in% val$vars$save_steps){
      call_main_observer(val$data, val$vars, val$config)
    }
    
    val <- update_summary_statistics(val$data, val$vars, val$config)
    
    save_val(val, save_state)
    
    if (verbose>=1){
      cat('step =', ti, ', species alive =', val$vars$n_sp_alive, ', species total =', val$vars$n_sp, '\n')  
    }
    
  }# close loop steps
  #
  if(verbose>=0 & val$vars$flag=="OK"){
    cat("Simulation finish. All OK \n")
  }

  # #------------------------------------------------------------------#
  # ######## update phylo with survival info (simulation.R) #######
  # #------------------------------------------------------------------#
  val <- update.phylo(val$config, val$data, val$vars)

  # #------------------------------------------------------------------#
  # ######## write phylogeny (internal_functions.R) #######
  # #------------------------------------------------------------------#
  write.table(val$data$phy, file = file.path(val$config$directories$output, "phy.txt"), sep="\t")

  write_nex(phy=val$data$phy, label="species", file.path(output_location=val$config$directories$output, "phy.nex"))
  
  # #------------------------------------------------------------------#
  # ######## prepare and save summaries   (internal_functions.R) #######
  # #------------------------------------------------------------------#
  system_time_stop <- Sys.time()
  total_runtime <- difftime(system_time_stop, system_time_start, units = "hours")[[1]]
  
  write_runtime_statisitics(val$data, val$vars, val$config, total_runtime)
  
  sgen3sis <- make_summary(val$config, val$data, val$vars, total_runtime, save_file=TRUE)
  
  if(verbose >= 1){
    cat("Simulation runtime:", total_runtime, "hours\n")
  }
  return(sgen3sis)
}
