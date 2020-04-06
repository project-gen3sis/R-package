# Copyright (c) 2020, ETH Zurich

#' @title Gen3sis: General Engine for Eco-Evolutionary Simulations
#' @name gen3sis
#' @examples
#' \dontrun{
#' #runing a Gen3sis simulation
#' # add(1, 1)
#' # add(10, 1)
#' }
#' @docType package
#' @useDynLib gen3sis, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import Matrix
NULL

counting <- new.env(parent = emptyenv())
assign("dist", -Inf, envir = counting)

#' Run a simulation
#'
#' @param config configuration file for the simulation or configuration object derived from a config file
#' @param input_directory directory where the all_geo_hab and distance_matrices reside
#' @param output_directory directory for the simulation output
#' @param save_intermediate_results save the eco, gen, and geo info
#' @param enable_gc enable gc in case of memory shortages
#' @param verbose integer value (0, 1 ,2 or 3). If 0 no printed statement, 1 timestep progress, 2 enable additional progress outputs regarding current timestep, 3 aditional information from within modules (default is 1)
#'
#' @return a summary object containing a minimal summary on species progress (alive, speciations, extinctions) 
#'
#' @importFrom utils packageVersion write.table
#' 
#' @example inst/examples/run_simulation_help.R
#'
#' @export
run_simulation <- function(config = NA,
                          input_directory = NA,
                          output_directory = NA, 
                          # timestep_restart = NA, disabled for the moment
                          # save_state = NA, disabled for the moment as for example compiled cpp functions are not saved correctly
                          save_intermediate_results = NA,
                          enable_gc = F,
                          verbose = 1){
  
  #param timestep_restart restart an exisitng simulation from this timestep or from the latest timestep
  #param save_state save the internal state of the simulation for restarts
  
  #----------------------------------------------------#
  ####### User defined variables (config.R) ############
  #----------------------------------------------------#
  #assign(x = "enable_gc", value = enable_gc, envir = globalenv())
  #assign(x = "gasm_version", value = "1.1", envir = globalenv())
  #gasm_version = "1.1"
  #assign(x = "gasm_nickname", value = "Quintessence", envir = globalenv())
  #gasm_nickname = "Quintessence"

  system_time_start <- Sys.time() #Starting timer

  directories <- prepare_directories(config_file = config,
                                     input_directory = input_directory,
                                     output_directory = output_directory)

  if(is.na(config)[1]){
    stop("please provide either a config file or a config object")
  } else if (class(config)=="gen3sis_config"){
    config[["directories"]] <- directories
  } else if (class(config)=="character"){
    config <- create_config(config_file = config)
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

  path = file.path(directories$output, paste0( "gen3sis_", packageVersion("gen3sis")))
  file.create(path)

  val$config$gen3sis$general$verbose <- verbose

  #val$config$gen3sis$version <- "1.0"
  #val$config$gen3sis$nickname <- "Quintessence"

  # #---------------------------------------------------------#
  # ######## ATTRIBUTE MRCA DISTRIBUTION (simulation.R) #######
  # #---------------------------------------------------------#
  val <- setup_inputs(val$config, val$data, val$vars)
  val <- setup_variables(val$config, val$data, val$vars)
  val <- setup_landscape(val$config, val$data, val$vars)
  val <- init_attribute_mrca_distribution(val$config, val$data, val$vars)

  # #---------------------------------------------------#
  # #####               SIMULATION START            #####
  # #---------------------------------------------------#
  # #---------------------------------------------------#
  # #####               Init simulation             #####
  # #---------------------------------------------------#
  val <- init_simulation(val$config, val$data, val$vars)
  
  val <- initialize_summary_statistics(val$data, val$vars , val$config)
  

  #--------------------------------------------#
  ######## Save inital geo/gen/eco state #######
  #--------------------------------------------#
  #save.ecogengeo(val)

  ### save eco gen and geo
  if (is.na(save_intermediate_results)){
    save_steps <- c(val$config$gen3sis$general$start_time,val$config$gen3sis$general$end_time)
  } else if (save_intermediate_results=="all"){
    save_steps <- val$config$gen3sis$general$start_time:val$config$gen3sis$general$end_time
  } else {
    steps <- as.integer(save_intermediate_results) + 2
    save_steps <- ceiling(seq(val$config$gen3sis$general$start_time,
                              val$config$gen3sis$general$end_time,
                              length.out = steps))
  }
  # # When to save the species data. +1 is added to tf for matters of timeps jumps between ti and tn
  val$vars$save_steps <- save_steps

  val$vars$steps <- val$config$gen3sis$general$start_time:val$config$gen3sis$general$end_time
  #
  #
  #

  # if(!is.na(timestep_restart)){
  #  val <- restore_state(val, timestep_restart)
  # }

  for(ti in val$vars$steps){ #loop over time steps
    # set to zero every new timestep!
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
      print("breaking loop due to too large number of species")
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
    #    ######## loop dispersal (simulation.R) #######
    #    #---------------------------------------------#
    if(verbose>=2){
      cat("dispersal \n")
    }
    val <- loop_dispersal(val$config, val$data, val$vars)


    #     #----------------------------------------------------------#
    #     ######## loop mutation (simulation.R) #######
    #     #----------------------------------------------------------#
    if(verbose>=2){
      cat("evolution \n")
    }
    val <- loop_evolution(val$config, val$data, val$vars)
    #     #--------------------------------------------------------#
    #     ######## loop Eco Equilibria Module (simulation.R) #######
    #     #--------------------------------------------------------#
    if(verbose>=2){
      cat("ecology \n")
    }
    val <- loop_ecology(val$config, val$data, val$vars)
    # #     #------------------------------------------------------------------#
    # #     ######## end of timestep loop variable update (simulation.R) #######
    # #     #------------------------------------------------------------------#
    # val <- update1.n_sp.all_geo_sp_ti(val$config, val$data, val$vars)
    # val <- update2.n_sp_alive.geo_sp_ti(val$config, val$data, val$vars)
    if(verbose>=2){
      cat("end of loop updates \n")
    }

    #
    #
    #   #-----------------------------------------------------------------#
    #   ########     update loop steps variable (simulation.R)      #######
    #   ######## !!! check with end of timestep variable update !!! #######
    #   #-----------------------------------------------------------------#
    val$vars$n_sp_alive <- sum( sapply(val$data$all_species, function(sp){ifelse(length(sp[["abundance"]]), 1, 0) }))
    val$vars$n_sp <- length(val$data$all_species)

    val <- update_loop_steps_variable(val$config, val$data, val$vars)
    
    #### START WIPOBSERVER ####
    #call here the observer summary functions to update vals$observer
    # do.call(observer_summary)
    #### END WIPOBSERVER ####
    
    # save_val(val, save_state)

    if(val$vars$ti %in% val$vars$save_steps){
      call_main_observer(val$data, val$vars, val$config)
      # save_ecogengeo(val)
    }
    val <- update_summary_statistics(val$data, val$vars, val$config)
    
    if (verbose>=1){
      cat('step =', ti, ', species alive =', val$vars$n_sp_alive, ', species total =', val$vars$n_sp, '\n')  
    }
    
  }# close loop steps
  #
  if(verbose>=2){
    cat("Hello present! Simulation finish. All Ok \n")
  }
  system_time_stop <- Sys.time()
  write(difftime(system_time_stop, system_time_start, units = "hours")[[1]], file=file.path(val$config$directories$output, "CPUtime_h.txt"))
  #
  #
  
  # observer functions
  plot_end_of_simulation(val$data, val$vars, val$config)
  write_runtime_statisitics(val$data, val$vars, val$config)
  
  
  # #------------------------------------------------------------------#
  # ######## update phylo with survival info (simulation.R) #######
  # #------------------------------------------------------------------#
  val <- update.phylo(val$config, val$data, val$vars)


  # #------------------------------------------------------------------#
  # ######## save sgen3sis (internal_functions.R) #######
  # #------------------------------------------------------------------#
  save_summary(val$config, val$data, val$vars)


  # #------------------------------------------------------------------#
  # ######## write phylogeny (internal_functions.R) #######
  # #------------------------------------------------------------------#
  write.table(val$data$phy, file = file.path(val$config$directories$output, "phy.txt"), sep="\t")

  write_nex(phy=val$data$phy, label="sp", output_location=val$config$directories$output)
  
  return(val$data$summaries)
}
