# Copyright (c) 2020, ETH Zurich

#' Internal function that saves the current simulation state
#'
#' @param val the internal state
#' @param save_state This parameter can take any of the following
#'   values:
#'
#' * NA (default): do not save any internal state of the simulation.
#'
#' * "all": save all time steps.
#'
#' * "last": save last time step only
#'
#' * Numeric vector: save the specified time steps.
#' 
#' @noRd
save_val <- function(val, save_state = NA){
  # Check if the simulation will be saved
  create_save <- identical(save_state, "all") || 
    identical(save_state, "last") ||
    val$vars$ti %in% save_state
  
  
  if (create_save) {
    state_dir <- file.path(val$config$directories$output, "val")
    if (!dir.exists(state_dir)) { # if the saving directory does not exist
      dir.create(state_dir, recursive = TRUE) # this line creates it
    }
    
    if (identical(save_state, "last")) {
      # Previous state file(s) will be removed after the most recent
      # one has been created
      prev_files <- list.files(state_dir, full.names = TRUE)
    }
    
    # Save current Random Number Generator (seed) state
    val$config$seed <- .GlobalEnv$.Random.seed
    val$data$distance_matrix <- NULL
    state_file <- file.path(state_dir, paste0("val_t_", val$vars$ti, ".rds")) # saving path
    saveRDS(val, state_file) # save the state
    if (identical(save_state, "last")) {
      file.remove(prev_files) # remove previous state file(s)
    }
  }
}

#' Saves the current species and landscape objects
#'
#' @param val the current simulation state
#' @noRd
save_ecogengeo <- function(val){
  # save eco at time ti
  if(is.null(val$vars$ti)){
    ti <- val$config$gen3sis$general$start_time
  } else {
    ti <- val$vars$ti
  }

  species <- val$data$all_species
  saveRDS(object = species,
          file = file.path(val$config$directories$output_species, paste0("species_t_", val$vars$ti, ".rds")))

  landscape <- val$data$landscape
  saveRDS(object = landscape,
          file = file.path(val$config$directories$output_landscapes, paste0("landscape_t_", val$vars$ti, ".rds")))

}


#' Restores the simulation form a previously saved state
#'
#' @param val a semi valid simulation state
#' @param restart_timestep the time-step to restart from
#' @noRd
restore_state <- function(val, restart_timestep){
  ### val contains a populated config, required for directory information
  ### restore previous simulation state
  state_dir <- file.path(val$config$directories$output, "val")
  if(restart_timestep == "ti"){
    #look for the most recent completed time-step
    regex <- "\\d+"
    files <- list.files(state_dir)
    if(length(files) == 0){
      print("no val found, starting from the initial time-step")
      return(val)
    }
    numbers <- as.integer(regmatches(files, regexpr(regex, files)))
    timestep <- min(numbers)

  } else {
    timestep <- as.integer(restart_timestep)
  }

  val <- readRDS(file.path(state_dir, paste0("val_t_", timestep, ".rds")))
  .GlobalEnv$.Random.seed <- val$config$seed

  if(timestep > 0){
    val$vars$save_steps <- (timestep-1):(val$config$gen3sis$general$end_time)
    val$vars$steps <- (timestep-1):(val$config$gen3sis$general$end_time)
    print(paste("restarting at time-step:", timestep))
  } else {
    val$vars$save_steps <- NULL
    val$vars$steps <- NULL
    print("simulation already completed")
  }

  return(val)
}
