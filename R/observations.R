# Copyright (c) 2020, ETH Zurich



#' prepare the interal data structures to be called with the observe_xx functions and call the user provide observer
#'
#' @param data the current data object 
#' @param vars the current vars object
#' @param config the current config
#'
#' @noRd
call_main_observer <- function(data, vars, config) {
  config$gen3sis$general$end_of_timestep_observer(data, vars, config)
}


#' Here the 1-2 page default summary plotting can take place
#'
#' @param data the current data object 
#' @param vars the current vars object
#' @param config the current config
#'
#' @noRd
plot_end_of_simulation <- function(data, vars, config) {
  # plotting of end of simulation goes here, like plotting the phylo_summary, 
}


#' Write the runtime information to a file
#'
#' @param data the current data object 
#' @param vars the current vars object
#' @param config the current config
#'
#' @noRd
write_runtime_statisitics <- function( data, vars, config) {
  # write out the runtime statistics, e.g R version, package version, runtime etc
}


#' This function can be called within the observer function to observe (save) the full species list.
#'
#' @export
observe_species <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <-  dynGet("vars")
  observe_landscape()
  dir.create(file.path(config$directories$output, "species"), showWarnings = F, recursive = T)
  species <- data$all_species
  saveRDS(object = species,
          file = file.path(config$directories$output, "species", paste0("species_t_", vars$ti, ".rds")))
}


#' observes (saves) the current landscape, can be called independantly by the user and is called by 
#' other observer functions relying on the landscape to be present
#'
#' @export
observe_landscape <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <-  dynGet("vars")
  landscape_file = file.path(config$directories$output, "landscapes", paste0("landscape_t_", vars$ti, ".rds"))
  if( !file.exists(landscape_file)){
    dir.create(file.path(config$directories$output, "landscapes"), showWarnings = F, recursive = T)
    landscape <- data$landscape
    saveRDS(object = landscape, file = landscape_file)
  }
}