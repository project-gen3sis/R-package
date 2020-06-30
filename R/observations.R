# Copyright (c) 2020, ETH Zurich



#' Prepare the internal data structures to be called with the observe_xx functions and call the user provide observer
#' Unless a problem shows up the save_xx functions call getDyn() to get access to the internal state, no prep here.
#'
#' @param data the current data object 
#' @param vars the current vars object
#' @param config the current config
#'
#' @noRd
call_main_observer <- function(data, vars, config) {
  config$gen3sis$general$end_of_timestep_observer(data, vars, config)
}


#' This function can be called within the observer function to save the current occupancy pattern
#' 
#' @example inst/examples/save_richness_help.R
#' @export
save_occupancy <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <-  dynGet("vars")
  save_landscape()
  dir.create(file.path(config$directories$output, "occupancy"), showWarnings = F, recursive = T)
  tmp <- get_geo_richness(data$all_species, data$landscape)
  tmp <- tmp > 0 
  saveRDS(object = tmp,
          file = file.path(config$directories$output, "occupancy", paste0("occupancy_t_", vars$ti, ".rds")))
}


#' This function can be called within the observer function to save the current richness pattern
#' 
#' @example inst/examples/save_richness_help.R
#' @export
save_richness <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <-  dynGet("vars")
  save_landscape()
  dir.create(file.path(config$directories$output, "richness"), showWarnings = F, recursive = T)
  richness <- get_geo_richness(data$all_species, data$landscape)
  saveRDS(object = richness,
          file = file.path(config$directories$output, "richness", paste0("richness_t_", vars$ti, ".rds")))
}


#' This function can be called within the observer function to save the current phylogeny.
#' 
#' @example inst/examples/save_phylogeny_help.R
#' @export
save_phylogeny <- function(){
  config <- dynGet("config")
  data <- dynGet("data")
  vars <-  dynGet("vars")
  
  directory <- file.path(config$directories$output, "phylogeny")
  dir.create(directory, showWarnings = F, recursive = T)
  
  file <- file.path(directory, paste0("phylogeny_t_", vars$ti, ".nex"))
  write_nex(phy=data$phy, label="species", output_file=file)
}


#' This function can be called within the observer function to save the full species list.
#' 
#' @example inst/examples/save_species_help.R
#' @export
save_species <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <-  dynGet("vars")
  save_landscape()
  dir.create(file.path(config$directories$output, "species"), showWarnings = F, recursive = T)
  species <- data$all_species
  saveRDS(object = species,
          file = file.path(config$directories$output, "species", paste0("species_t_", vars$ti, ".rds")))
}


#' saves the current landscape, can be called independantly by the user and is called by 
#' other observer functions relying on the landscape to be present
#'
#' @example inst/examples/save_landscape_help.R
#' @export
save_landscape <- function() {
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


#' This function can be called within the observer function to save the species abundances.
#' 
#' @example inst/examples/save_abundance_help.R
#' @export
save_abundance <- function() {
  save_extract("abundance")
}


#' This function can be called within the observer function to save the species traits.
#' 
#' @example inst/examples/save_traits_help.R
#' @export
save_traits <- function() {
  save_extract("traits")
}


#' This function can be called within the observer function to save the compressed species divergence.
#' 
#' @example inst/examples/save_divergence_help.R
#' @export
save_divergence <- function() {
  save_extract("divergence")
}


#' Save a named element from all species.
#' @param element Name of element to save, e.g. "abundance" or "traits"
#' @noRd
save_extract <- function(element) {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <-  dynGet("vars")
  save_landscape()
  dir.create(file.path(config$directories$output, element), showWarnings = F, recursive = T)
  tmp <- lapply(data$all_species, function(x){return(x[[element]])})
  names(tmp) <- sapply(data$all_species, function(x){x$id})
  saveRDS(object = tmp,
          file = file.path(config$directories$output, element, paste0(element, "_t_", vars$ti, ".rds")))
}

