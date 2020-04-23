# Copyright (c) 2020, ETH Zurich


#' Initialize the summary statistics, currently a matrix of species totals, speciations, and extinctions 
#'
#' @param data the current data object
#' @param vars the current vars object
#' @param config the current vconfig
#'
#' @return the standard val list of data, vars, config
#' @noRd
initialize_summary_statistics <- function(data, vars, config){
  data[["summaries"]] <- list()
  
  phylo_summary <- matrix(NA, nrow = 1, ncol = 4, 
                          dimnames = list("initial", c("total", "alive", "speciations", "extinctions")))
  
  phylo_summary[1, "total"] <- length(data$all_species)
  phylo_summary[1, "alive"] <- sum(unlist(lapply(data$all_species, function(x){ifelse(length(x$abundance), 1, 0)} )))
  phylo_summary[1, "speciations"] <- length(data$all_species)
  phylo_summary[1, "extinctions"] <- phylo_summary[1, "total"] - phylo_summary[1, "alive"] 
  
  data$summaries[["phylo_summary"]] <- phylo_summary
  
  data$summaries[["occupancy"]] <- c("initial" = percentage_inhabited(data$all_species, data$landscape))
  
  return(list(data = data, vars = vars, config = config))
}


#' Update the summary statistics, currently a matrix of species totals, speciations, and extinctions 
#'
#' @param data the current data object
#' @param vars the current vars object
#' @param config the current vconfig
#'
#' @return the standard val list of data, vars, config
#' @noRd
update_summary_statistics <- function(data, vars, config) {
  # phylo
  phylo_summary <- data$summaries$phylo_summary
  n <- nrow(phylo_summary)
  total <- length(data$all_species)
  alive <- sum(unlist(lapply(data$all_species, function(x){ifelse(length(x$abundance), 1, 0)} )))
  speciations <- total - phylo_summary[n, "total"] 
  extinctions <- speciations - alive + phylo_summary[n, "alive"]
  
  phylo_summary <- rbind(phylo_summary, c(total, alive, speciations, extinctions))
  
  rownames(phylo_summary) <- c(rownames(phylo_summary)[1:n], vars$ti)
  
  data$summaries$phylo_summary <- phylo_summary
  
  # occupancy
  tmp <- c(names(data$summaries$occupancy), vars$ti)
  occupancy <- c(data$summaries$occupancy, percentage_inhabited(data$all_species, data$landscape)) 
  names(occupancy) <- tmp
  data$summaries$occupancy <- occupancy
  
  return(list(data = data, vars = vars, config = config))
}


#' Saves a numer of summry statistics at the end of a simulation run at sgen3sis. Contents tbd
#'
#' @param config the current config object
#' @param data the current data oject
#' @param vars the current vars object
#' 
#' @importFrom utils packageVersion write.table
#'
#' @noRd
save_summary <- function(config, data, vars){
  # function that creates a summary file, mainly sgen3sis.RData at the end of the simulation
  path <- strsplit(config$directories$input, "/")[[1]]
  world <- path[length(path)]
  
  # sgen3sis <- list(parameters=list(gasm_version=config$gasm_version,
  #                               gasm_version=config$gasm_nickname))
  
  sgen3sis <- list("parameters" = list())
  
  sgen3sis$parameters <- c(sgen3sis$parameters, config)
  
  sgen3sis <- c(sgen3sis, list(
    packageVersion=paste0( "gen3sis_", packageVersion("gen3sis")),
    turnover=data$turnover,
    phy = data$phy,
    geo_richness = data$geo_richness,
    # eco_by_cell=NULL#,    #Possible fix, add: eco_by_cell=data$eco_by_sp [Oskar]
    #eco_by_sp_tf0=data$eco_by_sp_tf0,
    eco_by_sp = data$eco_by_sp
    #cpu_time=difftime(system_time_stop, system_time_start, units = "hours")[[1]]
  ))
  
  #### START WIPOBSERVER ####
  #add to sgen3sis all summary objects created with observer_summary
  #### END WIPOBSERVER ####
  
  sgen3sis <- c(sgen3sis, flag=vars$flag)
  class(sgen3sis) <- "gen3sis_summary"
  save(sgen3sis, file=file.path(config$directories$output,"sgen3sis.RData"))
}


#' Write the runtime information to a file
#'
#' @param data the current data object 
#' @param vars the current vars object
#' @param config the current config
#' @param total_runtime runtime for the simulation as collected in gen3sis_main.R
#'
#' @noRd
write_runtime_statisitics <- function( data, vars, config, total_runtime) {
  # write out the runtime statistics, e.g R version, package version, runtime etc
  cat("write_runtime_statistics to be implemented\n")
  stat_file <- file(file.path(config$directories$output, "runtime_information.txt"))
  sink(stat_file)
  cat("runtime:", total_runtime, "hours\n")
  cat("\n\n R Version Information:\n\n")
  print(version)
  cat("\n\n Session Information: \n\n")
  print(sessionInfo())
  cat("\n\n System: \n\n")
  print(Sys.info()["sysname"])
  sink()
  close(stat_file)
}


#' calcuates the ratio of occupied cells in a given landscape
#'
#' @param species_list a list of species to consider 
#' @param landscape the landscape to use
#'
#' @return the ratio of inhabited cells
#' @noRd
percentage_inhabited <- function(species_list, landscape) {
  richness <- get_geo_richness(species_list, landscape)
  inhabited <- sum(richness != 0)
  return(inhabited / length(richness))
}


percentage_species_alive <- function(species_list){
  alive <- sum( sapply(species_list, function(sp){ifelse(length(sp[["abundance"]]), 1, 0) }))
  return(alive / length(species_list))
}


min_max_traits <- function(species){
  min_max <- matrix(data = NaN,
                    nrow = 2,
                    ncol = ncol(species[["traits"]]),
                    dimnames = list(c("min", "max"), colnames(species[["traits"]])))
  if(!length(species[["abundance"]])) {
    min_max["min", ] <- Inf
    min_max["max", ] <- -Inf
  } else {
    min_max["min",] <- apply(species[["traits"]], 2, min)
    min_max["max",] <- apply(species[["traits"]], 2, max)
  }
  return(min_max)
}


trait_ranges <- function(species_list) {
  all_ranges <- lapply(species_list, min_max_traits)
  ranges <- do.call(pmin, all_ranges)
  ranges["max", ] <- do.call(pmax, all_ranges)["max", ]
  return(ranges)
}


habitat_extents <- function(species_list, exclude_extinct = T, plot_hist = F) {
  extents <- sapply(species_list, function(sp){length(sp[["abundance"]])})
  names(extents) <- sapply(species_list, function(sp){as.character(sp[["id"]])})
  if(exclude_extinct) {
    extents <- extents[extents != 0]
  }
  return(extents)
}
