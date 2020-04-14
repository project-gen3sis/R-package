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


#' Write the runtime information to a file
#'
#' @param data the current data object 
#' @param vars the current vars object
#' @param config the current config
#'
#' @noRd
write_runtime_statisitics <- function( data, vars, config) {
  # write out the runtime statistics, e.g R version, package version, runtime etc
  cat("write_runtime_statistics to be implemented\n")
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
  inhabited <- sum(which(richness != 0))
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
