# Copyright (c) 2020, ETH Zurich


#' create_initial_species allows the user to populate the world at the beginning of a simulation
#'
#' @details The user can create any number of species in this function. For every species she shall choose a number of
#' habitable cells from the landscape and call 'create_species'. Afterwards she must initialize the species[["traits"]]
#' matrix with the desired initial traits values.
#'
#' @param landscape the current landscape
#' @param config the current config
#'
#' @return a list of species
#' @export
create_initial_species <- function(landscape, config){
  stop("this function documents the user function interface only, do not use it!")
}


#' create_species This creates a new species
#'
#' @details This function is intented to be used in the create_initiual_species function you, the user,
#' provide in the configuration of a simulation. It will create a species object representing one species
#' in the simulation occupying the given list of initial cells.
#'
#' @param id the id for the new species, might be overriden, tbd
#' @param initial_cells a list of initial cells (strings) to occupy
#' @param config the current config object
#'
#' @return returns a newly created species occupying the provided initial cells
#' @export
create_species <- function(id, initial_cells, config) {
  num_cells <- length(initial_cells)
  traits <- unique(c(config$gen3sis$general$trait_names, "dispersal"))
  species <- list()
  species[["id"]] <- as.character(id)
  species[["abundance"]] <- rep(config$gen3sis$initialization$initial_abundance, times = num_cells)
  names(species[["abundance"]]) <- initial_cells
  species[["traits"]] <- matrix(NA,
                                nrow = num_cells,
                                ncol = length(traits),
                                dimnames = list(initial_cells, traits))

  clusters <- as.integer(rep(1, length = num_cells))
  names(clusters) <- initial_cells
  dist_matrix <- matrix(0, nrow = 1, ncol = 1)
  dimnames(dist_matrix) <- list(1, 1)
  species[["divergence"]] <- list("clusters" = clusters,
                                  "cluster_divergence" = dist_matrix)
  class(species) <- "gen3sis_species"
  return(invisible(species))
}


#' creates a new species from an existing species
#'
#' @param parent_species the parent species
#' @param new_id the id of the new species (tdb)
#' @param new_cells which cells should be taken from the parent species
#' @param config the current config object
#'
#' @return returns a new species. !! the parent_species is not altered, it will need to be shrunken seperately !!
#' @noRd
create_species_from_existing <- function(parent_species, new_id, new_cells, config) {
  new_species <- create_species(new_id, new_cells, config)
  new_species[["abundance"]] <- parent_species[["abundance"]][new_cells]
  new_species[["traits"]] <- parent_species[["traits"]][new_cells, , drop = F]
  gen_dist <- limit_divergence_to_cells(parent_species[["divergence"]], new_cells)

  # quick and dirty fix
  # if the cells belong to multiple genetic clusters those clusters are not collapsed if possible.
  new_species[["divergence"]] <- consolidate_divergence(gen_dist)

  return(invisible(new_species))
}

summary.gen3sis_species <- function(species) {
  return(invisible(species))
}

update_species_from_abundance <- function(species) {
  return(invisible(species))
}


#' get_divergence_matrix returns the full genetic distance matrix for a given species (cell x cell)
#'
#' @param species the species to produce the full genetic distance matrix for
#'
#' @return the full/decompressed divergence matrix
#' @export
get_divergence_matrix <- function(species) {
  return( invisible(decompress_divergence(species[["divergence"]]) ) )
}


#' disperse_species disperses a species to occupy a list of newly selected cells
#'
#' @details This function disperses a species to cover new cells. In order to maintain the divergence and trait
#' distributions for every newly occupied cell we need to provide the source cell to take this information from.
#'
#' @param species the species to disperse
#' @param source a list of cell occupied by the species
#' @param destination a list of target cells to colonize
#' @param config the current config object
#'
#' @return the dispersed species
#' @noRd
disperse_species <- function(species, source, destination, config){
  # expand species to cover destianation cells
  # for every cell in destination, source indicates the origin cell
  index <- 1:length(species[["abundance"]])
  names(index) <- names(species[["abundance"]])
  index[destination] <- index[source]

  sorted <- as.character(sort(as.numeric(names(index))))

  abundance <- species[["abundance"]]
  abundance[destination] <- config$gen3sis$initialization$initial_abundance
  species[["abundance"]] <- abundance[sorted]

  traits <- species[["traits"]][source, , drop = F]
  rownames(traits) <- destination
  species[["traits"]] <- rbind(species[["traits"]], traits)[sorted, , drop = F]

  clusters <- species[["divergence"]][["clusters"]]
  clusters[destination] <- clusters[source]
  species[["divergence"]][["clusters"]] <- clusters[sorted]

  return(invisible(species))
}


#' limit_species_to_cells limits a species to a given set of cells
#'
#' @details this function is used in two context: after creating a new species during a speciation event
#' this function removes any cell belonging to the newly separated species. And at the beginning of a new timestep
#' it is used to remove all cells that become uninhabitable.
#'
#' @param species the current species
#' @param cells the list of cells to limit the species to
#'
#' @return the newly constrained species
#' @noRd
limit_species_to_cells <- function(species, cells) {
  limited_cells <- names(species[["abundance"]])
  limited_cells <- limited_cells[which(limited_cells %in% cells)]

  species[["abundance"]] <- species[["abundance"]][limited_cells]
  species[["traits"]] <- species[["traits"]][limited_cells, , drop = F]
  species[["divergence"]] <- limit_divergence_to_cells(species[["divergence"]], limited_cells)

  return(invisible(species))
}

