# Copyright (c) 2020, ETH Zurich


#' The function create_ancestor_species  allows the user to populate the world at the beginning of a simulation.
#'
#' @details Using this function any number of new species can be created. For every species, a number of
#' habitable cells from the landscape are selected and call 'create_species'. In another step, the user must initialize 
#' the species[["traits"]] matrix with the desired initial traits values.
#'
#' @param landscape the landscape over which to create the species
#' @param config the configuration information
#'
#' @return a list of species
#' @export
create_ancestor_species  <- function(landscape, config){
  stop("this function documents the user function interface only, do not use it.")
}


#' creates a new species.
#'
#' @details This function is to be used in the create_ancestor_species function at the configuration 
#' of a simulation. It will create a species object representing one species in the simulation occupying the 
#' given list of initial cells.
#'
#' @param initial_cells a list of initial cells (strings) to occupy
#' @param config the configuration information
#'
#' @return returns a newly created species occupying the provided initial cells
#' @example inst/examples/create_species_help.R
#' @export
create_species <- function(initial_cells, config) {
  num_cells <- length(initial_cells)
  traits <- unique(c(config$gen3sis$general$trait_names, "dispersal"))
  species <- list()
  species[["id"]] <- as.character(-1)
  species[["abundance"]] <- rep(config$gen3sis$initialization$initial_abundance, times = num_cells)
  names(species[["abundance"]]) <- initial_cells
  species[["traits"]] <- matrix(NA,
                                nrow = num_cells,
                                ncol = length(traits),
                                dimnames = list(initial_cells, traits))

  index <- as.integer(rep(1, length = num_cells))
  names(index) <- initial_cells
  compressed_matrix <- matrix(0, nrow = 1, ncol = 1)
  dimnames(compressed_matrix) <- list(1, 1)
  species[["divergence"]] <- list("index" = index,
                                  "compressed_matrix" = compressed_matrix)
  class(species) <- "gen3sis_species"
  return(invisible(species))
}


#' Creates a new species from an existing species.
#'
#' @param parent_species the parent species
#' @param new_id the id of the new species
#' @param new_cells cells which should be taken from the parent species
#' @param config the configuration information
#'
#' @return returns a new species, the parent_species is not altered, and will need to be modified in another step
#' @noRd
create_species_from_existing <- function(parent_species, new_id, new_cells, config) {
  new_species <- create_species(new_cells, config)
  new_species[["id"]] <- new_id
  new_species[["abundance"]] <- parent_species[["abundance"]][new_cells]
  new_species[["traits"]] <- parent_species[["traits"]][new_cells, , drop = F]
  divergence <- limit_divergence_to_cells(parent_species[["divergence"]], new_cells)

  # quick and dirty fix
  # if the cells belong to multiple genetic clusters those clusters are not collapsed if possible.
  new_species[["divergence"]] <- consolidate_divergence(divergence)

  return(invisible(new_species))
}

summary.gen3sis_species <- function(species) {
  return(invisible(species))
}

update_species_from_abundance <- function(species) {
  return(invisible(species))
}


#' The function get_divergence_matrix returns the full divergence matrix for a given species (cell x cell).
#'
#' @details The functions allows to extract the full divergence matrix representing the accumulated differentiation
#' between all the cells that are occupied by the species. The input is a species object for any time step.
#' 
#' @param species the species for which the divergence matrix should be produced
#'
#' @return the full decompressed divergence matrix
#' @example inst/examples/get_divergence_matrix_help.R
#' @export
get_divergence_matrix <- function(species) {
  return( invisible(decompress_divergence(species[["divergence"]]) ) )
}


#' The function disperse_species disperses a species to occupy a list of newly selected cells.
#'
#' @details This function disperses a species to new cells. In order to maintain the divergence and trait
#' distributions for every newly occupied cell, this function inputs the source cell to take this information from.
#'
#' @param species the species to which dispersal is applied
#' @param source a list of cell occupied by the species
#' @param destination a list of target cells to colonize
#' @param config the configuration information
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

  index <- species[["divergence"]][["index"]]
  index[destination] <- index[source]
  species[["divergence"]][["index"]] <- index[sorted]

  return(invisible(species))
}


#' The function limit_species_to_cells limits a species to a given set of cells.
#'
#' @details This function is used in two situations. First, after creating a new species during a speciation event,
#' this function removes any cell from the first species that will be transferred to the newly separated species. 
#' Second, at the beginning of a new time step, it is used to remove all cells that have become uninhabitable.
#'
#' @param species the species to which this limit should be applied
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

