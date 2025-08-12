# Copyright (c) 2020, ETH Zurich


#' Allows defining the function that changes the values of traits of a given
#' species at each time-step and in each site. If no operations are provided, traits are not changing
#'
#' @details This function is called for any single species alongside an index for
#' the geographical clusters within the species
#'
#' @param species the target species object whose traits will be changed
#' @param cluster_indices an index vector indicating the cluster every occupied site is part of
#' @param space the current space which can co-determine the rate of trait changes
#' @param config the current config
#'
#' @return the mutated species traits matrix
#' @export
apply_evolution <- function(species, cluster_indices, space, config){
  stop("this function documents the user function interface only, do not use it")
}


loop_evolution <- function(config, data, vars){
  if(config$gen3sis$general$verbose>=3){
    cat(paste("entering mutation module \n"))
  }

  data$all_species <- lapply(data$all_species, evolve, data$space, data$distance, config)

  if(config$gen3sis$general$verbose>=3){
    cat(paste("exiting mutation module \n"))
  }
  return(list(config = config, data = data, vars = vars))
}


evolve <- function(species, space, distance_matrix, config){
  if (!length(species[["abundance"]])) {
    return(species)
  }
  species_presence <- names(species[["abundance"]])

  distances <- config$gen3sis$dispersal$get_dispersal_values(length(species_presence), species, space, config)

  permutation <- sample(1:length(species_presence), length(species_presence))
  cluster_indices <- Tdbscan_variable(distance_matrix[species_presence[permutation],species_presence[permutation],
                                                         drop=FALSE], distances, 1)
  cluster_indices <- cluster_indices[order(permutation)]

  new_traits <- config$gen3sis$mutation$apply_evolution(species, cluster_indices, space, config)

  species_traits <- colnames(species[["traits"]])
  species[["traits"]][ , species_traits] <- new_traits[, species_traits, drop=FALSE]

  return(species)
}



#' No evolution considered
#'
#' @param species the current species
#' @param cluster_indices indices to assign cells to geographic clusters
#' @param space the current space
#' @param config the general config
#' @return returns an invisible empty species traits when no evolution is considered
#'
#' @export
evolution_mode_none <- function(species, cluster_indices, space, config){
  return(invisible(species[["traits"]]))
}

