# Copyright (c) 2020, ETH Zurich


#' apply_evolution let's the user mutate the traits of a given species
#'
#' @details This function is called for any single species alongside an index for
#' the geographical clusters within the species.
#'
#' @param species The current species to mutate
#' @param cluster_indices an index vector indicating the cluster every occupied cell is part of
#' @param landscape the current landscape
#' @param config the current config
#'
#' @return the mutated species traits matrix
#' @export
apply_evolution <- function(species, cluster_indices, landscape, config){
  stop("this function documents the user function interface only, do not use it!")
}


loop_evolution <- function(config, data, vars){
  if(config$gen3sis$general$verbose>=3){
    cat(paste("entering mutation module \n"))
  }

  data$all_species <- lapply(data$all_species, evolve, data$landscape, data$distance, config)

  if(config$gen3sis$general$verbose>=3){
    cat(paste("exiting mutation module \n"))
  }
  return(list(config = config, data = data, vars = vars))
}


evolve <- function(species, landscape, distance_matrix, config){
  if (!length(species[["abundance"]])) {
    return(species)
  }
  species_presence <- names(species[["abundance"]])
  distances <- config$gen3sis$dispersal$get_dispersal_values(length(species_presence), species, landscape, config)

  permutation <- sample(1:length(species_presence), length(species_presence))
  cluster_indices <- Tdbscan_variable(distance_matrix[species_presence[permutation],species_presence[permutation],
                                                         drop = F], distances, 1)
  cluster_indices <- cluster_indices[order(permutation)]

  new_traits <- config$gen3sis$mutation$apply_evolution(species, cluster_indices, landscape, config)

  species_traits <- colnames(species[["traits"]])
  species[["traits"]][ , species_traits] <- new_traits[, species_traits, drop = F]

  return(species)
}



#' evolution_mode_none
#'
#' @param species the current species
#' @param cluster_indices indices to assign cells to geographic clusters
#' @param landscape the current landscape
#' @param config the general config
#'
#' @export
evolution_mode_none <- function(species, cluster_indices, landscape, config){
  return(invisible(species[["traits"]]))
}

