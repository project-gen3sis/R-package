# Copyright (c) 2020, ETH Zurich

#' create_landscape creates an object representing the current landscape for a given timestep
#'
#' @details This is internal representation for a given landscape. Int consists of an id for the current timestep,
#' an environmental matrix containing the environmental conditions for each suitable cell,
#' and a coordinates matrix containing the cell-centre coordinates of all suitable cells
#'
#' @param id the current timestep ti
#' @param timestep the current timestep name
#' @param environment a matrix containing the environmental conditions
#' @param coordinates a matrix containg the cell-centre coordinates
#' @param extent the extent of the overall landscape
#' @param resolution the resolution of the input in coords/enironment
#'
#' @return returns a landscape of class "gen3sis_landscape"
#' @noRd
create_landscape <- function(id, timestep, environment, coordinates, extent = NA, resolution = NA) {
  landscape <- list()
  landscape[["id"]] <- id
  landscape[["timestep"]] <- timestep
  landscape[["environment"]] <- environment
  landscape[["coordinates"]] <- coordinates
  landscape[["extent"]] <- extent
  landscape[["resolution"]] <- resolution
  class(landscape) <- "gen3sis_landscape"
  return(invisible(landscape))
}

