# Copyright (c) 2020, ETH Zurich

#' Creates an object representing the current landscape for a given time-step
#'
#' @details This function generates the internal representation of a given landscape. It 
#' consists of an identifier for the input time-step, an environmental matrix containing the environmental 
#' conditions for each suitable site, and a coordinates matrix containing the site cell center coordinates of 
#' all suitable sites
#'
#' @param id the identifier of the input time-step
#' @param timestep the name of the input time-step 
#' @param environment a matrix containing the environmental conditions
#' @param coordinates a matrix containing the coordinates of the cells centers
#' @param extent the extent of the landscape
#' @param resolution the spatial resolution of the input 
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

