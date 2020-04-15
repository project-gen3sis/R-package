# Copyright (c) 2020, ETH Zurich

#' plot a species' presence on a given landscape
#'
#' @param species a single species
#' @param landscape a landscape
#' @example inst/examples/plot_species_presence_help.R
#' @export
plot_species_presence <- function(species, landscape) {
  presence <- species[["abundance"]]
  presence[] <- 1
  plot_raster_single(presence, landscape, paste("Species", species[["id"]]))
}


#' plot the environment variable of a given landscape
#'
#' @param landscape the landscape to plot the environment from
#'
#' @export
plot_landscape <- function(landscape) {
  plot_raster_multiple(landscape[["environment"]],
                       landscape)
}


#' plot the outline of a given landscape over time
#'
#' @param landscape the input landscape to plot
#' @param slices the amount of slices though time between start and end (dafaul value is 2).
#' @param stat_end_times the stating and ending times of the simulation (default is NULL, takes the oldest and most recent avaiable)
#'
#' @export
plot_landscape_overview <- function(landscape, slices=2, start_end_times=NULL) {
  landscape=readRDS("S:/ffopp/gasm/input/world_scotese/data_driven/3_input_gen3sis_1d/landscapes.rds")
  landscape <- landscape[[1]] # takes only the first one
  if (is.null(start_end_times)){
    start_end_times <- colnames(landscape)[c(3,ncol(landscape))]
  }
  
  
  seq(1:ncol(landcas), length.out =  1    )
  
  
  plot_raster_multiple(landscape[["environment"]],
                       landscape)
}


#' Plot the richness of the given list of species on a landscape
#'
#' @param species_list a list of species to use in the richness calculation
#' @param landscape a landscape to plot the richness onto
#' @example inst/examples/plot_richness_help.R
#' @export
plot_richness <- function(species_list, landscape) {
  richness <- get_geo_richness(species_list, landscape)
  plot_raster_single(richness, landscape, "richness")
}


#' Plot a single set of values onto a given landscape
#'
#' @param values a named list of values, the names must correspond to cells in the landscape
#' @param landscape a landscape to plot the values onto
#' @param title a title string for resulting plot, the time will be taken and appended from the landscape id
#' @param no_data what value should be used for missing values in values
#' @example inst/examples/plot_raster_single_help.R
#' @export
plot_raster_single <- function(values, landscape, title, no_data = 0) {
  img <- cbind(landscape[["coordinates"]], no_data)
  img[names(values), 3] <- values
  ras <- rasterFromXYZ(img)
  ras <- extend(ras, landscape[["extent"]])
  plot(ras, main=paste0(title, ", t: ", landscape[["id"]]))
}


#' Plot a set of values onto a given landscape
#'
#' @param values a matrix of values with columns coresponding to sets of values, and rows corresponding to grid cells,
#' this will result in ncol(values) raster plots.
#' @param landscape a landscape to plot the values onto
#' @param no_data what value should be used for missing values in values
#'
#' @export
plot_raster_multiple <- function(values, landscape, no_data = 0) {
  img <- matrix(no_data,
                nrow = nrow(landscape[["coordinates"]]),
                ncol = ncol(values) + 2,
                dimnames = list(rownames(landscape[["coordinates"]]),
                                c(colnames(landscape[["coordinates"]]),
                                  colnames(values))))
  img[, 1:2] <- landscape[["coordinates"]]
  img[rownames(values), -c(1:2)] <- values
  ras <- rasterFromXYZ(img)
  ras <- extend(ras, landscape[["extent"]])
  plot(ras, main=paste0(colnames(values), ", t: ", landscape[["id"]]))
}
