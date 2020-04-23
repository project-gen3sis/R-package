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
#' @param start_end_times the stating and ending times of the simulation (default is NULL, takes the oldest and most recent avaiable)
#'
#' @export
plot_landscape_overview <- function(landscape, slices=2, start_end_times=NULL) {

  landscape <- landscape[[1]] # takes only the first one
  if (is.null(start_end_times)){ # takes last and first timesteps
    start_end_times <- which(colnames(landscape)%in%colnames(landscape)[c(3,ncol(landscape))])
  }
  
  times=rev(round(seq(from=start_end_times[1], to=start_end_times[2], length.out = slices+2  ),0))
  previous_par <- par(no.readonly = TRUE)
  par(mfrow=c(1,length(times)))
  for (times_i in 1:length(times)){
    # times_i <- 1
    plot(rasterFromXYZ(landscape[,c(1,2,times[times_i])]), col="black", 
         axes=F,legend=FALSE, tck = 0, main=colnames(landscape)[times[times_i]], line=-3)
  }
  par(previous_par)
}


#' Plot simulation default summary
#'
#' @param data the current data object 
#' @param vars the current vars object
#' @param config the current config
#'
#' @noRd
plot_summary <- function(output) {
  if (class(output)!="gen3sis_output"){
    stop("this is not  a gen3sis_output object")
  }
  # plotting of end of simulation goes here, like plotting the phylo_summary.
  grDevices::pdf(file=file.path(config$directories$output, "/EndConditions.pdf"), width=10, height=12)
  par(mfrow=c(2,1))
  plot_richness(data$all_species, data$landscape)
  
  categories <- c("total", "alive", "speciation", "extinctions")
  colours <- c("black", "magenta", "blue", "orange")
  matplot(data$summaries$phylo_summary[-1,], 
          type = "b", 
          lty = 1, 
          pch = 1:4,
          col = colours,
          xlab = "timesteps",
          ylab = "# of events",
          main = "Species development through time")
  
  legend("topright", col=colours, categories, bg="white", lwd=1, pch=1:4)
  
  grDevices::dev.off()
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
