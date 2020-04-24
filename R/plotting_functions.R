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
  
  {
  layout( matrix(c(1,3,3,1,3,3,2,2,2,2,2,2),ncol=3, byrow =T)  )
  layout.show(3)
  par(mar=c(7.3,3,0,7.5), oma=c(0.3,0.8,0.3,0.8))
  
  # summary text
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10),axes=FALSE,ann=FALSE)
  #title
  text(1,10, paste("Simulation", ), font=2)
  
  # time behaviour
  d <- output$summary$phylo_summary[-1,-1]
  plot( d[,"alive"],  xlab="", ylab="", type='l', col="black", lwd=4, frame.plot = FALSE, xaxt='n', yaxt='n')
  
  
  axis(4,line=-1, cex=1, cex.axis=1, col="black")
  mtext(side = 4, text = "Species richness", col = "black", line = 2, cex=1.1)
  
  par(new=TRUE)
  plot( d[,"speciations"],  pch=3, col=rgb(0,0,1, 0.5), xlab="", ylab="", type='b',frame.plot = FALSE, xaxt='n', yaxt='n', ylim=range(d[,c("speciations", "extinctions")]))
  points(d[,"extinctions"], pch=4, col=rgb(1,0,0, 0.5), type="b")
  
  axis(2,line=-1, cex=1, cex.axis=1, col="black")
  mtext(side = 2, text = "Evolutionary events", col = "black", line = 1.5, cex=1.1)
  
  legend(x=1, y=max(d[,c("speciations", "extinctions")]), legend=c("Speciation", "Extinction"), col=c(rgb(0,0,1, 0.5), rgb(1,0,0, 0.5)), pch=c(3,4),  bty = "n")
  
  axis(1)
  mtext(side=1, text="Time steps", line=2.5, cex=1.1)
  
  
  # richness map
  col_vec <- colorRampPalette(c( "snow2", "yellow", "orange" ,"red", "darkred", "chocolate4")  )(max(output$summary$`richness-final`[,3], na.rm=T))
  
  image(rasterFromXYZ(output$summary$`richness-final`), col=col_vec, bty = "n", xlab = "", ylab = "", main="bla")
  title("Species richness map at final step", line=-1)
  plot(rasterFromXYZ(output$summary$`richness-final`), legend.only=T, add=T,col=col_vec)
  
  }
  

  
  
       #      , lwd=3, frame.plot = FALSE, xaxt='n', yaxt='n')
  
  # b 
  # layout( matrix(c(1,2,2,1,2,2,3,3,3,3,3,3),ncol=3, byrow =T)  )
  # layout.show(3)
  # par(mar=c(7.3,3,0,7.5), oma=c(0.3,0.8,0.3,0.8))
  # # TEMP
  # # Base plot!
  # plot(mean_vars[,c("Ma", "Mean_Temperature")], xlim=rev(range(mean_vars[, "Ma"], na.rm=T)),  xlab="", ylab="", ylim=c(range_mean_temp_plot[1], range_mean_temp_plot[2]), type='l', col="red" 
  #      , lwd=3, frame.plot = FALSE, xaxt='n', yaxt='n')
  # at = axTicks(2)
  # axis(2,labels = FALSE, line=-2.5, cex=2, cex.axis=3, col="red")
  # mtext(side = 2, text = at, at = at, col = "red", line = -1, cex=1.5)
  # points(mean_vars[which(round(mean_vars[,"Ma"],2)==as.numeric(formatedyear)), "Ma"], 
  #        mean_vars[which(round(mean_vars[,"Ma"],2)==as.numeric(formatedyear)),"Mean_Temperature"]
  # 
  
  
  
  
  categories <- c("alive", "speciation", "extinctions")
  colours <- c("black", "blue", "red")
  matplot(output$summary$phylo_summary[-1,-1], line = 0.5,
          type = "o", 
          lty = 1, 
          pch = 2:4,
          col = colours,
          xlab = "timestep",
          ylab = "Eevents",
          main = "Species development through time")
  
  legend("topright", col=colours, categories, bg="white", lwd=1, pch=1:4)
  
  # c
  
  
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
