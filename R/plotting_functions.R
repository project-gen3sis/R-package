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
#' @param output tsgen3sis output object resulting from a gen3sis simulation 
#' @param summary_legend either a satring with _\_n for new lines or NULL. If NULL, provides default summary and simulation information.
#' @example inst/examples/plot_summary_help.R
#' @importFrom graphics layout legend axis mtext points
#' @importFrom grDevices rgb colorRampPalette
#' @importFrom stringr str_split str_remove
#' @export
plot_summary <- function(output, summary_legend=NULL) {
  if (class(output)!="gen3sis_output"){
    stop("this is not  a gen3sis_output object")
  }
  
  {
  layout( matrix(c(1,3,3,1,3,3,2,2,2,2,2,2),ncol=3, byrow =T)  )
  #layout.show(3)
  par(mar=c(7.3,3,0,7.5), oma=c(0.3,0.8,0.3,0.8))
  
  # summary text
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), axes=FALSE, ann=FALSE)
  title <- str_split(output$parameters$directories$input, "/")[[1]]
  title <- paste(title, collapse = ">")
  title <- str_remove(title, "..>")
  title <- str_remove(title, ".>")
  title <- str_remove(title, ".")
  #title and summary text
  if (is.null(summary_legend)){
    legend(-1,10, title=title,legend=paste(
      paste(names(output$parameters$gen3sis$general[2]), output$parameters$gen3sis$general[2], sep=": "),
      paste(names(output$parameters$gen3sis$general[3]), output$parameters$gen3sis$general[3], sep=": "),
      paste(names(output$system)[1], round(output$system$`runtime-hours`,3), sep=": "),
      paste("world_habited_present", paste0(round(output$summary$occupancy[length(output$summary$occupancy)],1)*100,"%"), sep=": "),
      paste("cumulative_richness", output$summary$phylo_summary[nrow(output$summary$phylo_summary),"total"], sep=": "),
      paste("extinction", paste0(round(((output$summary$phylo_summary[nrow(output$summary$phylo_summary),"total"]-output$summary$phylo_summary[nrow(output$summary$phylo_summary),"alive"])/output$summary$phylo_summary[nrow(output$summary$phylo_summary),"total"])*100,0),"%"), sep=": "),
      sep="\n"),
      bty="n")
  } else {
    legend(-1.5,13, title=title, legend=summary_legend,
        bty="n")
  }

  
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
  legend(x=1, y=max(d[,c("speciations", "extinctions")]), 
         legend=c("Richness", "Speciation", "Extinction"), 
         col=c("black",rgb(0,0,1, 0.5), rgb(1,0,0, 0.5)), 
         pch=c(NA,3,4),
         lty = c(1, NA, NA),
         lwd=c(4,NA,NA),
         bty = "n")
  axis(1)
  mtext(side=1, text="Time steps", line=2.5, cex=1.1)
  
  
  # richness map
  col_vec <- colorRampPalette(c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF",
                                "#6DCD59FF", "#B4DE2CFF", "#FDE725FF", "#FFA500",   "#FF2900",   "#C40000",   "#8B0000", "#8B0000")
                              )(max(output$summary$`richness-final`[,3], na.rm=T))
    
  
  image(rasterFromXYZ(output$summary$`richness-final`), col=col_vec, bty = "n", xlab = "", ylab = "")
  #title("Species richness map at final step", line=-1)
  mtext(4, text="Species richness at final step", line=1, cex=1.2)
  raster::plot(rasterFromXYZ(output$summary$`richness-final`), legend.only=T, add=T,col=col_vec)
  }
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
  col_vec <- colorRampPalette(c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF",
                                "#6DCD59FF", "#B4DE2CFF", "#FDE725FF", "#FFA500",   "#FF2900",   "#C40000",   "#8B0000", "#8B0000")
  )(max(output$summary$`richness-final`[,3], na.rm=T))
  raster::plot(ras, main=paste0(title, ", t: ", landscape[["id"]]), col=col_vec)
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
  raster::plot(ras, main=paste0(colnames(values), ", t: ", landscape[["id"]]))
}
