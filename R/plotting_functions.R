# Copyright (c) 2020, ETH Zurich

#' Plot a species' presence on a given landscape
#'
#' @param species a single species object
#' @param landscape a landscape object
#' @example inst/examples/plot_species_presence_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_species_presence <- function(species, landscape) {
  presence <- species[["abundance"]]
  presence[] <- 1
  conditional_plot(paste0("species_presence_", species$id),
                   landscape,
                   plot_raster_single,
                   presence,
                   landscape,
                   paste("Species", species[["id"]]))
}


#' Plot a species' abundance on a given landscape
#'
#' @param species a single species object
#' @param landscape a landscape object
#' @example inst/examples/plot_species_abundance_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_species_abundance <- function(species, landscape) {
  presence <- species[["abundance"]]
  conditional_plot(paste0("species_abundance_", species$id),
                   landscape,
                   plot_raster_single,
                   presence,
                   landscape,
                   paste("Abundance Species", species[["id"]]))
}




#' Plot the environment variable of a given landscape
#'
#' @param landscape the landscape to plot the environment from
#' @return no return value, called for plot
#'
#' @export
plot_landscape <- function(landscape) {
  conditional_plot(title = "landscape",
                   landscape = landscape,
                   plot_fun = plot_raster_multiple,
                   landscape[["environment"]],
                   landscape)
}


#' Plot the outline of a given landscape over time
#'
#' @param landscape the input landscape to be plotted
#' @param slices the amount of slices though time between start and end (default value is 2).
#' @param start_end_times the stating and ending times of the simulation (default is NULL, takes the oldest and most recent available)
#' @return no return value, called for plot
#'
#' @export
plot_landscape_overview <- function(landscape, slices=2, start_end_times=NULL) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  landscape <- landscape[[1]] # takes only the first one
  if (is.null(start_end_times)){ # takes last and first time-steps
    start_end_times <- which(colnames(landscape)%in%colnames(landscape)[c(3,ncol(landscape))])
  }
  
  times=rev(round(seq(from=start_end_times[1], to=start_end_times[2], length.out = slices+2  ),0))
  par(mfrow=c(1,length(times)))
  for (times_i in 1:length(times)){
    # times_i <- 1
    plot(rasterFromXYZ(landscape[,c(1,2,times[times_i])]), col="black", 
         axes=FALSE,legend=FALSE, tck = 0, main=colnames(landscape)[times[times_i]], line=-3)
  }
}


#' Plot simulation default summary object
#'
#' @param output a sgen3sis output object resulting from a gen3sis simulation (i.e. run_simulation)
#' @param summary_title summary plot title as character. If NULL, title is computed from input name.
#' @param summary_legend either a staring with _\_n for new lines or NULL. If NULL, provides default summary and simulation information.
#' @seealso \code{\link{run_simulation}}   
#' @example inst/examples/plot_summary_help.R
#' @importFrom graphics layout legend axis mtext points
#' @importFrom grDevices rgb colorRampPalette
#' @importFrom stringr str_split str_remove
#' @return no return value, called for plot
#' 
#' @export
plot_summary <- function(output, summary_title=NULL, summary_legend=NULL) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  if (class(output)!="gen3sis_output"){
    stop("this is not  a gen3sis_output object")
  }
  
  {
  layout( matrix(c(1,3,3,
                   1,3,3,
                   1,3,3,
                   2,2,2,
                   2,2,2),ncol=3, byrow =TRUE)  )
  #layout.show(3)
  par(mar=c(4,3,3,7), oma=c(0.1,0.8,0.3,0.8))
  
  # summary text
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), axes=FALSE, ann=FALSE)
  #summary title
  if (is.null(summary_title)){
    summary_title <- str_split(output$parameters$directories$input, "/")[[1]]
    summary_title <- paste(tail(summary_title,2)[1], collapse = ">")
    #summary_title <- str_remove(summary_title, "..>")
    #summary_title <- str_remove(summary_title, ".>")
    #summary_title <- str_remove(summary_title, ".")
  }
  #summary text
  if (is.null(summary_legend)){
    sum_names <- names(output$parameters$gen3sis$general)
    sumss <- output$parameters$gen3sis$general
    sumar <- output$summary
    phylo <- sumar$phylo_summary[c(1,nrow(sumar$phylo_summary)),]
    col_sum <- colSums(sumar$phylo_summary)
    summary_legend=paste(
      paste(sum_names[2], 
            sumss[2], sep=": "),
      #paste(sum_names[3],
      #      sumss[3], sep=": "),
      paste('end_time;', tail(names(sumar$occupancy), 1)),
      paste("traits", 
            paste0(sumss[7][[1]], collapse = ","), sep=": "),
      paste("world_habited_present", 
            paste0(round(sumar$occupancy[length(sumar$occupancy)],1)*100,"%"), sep=": "),
      paste("initial_richness", 
            phylo[1,"total"], sep=": "),
      paste("cumulative_richness", 
            phylo[2,"total"], sep=": "),
      paste("richness_present", 
            phylo[2,"alive"], sep=": "),
      paste("speciation", 
            paste0(round((col_sum["speciations"]-phylo["initial", "total"])/phylo[1, "total"]*100, 0),"%"), sep=": "),
      paste("extinction", 
            paste0(round((col_sum["extinctions"])/phylo[1, "total"]*100, 0),"%"), sep=": "),
      paste(names(output$system)[1], 
            round(output$system$`runtime-hours`,2), sep=": "),
      sep="\n")
  }
  #plot summary legend
  par(xpd=TRUE)
  
  legend("topleft", inset=c(0,-0.4), title=paste0("Summary [",summary_title,"]"), legend=summary_legend, bty="n", title.adj=0)

  #plot time behavior
  d <- output$summary$phylo_summary[-1,-1]
  plot( d[,"alive"],  xlab="", ylab="", type='l', col="black", lwd=4, frame.plot = FALSE, xaxt='n', yaxt='n')
  axis(4,line=-1, cex=1, cex.axis=1, col="black")
  mtext(side = 4, text = "\u03B3 richness", col = "black", line = 2, cex=1.1)
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
  axis_lab <- seq(from=1, to=nrow(d), length.out = max((nrow(d)/20),2))
  axis(1, at=axis_lab, labels = rownames(d)[axis_lab])
  mtext(side=1, text="Time steps", line=2.5, cex=1.1)
  
  # richness map
  #attribute collor
  ras <- rasterFromXYZ(output$summary$`richness-final`)
  max_ras <- max(ras@data@values, na.rm=TRUE)
  min_ras <- min(ras@data@values, na.rm=TRUE)
  # rc <- color_richness(max(ras@data@values, na.rm=TRUE) + 1)
  #terrain color
  zerorichness_col <- "navajowhite3"
  if (max_ras==0){ #if all extinct
    rc <-  zerorichness_col
  } else {
    rc <- color_richness(max_ras)
    if (min_ras==0){ #if there is zero-richness (i.e. inhabited sites)
      rc <- c(zerorichness_col, rc)
    }
  }
  
  
  
  image(ras, col=rc, bty = "o", xlab = "", ylab = "", las=1, asp = 1)
  mtext(4, text="Final \u03B1 richness", line=1, cex=1.2)
  raster::plot(rasterFromXYZ(output$summary$`richness-final`), legend.only=TRUE, add=TRUE,col=rc)
  }
}




#' Plot the richness of the given list of species on a landscape
#'
#' @param species_list a list of species to use in the richness calculation
#' @param landscape a corresponding landscape object
#' @example inst/examples/plot_richness_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_richness <- function(species_list, landscape) {
  richness <- get_geo_richness(species_list, landscape)
  max_richness <- max(richness, na.rm=TRUE)
  min_richnes <- min(richness, na.rm=TRUE)
  
  # attribute proper color scale
  zerorichness_col <- "navajowhite3"
  if (max_richness==0){ #if all extinct
    rc <-  zerorichness_col
  } else {
    rc <- color_richness(max_richness)
    if (min_richnes==0){ #if there is zero-richness (i.e. inhabited sites)
      rc <- c(zerorichness_col, rc)
    }
  }
  
  conditional_plot("richness",
                   landscape,
                   plot_raster_single,
                   richness,
                   landscape,
                   "richness",
                   col=rc)
}



#' Plot species ranges of the given list of species on a landscape
#'
#' @param species_list a list of species to use in the richness calculation
#' @param landscape a corresponding landscape object
#' @param disturb value randomly added to shift each species symbol. Useful to enhance visualization in case of multiple species overlaps  
#' @param max_sps maximum number of plotted species, not recommended above 20
#' @example inst/examples/plot_ranges_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_ranges <- function(species_list, landscape, disturb=0, max_sps=10) {
  disturb=abs(disturb)
  max_sps <- abs(max_sps)
  #plot landscape
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  layout( matrix(c(1,1,2),nrow=1, byrow =TRUE)  )
  #layout.show(2)
  #par(mar=c(4,3,3,7), oma=c(0.1,0.8,0.3,0.8))
  par(xpd = FALSE)
  raster::image(raster::rasterFromXYZ(cbind(landscape$coordinates,1)), main="species ranges", col="navajowhite3", asp = 1)
  n_species <- length(species_list)
  alive <- unlist(lapply(species_list, function(x){length(x$abundance)}))
  alive <- alive>0
  #visual combinations
  sp_cols <- c("#FF0000", "#FF4D00", "#FFE500", 
               "#CCFF00", "#80FF00", "#33FF00", 
               "#00FF19", "#00FF66", "#00FFB2", 
               "#00FFFF", "#00B3FF", "#0066FF", 
               "#001AFF", "#3300FF", "#7F00FF", 
               "#CC00FF", "#FF00E6", "#FF0099", 
               "#FF004D") #FIX
  sp_pchs <- 1:6 #FIX
  
  #limit plot
  omitted <- 0
  n_sps_max <- sum(alive)
  if (n_sps_max>max_sps){
    omitted <- n_sps_max-max_sps
    n_sps_max <- max_sps
  }
  # set to case
  cols <- rep(sp_cols, length.out=n_species)
  pchs <- rep(sp_pchs, length.out=n_species)
  par(xpd = TRUE)
  for (i in 1:n_sps_max){
    sp_i <- (1:n_species)[alive][i]
    img <- cbind(landscape[["coordinates"]][names(species_list[[sp_i]]$abundance),,drop=FALSE], species_list[[sp_i]]$id)
    df <- as.data.frame(img)
    plot_diturbance <- sample(seq(-disturb, disturb, by=0.01), 1)
    points(x=as.numeric(df$x)+plot_diturbance, y=as.numeric(df$y)+plot_diturbance, pch=pchs[sp_i], col=cols[sp_i])
  }
  # legend plotted species
  # empty plot
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), axes=FALSE, ann=FALSE)
  # legend
  par(xpd=TRUE)
  legend("top", inset=c(-0.15,0), title=paste(n_sps_max, "species", paste0("\n[", omitted, ' omitted]')), legend=(1:n_species)[alive][1:n_sps_max], pch=pchs[alive][1:n_sps_max], col=cols[alive][1:n_sps_max], bty="n")
}


#' Save plots if called from within a simulation run, display as well if run interactively
#'
#' @param title folder and file name
#' @param landscape current landscape
#' @param plot_fun plotting function to use (single or multiple rasters)
#' @param ... arguments for plot_fun
#'
#' @importFrom grDevices png
#' @importFrom methods is
#' @noRd
conditional_plot <- function(title, landscape, plot_fun, ...){
  fun_calls <- sys.calls()
  if (any(sapply(fun_calls, FUN = function(x){ is(x[[1]], "name") && "call_main_observer" == x[[1]]}))){
    # run during simulation save plot to file
    config <- dynGet("config")
    plot_folder <- file.path(config$directories$output, "plots", title)
    dir.create(plot_folder, showWarnings=FALSE, recursive=TRUE)
    file_name <- file.path(plot_folder, paste0(title, "_t_", landscape$id, ".png"))
    png(file_name)
    plot_fun(...)
    dev.off()
  }
  plot_fun(...)
}


#' Plot a single set of values onto a given landscape
#'
#' @param values a named list of values, the names must correspond to cells in the landscape
#' @param landscape a landscape to plot the values onto
#' @param title a title string for resulting plot, the time information will be taken and appended from the landscape id
#' @param no_data what value should be used for missing values in values
#' @param col corresponds to the \link{raster} col plot parameter. This can be omitted and colors are handled by raster::plot  
#' @param legend corresponds to the \link{raster} legend plot parameter. This can be omitted and legend is handled by raster::plot
#' @example inst/examples/plot_raster_single_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_raster_single <- function(values, landscape, title, no_data = 0, col, legend=TRUE) {
  img <- cbind(landscape[["coordinates"]], no_data)
  img[names(values), 3] <- values
  ras <- rasterFromXYZ(img)
  ras <- extend(ras, landscape[["extent"]])
  raster::plot(ras, main=paste0(title, ", t: ", landscape[["id"]]), col=col, legend=legend)
}


#' Plot a set of values onto a given landscape
#'
#' @param values a matrix of values with columns corresponding to sets of values, and rows corresponding to grid cells,
#' this will result in ncol(values) raster plots.
#' @param landscape a landscape to plot the values onto
#' @param no_data what value should be used for missing data present in the values parameter
#' @return no return value, called for plot
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


#' Define gen3sis richness color scale
#' @param n corresponds to the \link{colorRampPalette} parameter 
#' @return returns a \link{colorRampPalette} function with the gen3sis richness colors
#' @export
color_richness <- colorRampPalette(
  c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF",
    "#6DCD59FF", "#B4DE2CFF", "#FDE725FF", "#FFA500",   "#FF2900",   "#C40000",   "#8B0000", "#8B0000")
)
