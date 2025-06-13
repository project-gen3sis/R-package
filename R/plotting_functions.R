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
  #presence <- species[["abundance"]]
  #presence[] <- 1
  #get all locations
  all_presence <- landscape[["coordinates"]][,1, drop=T]
  all_presence[] <- 0
  all_presence[names(species[["abundance"]])] <- 1
  rc <- set_color(all_presence, type=landscape$`type`)
  conditional_plot(paste0("species_presence_", species$id),
                   landscape,
                   plot_single,
                   all_presence,
                   landscape,
                   paste("Species", species[["id"]]),
                   col=rc)
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
  all_presence <- landscape[["coordinates"]][,1, drop=T]
  all_presence[] <- 0
  all_presence[names(species[["abundance"]])] <- species[["abundance"]]
  rc <- set_color(all_presence, type=landscape$`type`)
  conditional_plot(paste0("species_abundance_", species$id),
                   landscape,
                   plot_single,
                   presence,
                   landscape,
                   paste("Abundance Species", species[["id"]]))
}




#' Plot the environment variable of a given landscape
#'
#' @param landscape the gen3sis_space to plot the environment from
#' @return no return value, called for plot
#'
#' @export
plot_landscape <- function(landscape) {
  conditional_plot(title = "landscape",
                   landscape = landscape,
                   plot_fun = plot_multiple,
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
#' @param summary_legend either a string using \\n for new lines or NULL. If NULL, provides default summary and simulation information.
#' @seealso \code{\link{run_simulation}}   
#' @example inst/examples/plot_summary_help.R
#' @importFrom terra rast values plot
#' @importFrom graphics layout legend axis mtext points
#' @importFrom grDevices rgb colorRampPalette
#' @importFrom stringr str_split str_remove
#' @return no return value, called for plot
#' 
#' @export
plot_summary <- function(output, summary_title=NULL, summary_legend=NULL) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  if (!is(output, "gen3sis_output")){
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
  mtext(side = 4, text = paste("gamma", "richness"), col = "black", line = 2, cex=1.1)
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
  #ras <- rasterFromXYZ(output$summary$`richness-final`)
  ras <- terra::rast(output$summary$`richness-final`,type="xyz")
  max_ras <- max(terra::values(ras), na.rm=TRUE)
  min_ras <- min(terra::values(ras), na.rm=TRUE)
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
  mtext(4, text=paste("Final", "alpha", "richness"), line=1, cex=1.2)

  par(new=TRUE, fig=c(0.55, 1, 0.45, 0.95), mar=c(0, 0, 0, 0))
  terra::plot(ras,
              legend.only=TRUE, add=TRUE, col=rc,
              type="continuous", range = c(min_ras, max_ras),
              plg = list(size = c(0.8,0.8)))
  #raster::plot(rasterFromXYZ(output$summary$`richness-final`), legend.only=TRUE, add=TRUE,col=rc)
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
  #attribute color
  rc <- set_color(richness, type=landscape$`type`)
  conditional_plot("Richness",
                   landscape,
                   plot_single,
                   richness,
                   landscape,
                   "richness",
                   col=rc)
}


#' Set the color scale for plots, adding zero_col if zero values are present
#' @param values a vector of values
#' @param colfun a color function to use, default is color_richness, 
#' consider using color_richness_CVDCBP for color-blind safe colors
#' @param zero_col a color to use for zero values, default is "navajowhite3"
#' @param type a string, see \code{\link{check_spaces}} for options or use \code{check_spaces()$type}
#' @return if type is "raster" the function returns a color scale, if type is "points" the function returns a vector of colors
#' @export
#' @example inst/examples/set_color_help.R
set_color <- function(values, colfun=color_richness, zero_col="navajowhite3", type="raster"){
  max_val <- max(values, na.rm=TRUE)
  min_val <- min(values, na.rm=TRUE)
  if (max_val==0){
    rc <- zero_col
  } else {
    rc <- colfun(max_val)
    if (min_val==0){
      rc <- c(zero_col, rc)
    }
  }
  if (type%in%c("raster")){
    return(rc)
  } else {
    cols_cut <- cut(values,length(rc))
    colors <- rc[cols_cut]
    return(colors)
  }
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
  plot_points_single(1, landscape, title, no_data = 0, col="navajowhite3", title="species ranges")
  # plot_points(, main="species ranges", col=, asp = 1))
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
#' @param plot_fun plotting function to use (single or multiple)
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



# Generic plot single function
plot_single <- function(x, ...) {
  # Convert '...' to a list to access additional arguments
  args <- list(...)
  check_args(args)
  # Dispatch based on the class of the second argument  
  # Assuming 'x' is the first argument and we need the second for dispatch,
  # which is the first in 'args'
  UseMethod("plot_single", args[[1]])
}


#' Plot a single set of values in a given landscape
#'
#' @param values a named list of values, the names must correspond to cells in the landscape
#' @param landscape a landscape to plot the values onto
#' @param title a title string for resulting plot, the time information will be taken and appended from the landscape id
#' @param no_data what value should be used for missing values in values
#' @param col corresponds to the \link{raster} col plot parameter. This can be omitted and colors are handled by raster::plot  
#' @param legend corresponds to the \link{raster} legend plot parameter. This can be omitted and legend is handled by raster::plot
#' @example inst/examples/plot_single_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_single.gen3sis_space_raster <- function(values, landscape, title, no_data = 0, col, legend=TRUE) {
  img <- cbind(landscape[["coordinates"]], no_data)
  img[names(values), 3] <- values
  ras <- terra::rast(img, type="xyz")
  # extend raster to landscape extent in order to avoid flickering when animating
  ras <- terra::extend(ras, terra::ext(landscape[["extent"]]), fill=NA)
  terra::plot(ras, main=paste0(title, " ", landscape$timestep, " t_", landscape[["id"]]), col=col, legend=legend)
}

# TODO update documentation
#' Plot a single set of values onto a given landscape
#'
#' @param values a named list of values, the names must correspond to cells in the landscape
#' @param landscape a landscape to plot the values onto
#' @param title a title string for resulting plot, the time information will be taken and appended from the landscape id
#' @param no_data what value should be used for missing values in values
#' @param col corresponds to the \link{raster} col plot parameter. This can be omitted and colors are handled by raster::plot  
#' @param legend corresponds to the \link{raster} legend plot parameter. This can be omitted and legend is handled by raster::plot
#' @example inst/examples/plot_single_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_single.gen3sis_space_points <- function(values, landscape, title="", no_data = 0, col, legend=TRUE) {
  plot(landscape[["coordinates"]], 
       main=paste0(title, " ", landscape$timestep, " t_", landscape[["id"]]),
       xlim=landscape[["extent"]][c("xmin","xmax")],
       ylim=landscape[["extent"]][c("ymin","ymax")],
       col=col, pch=20)
}

# TODO update documentation
#' Plot a single set of values onto a given landscape
#'
#' @param values a named list of values, the names must correspond to cells in the landscape
#' @param landscape a landscape to plot the values onto
#' @param title a title string for resulting plot, the time information will be taken and appended from the landscape id
#' @param no_data what value should be used for missing values in values
#' @param col corresponds to the \link{raster} col plot parameter. This can be omitted and colors are handled by raster::plot  
#' @param legend corresponds to the \link{raster} legend plot parameter. This can be omitted and legend is handled by raster::plot
#' @example inst/examples/plot_single_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_single.gen3sis_space_h3 <- function(values, landscape, title="", no_data = 0, col, legend=TRUE) {
  spatial_points <- sf::st_as_sf(as.data.frame(landscape$coordinates), coords = c("x", "y"), crs = 4326)
  # TODO pass crs here, review and standardize crs handling and protocol
  cells <- h3jsr::point_to_cell(spatial_points, landscape$type_spec_res)
  polygons <- h3jsr::cell_to_polygon(cells)
  #polygons <- h3jsr::cell_to_polygon(h3::geo_to_h3(landscape$coordinates, landscape$type_spec_res))
  # if the extent is global, wrap the dateline
  if (landscape$extent[["xmin"]] == -180 & landscape$extent[["xmax"]]==180){
    # wrap dateline
    polygons <- sf::st_wrap_dateline(polygons, options= c('WRAPDATELINE=YES', 'DATELINEOFFSET=180'), quiet=TRUE)
  }
  # Add the values to the polygons for plotting
  # polygons$values <- values  # Add the values as an attribute to polygons
  plot(polygons,
       main=paste0(title, " ", landscape$timestep, " t_", landscape[["id"]]),
       xlim=c(landscape$extent[1], landscape$extent[2]), ylim=c(landscape$extent[3], landscape$extent[4]), 
       xlab="", ylab="", col=col, border=NA)
  
}

# Generic plot multiple function 
plot_multiple <- function(x, ...) {
  # Convert '...' to a list to access additional arguments
  args <- list(...)
  check_args(args)
  # Dispatch based on the class of the second argument  
  # Assuming 'x' is the first argument and we need the second for dispatch,
  # which is the first in 'args'
  UseMethod("plot_multiple", args[[1]])
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
plot_multiple.gen3sis_space_raster <- function(values, landscape, no_data = 0) {
  img <- matrix(no_data,
                nrow = nrow(landscape[["coordinates"]]),
                ncol = ncol(values) + 2,
                dimnames = list(rownames(landscape[["coordinates"]]),
                                c(colnames(landscape[["coordinates"]]),
                                  colnames(values))))
  img[, 1:2] <- landscape[["coordinates"]]
  img[rownames(values), -c(1:2)] <- values
  
  ras <- terra::rast(img, type="xyz")
  terra::ext(ras) <- landscape$extent
  #terra::res(ras) <- landscape$type_spec_res
  terra::plot(ras, main=paste0(colnames(values), " ", landscape$timestep, " ts ", landscape[["id"]]))
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
plot_multiple.gen3sis_space_h3 <- function(values, landscape, no_data = 0) {
  # Creates a matrix with coordinates and values  
  env_mtx <- cbind(landscape$coordinates, values)
  
  # Convert to points
  env_points <- sf::st_as_sf(env_mtx |> as.data.frame(), coords = c("x", "y"), crs = 4326)
  
  # and extract the h3 cell indexes
  env_cells <- h3jsr::point_to_cell(env_points,landscape$type_spec_res)
  
  # Creates a base for the plot
  base_hexagons <- h3jsr::cell_to_polygon(env_cells) |>
    sf::st_union() |>
    sf::st_convex_hull() |>
    h3jsr::polygon_to_cells(res = landscape$type_spec_res)
  
  # Creates the polygons
  base_hexagons <- unlist(base_hexagons)[!unlist(base_hexagons)%in%env_cells] |>
    h3jsr::cell_to_polygon()
  
  env_hexagons <- h3jsr::cell_to_polygon(env_cells)
  
  envar_name <- colnames(values)
  
  # unite values and polygons
  full_hexagons <- c(env_hexagons, base_hexagons)
  
  base_values <- matrix(
    data = no_data,
    nrow = length(base_hexagons),
    ncol = ncol(values)
  )
  
  colnames(base_values) <- colnames(values)
  
  full_values <- rbind(values, base_values)

  # creates the final h3 and plot
  combined_h3 <- sf::st_sf(full_values, geometry = full_hexagons)
  plot(combined_h3)
}

#' Ensure there is at least one additional argument provided after 'x'
#' @param args a list of arguments
#' @return no return value, called for plot routine
#' @noRd
check_args <- function(args){
  # Ensure there is at least one additional argument provided after 'x'
  if (length(args) < 1) {
    stop("No gen3sis_space provided for dispatch as second parameter, please make sure you add one")
  }
}

#' Define gen3sis richness color scale for non colour-vision deficient
#' @param n corresponds to the \link{colorRampPalette} parameter 
#' @return returns a \link{colorRampPalette} function with the gen3sis richness colors
#' @export
#' @noRd
color_richness_CVDCBP <- colorRampPalette(
  c("#B2F2FD", "#81EEEA", "#61E5C9", "#63DAA0", "#73CE79", "#85BF51", "#94AD2F", "#9B951B",
    "#9C7E1F", "#9A692B", "#985538", "#944444", "#933251", "#901F61", "#8C0172")
)


#' Define richness color scale which is colour-vision deficient and colour-blind people safe based on scientific colour maps by Fabio Crameri
#' @param n corresponds to the \link{colorRampPalette} parameter 
#' @return returns a \link{colorRampPalette} function with the gen3sis richness colors
#' @export
#' @noRd
color_richness <- colorRampPalette(
  c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF",
    "#6DCD59FF", "#B4DE2CFF", "#FDE725FF", "#FFA500",   "#FF2900",   "#C40000",   "#8B0000", "#8B0000")
)
