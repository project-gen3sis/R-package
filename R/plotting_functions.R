# Copyright (c) 2020, ETH Zurich

#' Plot a species' presence on a given space
#'
#' @param species a single species object
#' @param space a space object
#' @param col a vector containing a color palette. For discrete values, the first element in the vector will be assigned to zero values. If NULL, gen3sis2 internal palette will be used. Default is NULL   
#' @example inst/examples/plot_species_presence_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_species_presence <- function(species, space, col=NULL) {
  #presence <- species[["abundance"]]
  #presence[] <- 1
  #get all locations
  all_presence <- space[["coordinates"]][,1, drop=T]
  all_presence[] <- 0
  all_presence[names(species[["abundance"]])] <- 1
  
  if(is.null(col)){
    col <- set_color(all_presence, type=space$`type`)
  }
  
  conditional_plot(paste0("species_presence_", species$id),
                   space,
                   plot_single,
                   all_presence,
                   space,
                   paste("Species", species[["id"]]),
                   col=col)
}


#' Plot a species' abundance on a given space
#'
#' @param species a single species object
#' @param space a space object
#' @param col a vector containing a color palette. For discrete values, the first element in the vector will be assigned to zero values. If NULL, gen3sis2 internal palette will be used. Default is NULL
#' 
#' @example inst/examples/plot_species_abundance_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_species_abundance <- function(species, space, col = NULL) {
  all_presence <- space[["coordinates"]][,1, drop=T]
  all_presence[] <- 0
  all_presence[names(species[["abundance"]])] <- species[["abundance"]]
  
  if(is.null(col)){
    col <- set_color(all_presence, type=space$`type`)
  }
  
  conditional_plot(paste0("species_abundance_", species$id),
                   space,
                   plot_single,
                   all_presence,
                   space,
                   paste("Abundance Species", species[["id"]]),
                   col=col)
}

#' Plot the environment variable of a given space
#'
#' @param space the gen3sis_space to plot the environment from
#' @param col a vector containing a color palette. For discrete values, the first element in the vector will be assigned to zero values. If NULL, gen3sis2 internal palette will be used. Default is NULL
#' 
#' @return no return value, called for plot
#'
#' @export
plot_space <- function(space, col = NULL) {
  
  if(is.null(col)){
    col <- color_richness(20)
  }

  conditional_plot(title = "space",
                   space = space,
                   plot_fun = plot_multiple,
                   space[["environment"]],
                   space,
                   col)
}


#' Plot the outline of a given space over time
#'
#' @param breaks numeric. A vector containing time slices to plot. If NULL, the older and the most recent time-steps will be used. Default is NULL
#' @param space the input space to be plotted
#' @param env_names character. A vector containing variable names to plot
#'
#' @importFrom ggplot2 ggplot scale_fill_gradientn scale_x_continuous scale_y_continuous theme_bw theme element_text element_line labs geom_sf aes element_blank scale_color_gradientn
#' @importFrom h3jsr point_to_cell cell_to_polygon
#' @importFrom patchwork wrap_plots plot_annotation wrap_elements
#' @importFrom scales alpha
#' @importFrom sf st_as_sf st_sf st_geometry
#' @importFrom terra rast
#' @importFrom tidyterra geom_spatraster
#' 
#' @return no return value, called for plot
#'
#' @export
plot_space_overview <- function(space, env_names = NULL, breaks = NULL) {
  # get the slices
  if(is.null(breaks)) {
    start_time <- space$meta$duration$from
    end_time <- space$meta$duration$to
    breaks <- c(start_time, end_time)
  } 
  
  # get the time unit
  unit <- space$meta$duration$unit
  
  # construct the breaks
  breaks <- paste0(breaks,unit)
    
  # breaks for x and y axis
  x_breaks <- seq(space$meta$area$extent[["xmin"]],space$meta$area$extent[["xmax"]], length.out = 7)
  y_breaks <- seq(space$meta$area$extent[["ymin"]],space$meta$area$extent[["ymax"]], length.out = 7)
  
  # gets the spaces type and environmental variables
  space_type <- space$meta$type
  env_vars <- space$env
  
  # if no variable was defined, take all of them
  if(is.null(env_names)) {
    env_names <- names(env_vars)
  }
  
  if (space_type == "raster") {
    plot_space_overview.raster(env_names, env_vars, breaks, x_breaks, y_breaks)
  } else if (space_type == "h3") {
    plot_space_overview.h3(env_names, env_vars, breaks, x_breaks, y_breaks)
  } else if (space_type == "points") {
    plot_space_overview.points(env_names, env_vars, breaks, x_breaks, y_breaks)
  }
}

#' Method-like function of plot_space_overview for raster spaces
#'
#' @param env_names character. A vector with variable names to include in plot.
#' @param env_vars matrix. A matrix containing the variables to plot.
#' @param breaks numeric. A vector containing time slices to plot. If NULL, the older and the most recent time-steps will be used. Default is NULL
#' @param x_breaks numeric. Break points of the x axis. Aesthetic only. 
#' @param y_breaks numeric. Break points of the y axis. Aesthetic only.
#'
#' @returns no return value, called for plot
#' @noRd
plot_space_overview.raster <- function(env_names, env_vars, breaks, x_breaks, y_breaks){
  # construct a plot for each timestep and variable
  plot_list <- lapply(env_names, function(vari){
    
    # Creates a matrix with coordinates and values
    vari_mtx <- env_vars[[vari]][,c("x","y",breaks)]
    temp_ras <- terra::rast(vari_mtx, type="xyz")
    
    # plot for each timestep
    plots <- lapply(names(temp_ras), function(v) {
      ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = temp_ras[[v]]) +
        ggplot2::scale_fill_gradientn(
          colors = color_richness(20),
          name = NULL,
          na.value = "transparent") +
        raster_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(v))
    })
    
    # wrap the each timestep plot and name them
    plot_group <- patchwork::wrap_plots(plots) +
      patchwork::plot_annotation(
        title = vari,
        theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
    
    # unite the plots in a single block
    patchwork::wrap_elements(full = plot_group)
  })
  
  # construct the final plot
  patchwork::wrap_plots(plot_list, ncol = 1)
}

#' Method-like function of plot_space_overview for h3 spaces
#'
#' @param env_names character. A vector with variable names to include in plot.
#' @param env_vars matrix. A matrix containing the variables to plot.
#' @param breaks numeric. A vector containing time slices to plot. If NULL, the older and the most recent time-steps will be used. Default is NULL
#' @param x_breaks numeric. Break points of the x axis. Aesthetic only. 
#' @param y_breaks numeric. Break points of the y axis. Aesthetic only.
#'
#' @returns no return value, called for plot
#' @noRd
plot_space_overview.h3 <- function(env_names, env_vars, breaks, x_breaks, y_breaks){
  # construct a plot for each timestep and variable
  plot_list <- lapply(env_names, function(vari){
    # Creates a matrix with coordinates and values
    vari_mtx <- env_vars[[vari]]
    coords <- vari_mtx[,c("x","y")]
    env_vars_values <- vari_mtx[,breaks]
    
    # Convert to points
    env_points <- sf::st_as_sf(as.data.frame(coords), coords = c("x", "y"), crs = 4326)
    
    # and extract the h3 cell indexes
    env_cells <- h3jsr::point_to_cell(env_points, space$meta$type_spec$res)
    polygons <- h3jsr::cell_to_polygon(env_cells)
    
    # a polygon with geometry and values
    polygons_sf <- sf::st_sf(
      value = env_vars_values,
      geometry = polygons
    )
    
    names(polygons_sf) <- gsub("value\\.","",names(polygons_sf))
    
    # convert to long format
    long_poly <- lapply(colnames(env_vars_values), function(time_step){
      temp_poly <- polygons_sf
      temp_poly$time_step <- time_step
      temp_poly$value <- temp_poly[[time_step]]
      temp_poly
    }) 
    
    long_poly <- do.call(rbind, long_poly)
    long_poly <- long_poly[,c("time_step","value")]
    
    # plot for each timestep
    plots <- lapply(unique(long_poly$time_step), function(v) {
      temp_polygons <- long_poly[long_poly$time_step == v, ] |> na.omit()
      
      ggplot2::ggplot(temp_polygons) +
        ggplot2::geom_sf(ggplot2::aes(fill = value), color = NA) +
        ggplot2::scale_fill_gradientn(
          colors = color_richness(20),
          name = NULL) +
        sf_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(v))
    })
    
    # wrap the plots and name them
    plot_group <- patchwork::wrap_plots(plots) +
      patchwork::plot_annotation(
        title = vari,
        theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
    
    # creates a block of plot elements
    patchwork::wrap_elements(full = plot_group)
  })
  
  # construct the final plot
  patchwork::wrap_plots(plot_list, ncol = 1)
}

#' Method-like function of plot_space_overview for points spaces
#'
#' @param env_names character. A vector with variable names to include in plot.
#' @param env_vars matrix. A matrix containing the variables to plot.
#' @param breaks numeric. A vector containing time slices to plot. If NULL, the older and the most recent time-steps will be used. Default is NULL
#' @param x_breaks numeric. Break points of the x axis. Aesthetic only. 
#' @param y_breaks numeric. Break points of the y axis. Aesthetic only.
#'
#' @returns no return value, called for plot
#' @noRd
plot_space_overview.points <- function(env_names, env_vars, breaks, x_breaks, y_breaks){
  # construct a plot for each timestep and variable
  plot_list <- lapply(env_names, function(vari){
    # Creates a matrix with coordinates and values
    vari_mtx <- env_vars[[vari]]
    coords <- vari_mtx[,c("x","y")]
    env_vars_values <- vari_mtx[,breaks]
    
    # Convert to points
    env_points <- sf::st_as_sf(as.data.frame(coords), coords = c("x", "y"), crs = 4326)
    
    # creates a sf with point geometry
    points_sf <- sf::st_sf(
      value = env_vars_values,
      geometry = sf::st_geometry(env_points)
    )
    
    names(points_sf) <- gsub("value\\.","",names(points_sf))
    
    # convert to long format
    long_point <- lapply(colnames(env_vars_values), function(time_step){
      long_point <- points_sf
      long_point$time_step <- time_step
      long_point$value <- long_point[[time_step]]
      long_point
    }) 
    
    long_point <- do.call(rbind, long_point)
    long_point <- long_point[,c("time_step","value")]
    
    # plots for each timestep
    plots <- lapply(unique(long_point$time_step), function(v) {
      temp_points <- long_point[long_point$time_step == v, ] |> na.omit()
      
      ggplot2::ggplot(temp_points) +
        ggplot2::geom_sf(ggplot2::aes(color = value)) +
        ggplot2::scale_color_gradientn(
          colors = color_richness(20),
          name = NULL) +
        sf_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(v))
    })
    
    # wrap and name
    plot_group <- patchwork::wrap_plots(plots) +
      patchwork::plot_annotation(
        title = vari,
        theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
    
    # creates a block of plot elements
    patchwork::wrap_elements(full = plot_group)
  })
  
  # construct the final plot
  patchwork::wrap_plots(plot_list, ncol = 1)
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

#' Plot the richness of the given list of species on a space
#'
#' @param species_list a list of species to use in the richness calculation
#' @param space a corresponding space object
#' @param col a vector containing a color palette. For discrete values, the first element in the vector will be assigned to zero values. If NULL, gen3sis2 internal palette will be used. Default is NULL
#' @example inst/examples/plot_richness_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_richness <- function(species_list, space, col = NULL) {
  richness <- get_geo_richness(species_list, space)
  #attribute color

  if(is.null(col)){
    col <- set_color(richness, type=space$`type`)
  }
  
  conditional_plot("Richness",
                   space,
                   plot_single,
                   richness,
                   space,
                   "richness",
                   col=col)
}


#' Set the color scale for plots, adding zero_col if zero values are present
#' @param values a vector of values
#' @param colfun a color function to use, default is color_richness, 
#' consider using color_richness_CVDCBP for color-blind safe colors
#' @param zero_col a color to use for zero values, default is "navajowhite3"
#' @param type a string, see \code{\link{check_spaces}} for options or use \code{check_spaces()$type} # TODO deprecated
#' @return if type is "raster" the function returns a color scale, if type is "points" the function returns a vector of colors
#' @export
#' @example inst/examples/set_color_help.R
set_color <- function(values, colfun=color_richness, zero_col="navajowhite", type="raster"){
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
  
  return(rc)
  # if (type%in%c("raster")){
  #   return(rc)
  # } else {
  #   cols_cut <- cut(values,length(rc))
  #   colors <- rc[cols_cut]
  #   return(colors)
  # }
}


#' Plot species ranges of the given list of species on a space
#'
#' @param species_list a list of species to use in the richness calculation
#' @param space a corresponding space object
#' @param disturb value randomly added to shift each species symbol. Useful to enhance visualization in case of multiple species overlaps  
#' @param max_sps maximum number of plotted species, not recommended above 20
#' @example inst/examples/plot_ranges_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_ranges <- function(species_list, space, disturb=0, max_sps=10) {
  disturb=abs(disturb)
  max_sps <- abs(max_sps)

  # Gets the plot base/background
  base_plot <- plot_single(1, space, title, no_data=0, col="navajowhite", title="species ranges")
  
  # compute the species to plot
  n_species <- length(species_list)
  alive <- unlist(lapply(species_list, function(x){length(x$abundance)}))
  alive <- alive>0
  
  omitted <- 0
  n_sps_max <- sum(alive)
  if (n_sps_max>max_sps){
    omitted <- n_sps_max-max_sps
    n_sps_max <- max_sps
  }
  
  # construct the legend
  legend_title <- paste(n_sps_max, "species", paste0("\n[", omitted, ' omitted]'))
  
  # get species coordinates
  spp_list <- lapply(1:n_sps_max, function(i){
    sp_coords <- space[["coordinates"]][names(species_list[[i]][["abundance"]]),]
    sp_coords <- cbind(sp_coords, rep(i, nrow(sp_coords)))
    colnames(sp_coords) <- c("x","y","shape")
    sp_coords <- as.data.frame(sp_coords)
    sp_coords$shape <- as.character(sp_coords$shape)
    sp_coords
  })
  spp_list <- do.call(rbind, spp_list)
  
  # get the colors and names
  shape_colors <- color_richness(n_sps_max)
  names(shape_colors) <- unique(spp_list$shape)
  
  # plot
  base_plot + # background
  ggplot2::geom_jitter( # the species symbols (shape and color)
    data = spp_list,
    mapping = ggplot2::aes(x = x, y = y, shape = shape, color = shape),
    size = 3,
    width = disturb,
    height = disturb
  ) +
  ggplot2::scale_color_manual(values = shape_colors) + # the color legend
  ggplot2::guides(fill = "none") + # clean previous legend
  ggplot2::labs( # set labels
    shape = legend_title, ## legend
    color = legend_title, ## legend
    x = ggplot2::element_blank(), ## nothing in x axis
    y = ggplot2::element_blank() ## nothing in y axis 
  )
}


#' Save plots if called from within a simulation run, display as well if run interactively
#'
#' @param title folder and file name
#' @param space current space
#' @param plot_fun plotting function to use (single or multiple)
#' @param ... arguments for plot_fun
#'
#' @importFrom grDevices png
#' @importFrom methods is
#' @importFrom ggplot2 ggsave
#' 
#' @noRd
conditional_plot <- function(title, space, plot_fun, ...){
  fun_calls <- sys.calls()
  if (any(sapply(fun_calls, FUN = function(x){ is(x[[1]], "name") && "call_main_observer" == x[[1]]}))){
    # run during simulation save plot to file
    config <- dynGet("config")
    plot_folder <- file.path(config$directories$output, "plots", title)
    dir.create(plot_folder, showWarnings=FALSE, recursive=TRUE)
    file_name <- file.path(plot_folder, paste0(title, "_t_", space$id, ".svg"))
    
    # svg(file_name)
    # p <- plot_fun(...)
    # p
    # dev.off()
    
    p <- plot_fun(...)
    # ggplot2::ggsave(filename = file_name,
    #                 plot = p,
    #                 width = 1400,
    #                 height = 1080,
    #                 units = "px",
    #                 dpi = 300)
    ratio <- (space$extent[["ymax"]] - space$extent[["ymin"]]) / (space$extent[["xmax"]] - space$extent[["xmin"]]) 
    ggplot2::ggsave(filename = file_name,
                    plot = p,
                    width = (1400/(ratio)),
                    height = 1080,
                    units = "px",
                    dpi = 300)
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


#' Plot a single set of values in a given space
#'
#' @param values a named list of values, the names must correspond to cells in the space
#' @param space a space to plot the values onto
#' @param title a title string for resulting plot, the time information will be taken and appended from the space id
#' @param no_data what value should be used for missing values in values
#' @param col a vector containing a color palette. For discrete values, the first element in the vector will be assigned to zero values. If NULL, gen3sis2 internal palette will be used. Default is NULL
#' @param legend corresponds to the \link{raster} legend plot parameter. This can be omitted and legend is handled by raster::plot
#' 
#' @importFrom ggplot2 ggplot scale_fill_manual scale_x_continuous scale_y_continuous theme_bw theme element_text element_line labs scale_fill_gradientn
#' @importFrom scales alpha
#' @importFrom terra rast extend ext values
#' @importFrom tidyterra geom_spatraster
#' 
#' @example inst/examples/plot_single_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_single.gen3sis_space_raster <- function(values, space, title, no_data = 0, col, legend=TRUE) {
  # creates a temporary raster with original extent
  temp_ras <- terra::rast(xmin=space$extent[["xmin"]], 
                          xmax=space$extent[["xmax"]],
                          ymin=space$extent[["ymin"]],
                          ymax=space$extent[["ymax"]],
                          resolution = space$type_spec_res)
  values(temp_ras) <- no_data

  img <- cbind(space[["coordinates"]], no_data)
  img[names(values), 3] <- values
  
  temp_df <- merge(as.data.frame(temp_ras, xy=T), img, all.x=T)
  temp_df <- temp_df[,-which(colnames(temp_df)==names(temp_ras)[1])]
  
  #ras <- terra::rast(img, type="xyz", resolution = space$type_spec_res)
  # Release temporary raster object and free memory
  rm(temp_ras) 
  gc()
  
  # creates the final raster
  ras <- terra::rast(temp_df)
  
  # extend raster to space extent in order to avoid flickering when animating
  ras <- terra::extend(ras, terra::ext(space[["extent"]]), fill=NA)
  
  # get the axis break points
  x_breaks <- seq(space$extent[["xmin"]],space$extent[["xmax"]], length.out = 7)
  y_breaks <- seq(space$extent[["ymin"]],space$extent[["ymax"]], length.out = 7)
  
  # test data nature
  if (length(unique(values)) <= 10) {
    # discrete, must be converted to factor and treat low values to deal with NA values
    values_vec <- terra::values(ras) 
    if ((max(values_vec, na.rm = T) - min(values_vec, na.rm = T)) < 1 &&
        (max(values_vec, na.rm = T) != min(values_vec, na.rm = T))) {
      while ((max(values_vec, na.rm = T) - min(values_vec, na.rm = T)) < 1) {
        values_vec <- values_vec * 10
      }
    }
    terra::values(ras) <- values_vec  
    
    # converting to factor
    ras <- as.factor(ras)
    lvl <- levels(ras)[[1]]
    lvl$no_data <- c(unique(values) |> sort() |> as.character())
    levels(ras) <- lvl
    
    col <- unique(col)
    names(col) <- unique(values)

    # the plot
    ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = ras) + # to plot the raster
      ggplot2::scale_fill_manual( # to construct the legend
        values = col,
        name = title,
        na.translate = F
      ) +
      raster_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(title, " ", space$timestep, " t_", space[["id"]])) # gen3sis2 standard aesthetic
  } else {
    # continuous, ggplot2 deals with the NA
    ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = ras) + # to plot the raster
      ggplot2::scale_fill_gradientn( # to construct the legend
        colors = color_richness(20),
        na.value = "transparent",
        name = title) +
      raster_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(title, " ", space$timestep, " t_", space[["id"]])) # gen3sis2 standard aesthetic
  } 
}

#' Plot a single set of values onto a given space
#'
#' @param values a named list of values, the names must correspond to cells in the space
#' @param space a space to plot the values onto
#' @param title a title string for resulting plot, the time information will be taken and appended from the space id
#' @param no_data what value should be used for missing values in values
#' @param col a vector containing a color palette. For discrete values, the first element in the vector will be assigned to zero values. If NULL, gen3sis2 internal palette will be used. Default is NULL
#' @param legend corresponds to the \link{raster} legend plot parameter. This can be omitted and legend is handled by raster::plot
#' 
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual scale_x_continuous scale_y_continuous theme_bw theme element_text element_blank labs scale_fill_gradientn
#' @importFrom h3jsr point_to_cell cell_to_polygon
#' @importFrom sf st_as_sf st_wrap_dateline st_sf
#' 
#' @example inst/examples/plot_single_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_single.gen3sis_space_points <- function(values, space, title="", no_data = 0, col, legend=TRUE) {
  # construct the sf to plot
  spatial_points <- sf::st_as_sf(as.data.frame(space$coordinates), coords = c("x", "y"))
  
  # get axis breaks
  x_breaks <- seq(space$extent[["xmin"]],space$extent[["xmax"]], length.out = 7)
  y_breaks <- seq(space$extent[["ymin"]],space$extent[["ymax"]], length.out = 7)
  
  # test data nature
  if (length(unique(values)) <= 10) {
    # discrete
    # creates the sf
    polygons_sf <- sf::st_sf(
      value = as.factor(values), # converts to factor
      geometry = sf::st_geometry(spatial_points)
    )
    
    col <- unique(col)
    names(col) <- unique(values)
  
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = polygons_sf, ggplot2::aes(color = value)) + # to plot the points
      ggplot2::scale_color_manual( # to construct the legend
        values = col,
        name = title
      ) +
      sf_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(title, " ", space$timestep, " t_", space[["id"]])) # gen3sis2 standard aesthetics
  } else {
    # continuous
    # creates the sf
    polygons_sf <- sf::st_sf(
      value = values,
      geometry = sf::st_geometry(spatial_points)
    )
    
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = polygons_sf, ggplot2::aes(color = value)) + # to plot the points
      ggplot2::scale_color_gradientn( # to construct the legend
        colors = col,
        name = ifelse(grepl("Abundance", title), "Individuals", title)
      ) +
      sf_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(title, " ", space$timestep, " t_", space[["id"]])) # gen3sis2 standard aesthetics
  } 
  
  ####
  # plot(space[["coordinates"]], 
  #      main=paste0(title, " ", space$timestep, " t_", space[["id"]]),
  #      xlim=space[["extent"]][c("xmin","xmax")],
  #      ylim=space[["extent"]][c("ymin","ymax")],
  #      col=col, pch=20)
}

# TODO update documentation
#' Plot a single set of values onto a given space
#'
#' @param values a named list of values, the names must correspond to cells in the space
#' @param space a space to plot the values onto
#' @param title a title string for resulting plot, the time information will be taken and appended from the space id
#' @param no_data what value should be used for missing values in values
#' @param col corresponds to the \link{raster} col plot parameter. This can be omitted and colors are handled by raster::plot  
#' @param legend corresponds to the \link{raster} legend plot parameter. This can be omitted and legend is handled by raster::plot
#' 
#' @importFrom ggplot2 ggplot geom_sf aes scale_color_manual scale_x_continuous scale_y_continuous theme_bw theme element_text element_blank labs scale_color_gradientn
#' @importFrom sf st_as_sf st_sf st_geometry
#' 
#' @example inst/examples/plot_single_help.R
#' @return no return value, called for plot
#' 
#' @export
plot_single.gen3sis_space_h3 <- function(values, space, title="", no_data = 0, col, legend=TRUE) {
  # get the points
  spatial_points <- sf::st_as_sf(as.data.frame(space$coordinates), coords = c("x", "y"), crs = 4326)
  
  # get the cells and polygons
  # TODO pass crs here, review and standardize crs handling and protocol
  cells <- h3jsr::point_to_cell(spatial_points, space$type_spec_res)
  polygons <- h3jsr::cell_to_polygon(cells)
  
  #polygons <- h3jsr::cell_to_polygon(h3::geo_to_h3(space$coordinates, space$type_spec_res))
  # if the extent is global, wrap the dateline
  if (space$extent[["xmin"]] == -180 & space$extent[["xmax"]]==180){
    # wrap dateline
    polygons <- sf::st_wrap_dateline(polygons, options= c('WRAPDATELINE=YES', 'DATELINEOFFSET=180'), quiet=TRUE)
  }
  # Add the values to the polygons for plotting
  # polygons$values <- values  # Add the values as an attribute to polygons

  # plot(polygons_sf,
  #      main=paste0(title, " ", space$timestep, " t_", space[["id"]]),
  #      xlim=c(space$extent[1], space$extent[2]), ylim=c(space$extent[3], space$extent[4]), 
  #      xlab="", ylab="", col=col, border=NA)
  
  # background_grid <- h3jsr::cell_to_polygon(unique(unlist(h3jsr::get_children(h3jsr::get_res0(), space$type_spec_res))))
  
  # get axis breaks
  x_breaks <- seq(space$extent[["xmin"]],space$extent[["xmax"]], length.out = 7)
  y_breaks <- seq(space$extent[["ymin"]],space$extent[["ymax"]], length.out = 7)
  
  # test data nature
  if (length(unique(values)) <= 10) {
    # discrete
    # construct the polygons
    polygons_sf <- sf::st_sf(
      value = as.factor(values), # converts to factor
      geometry = polygons
    )
  
    names(col) <- unique(values)
    col <- col[1:length(unique(values))]
    
    # plot
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = polygons_sf, ggplot2::aes(fill = value)) + # to plot the polygons
      ggplot2::scale_fill_manual( # to construct the legends
        values = col,
        name = title
      ) +
      sf_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(title, " ", space$timestep, " t_", space[["id"]])) # gen3sis2 standard aesthetics
  } else {
    # continuous
    # construct the sf
    polygons_sf <- sf::st_sf(
      value = values,
      geometry = polygons
    )
    
    # plot
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = polygons_sf, ggplot2::aes(fill = value)) + # to plot the polygons
      ggplot2::scale_fill_gradientn( # to construct the legend 
        colors = col,
        name = ifelse(grepl("Abundance", title), "Individuals", title)) +
      sf_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(title, " ", space$timestep, " t_", space[["id"]])) # gen3sis2 standard aesthetics
  } 
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



#' Plot a set of values onto a given space
#'
#' @param values a matrix of values with columns corresponding to sets of values, and rows corresponding to grid cells,
#' this will result in ncol(values) raster plots.
#' @param space a space to plot the values onto
#' @param col a vector containing a color palette. For discrete values, the first element in the vector will be assigned to zero values. If NULL, gen3sis2 internal palette will be used. Default is NULL
#' @param no_data what value should be used for missing data present in the values parameter
#' 
#' @importFrom ggplot2 ggplot scale_fill_gradientn scale_x_continuous scale_y_continuous theme_bw theme element_text element_line labs
#' @importFrom patchwork wrap_plots
#' @importFrom scales alpha
#' @importFrom terra rast ext
#' @importFrom tidyterra geom_spatraster
#' 
#' @return no return value, called for plot
#'
#' @export
plot_multiple.gen3sis_space_raster <- function(values, space, col, no_data = 0) {
  # reconstruct the raster
  img <- matrix(no_data,
                nrow = nrow(space[["coordinates"]]),
                ncol = ncol(values) + 2,
                dimnames = list(rownames(space[["coordinates"]]),
                                c(colnames(space[["coordinates"]]),
                                  colnames(values))))
  img[, 1:2] <- space[["coordinates"]]
  img[rownames(values), -c(1:2)] <- values
  
  ras <- terra::rast(img, type="xyz")
  terra::ext(ras) <- space$extent
  #terra::res(ras) <- space$type_spec_res
  #terra::plot(ras, main=paste0(colnames(values), " ", space$timestep, " ts ", space[["id"]]))
  
  # get axis breaks
  x_breaks <- seq(space$extent[["xmin"]],space$extent[["xmax"]], length.out = 7)
  y_breaks <- seq(space$extent[["ymin"]],space$extent[["ymax"]], length.out = 7)
  
  # plot each variable at a time to maintain the legend and stores in a list
  plots <- lapply(names(ras), function(v) {
    ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = ras[[v]]) + # to plot the raster
      ggplot2::scale_fill_gradientn( # to construct the legend
        colors = color_richness(20),
        na.value = "transparent",
        name = v) +
      sf_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(v, " ", space$timestep, " t_", space[["id"]])) # gen3sis2 standard aesthetics
  })
  
  patchwork::wrap_plots(plots) # wrap everything up and plot it 
}

#' Plot a set of values onto a given space
#'
#' @param values a matrix of values with columns corresponding to sets of values, and rows corresponding to grid cells,
#' this will result in ncol(values) raster plots.
#' @param space a space to plot the values onto
#' @param col a vector containing a color palette. For discrete values, the first element in the vector will be assigned to zero values. If NULL, gen3sis2 internal palette will be used. Default is NULL
#' @param no_data what value should be used for missing data present in the values parameter
#' 
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_gradientn scale_x_continuous scale_y_continuous theme_bw theme element_text element_blank labs
#' @importFrom h3jsr point_to_cell cell_to_polygon
#' @importFrom patchwork wrap_plots
#' @importFrom sf st_as_sf st_sf
#' 
#' @return no return value, called for plot
#'
#' @export
plot_multiple.gen3sis_space_h3 <- function(values, space, col, no_data = NA) {
  # Creates a matrix with coordinates and values  
  env_mtx <- cbind(space$coordinates, values)
  
  # Get axis breaks
  x_breaks <- seq(space$extent[["xmin"]],space$extent[["xmax"]], length.out = 7)
  y_breaks <- seq(space$extent[["ymin"]],space$extent[["ymax"]], length.out = 7)
  
  # Convert to points
  env_points <- sf::st_as_sf(as.data.frame(env_mtx), coords = c("x", "y"), crs = 4326)
  
  # and extract the h3 cell indexes
  env_cells <- h3jsr::point_to_cell(env_points,space$type_spec_res)
  polygons <- h3jsr::cell_to_polygon(env_cells)
  
  # construct the sf
  polygons_sf <- sf::st_sf(
    value = values,
    geometry = polygons
  )
  
  names(polygons_sf) <- gsub("value\\.","",names(polygons_sf))
  
  # organize the polygons and variables in a long format
  long_poly <- lapply(colnames(values), function(variable){
    temp_poly <- polygons_sf
    temp_poly$variable <- variable
    temp_poly$value <- temp_poly[[variable]]
    temp_poly
  }) 
  
  long_poly <- do.call(rbind, long_poly)

  # plot each variable at a time to maintain the legend and stores in a list
  plots <- lapply(unique(long_poly$variable), function(v) {
    ggplot2::ggplot(long_poly[long_poly$variable == v, ]) +
      ggplot2::geom_sf(ggplot2::aes(fill = value), color = NA) + # to plot the polygons
      ggplot2::scale_fill_gradientn( # to construct the legend
        colors = col,
        name = v) +
      sf_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(v, " ", space$timestep, " t_", space[["id"]])) # gen3sis2 standard aesthetics
  })
  
  patchwork::wrap_plots(plots) # wrap everything up and plot it 
}

#' Plot a set of values onto a given space
#'
#' @param values a matrix of values with columns corresponding to sets of values, and rows corresponding to grid cells,
#' this will result in ncol(values) raster plots.
#' @param space a space to plot the values onto
#' @param col a vector containing a color palette. For discrete values, the first element in the vector will be assigned to zero values. If NULL, gen3sis2 internal palette will be used. Default is NULL
#' @param no_data what value should be used for missing data present in the values parameter
#' 
#' @importFrom ggplot2 ggplot geom_sf aes scale_color_gradientn scale_x_continuous scale_y_continuous theme_bw theme element_text element_blank labs
#' @importFrom patchwork wrap_plots
#' @importFrom sf st_as_sf st_sf st_geometry
#' 
#' @return no return value, called for plot
#'
#' @export
plot_multiple.gen3sis_space_points <- function(values, space, no_data = NA) {
  # Gets the points
  spatial_points <- sf::st_as_sf(as.data.frame(space$coordinates), coords = c("x", "y"))
  
  # Get axis breaks
  x_breaks <- seq(space$extent[["xmin"]],space$extent[["xmax"]], length.out = 7)
  y_breaks <- seq(space$extent[["ymin"]],space$extent[["ymax"]], length.out = 7)
  
  # Construct the sf...
  points_sf <- sf::st_sf(
    value = values,
    geometry = sf::st_geometry(spatial_points)
  )
  
  names(points_sf) <- gsub("value\\.","",names(points_sf))
  
  # ... and organize it in a long format 
  long_point <- lapply(colnames(values), function(variable){
    long_point <- points_sf
    long_point$variable <- variable
    long_point$value <- long_point[[variable]]
    long_point
  }) 
  
  long_point <- do.call(rbind, long_point)
  long_point <- long_point[,c("variable","value")]
  
  # Calculate a optimal point size
  x_cat <- space$extent[[2]] - space$extent[[1]]
  y_cat <- space$extent[[4]] - space$extent[[3]]
  auto_cex <- round(sqrt((x_cat**2)+(y_cat**2))/nrow(values))+1
  
  # plot each variable at a time to maintain the legend and stores in a list
  plots <- lapply(unique(long_point$variable), function(v) {
    ggplot2::ggplot(long_point[long_point$variable == v, ]) +
      ggplot2::geom_sf(ggplot2::aes(color = value), size = auto_cex) + # to plot the points
      ggplot2::scale_color_gradientn( # to construct the legend
        colors = color_richness(20),
        name = v) +
      sf_plot_aesthetics(space, col, x_breaks, y_breaks, title = paste0(v, " ", space$timestep, " t_", space[["id"]])) # gen3sis2 standard aesthetics
  })
  
  patchwork::wrap_plots(plots) # wrap everything up and plot it 
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

#' Standard gen3sis2 plot aesthetics for raster spaces
#'
#' @param space a space to plot the values onto
#' @param col corresponds to the \link{raster} col plot parameter. This can be omitted and colors are handled by raster::plot  
#' @param x_breaks numeric. Break points of the x axis. Aesthetic only. 
#' @param y_breaks numeric. Break points of the y axis. Aesthetic only.
#' @param title a string with plot title
#'
#' @returns no return value, called for plot
#' @export
raster_plot_aesthetics <- function(space, col, x_breaks, y_breaks, title) {
  list(
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      breaks = x_breaks
    ),
    ggplot2::scale_y_continuous(
        expand = c(0, 0),
        breaks = y_breaks
      ),
    ggplot2::theme_bw(),
    ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        panel.grid.major = ggplot2::element_line(color = scales::alpha("darkgray", 0.5), linewidth = 0.2),
        panel.grid.minor = ggplot2::element_line(color = scales::alpha("darkgray", 0.4), linewidth = 0.1)
      ),
    ggplot2::labs(
        title = title
      )
  )
}

#' Standard gen3sis2 plot aesthetics for H3 and points spaces
#'
#' @param space a space to plot the values onto
#' @param col corresponds to the \link{raster} col plot parameter. This can be omitted and colors are handled by raster::plot  
#' @param x_breaks numeric. Break points of the x axis. Aesthetic only. 
#' @param y_breaks numeric. Break points of the y axis. Aesthetic only.
#' @param title a string with plot title
#'
#' @returns no return value, called for plot
#' @export
sf_plot_aesthetics <- function(space, col, x_breaks, y_breaks, title) {
  list(
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      breaks = x_breaks,
      labels = function(x) x
    ),
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = y_breaks,
      labels = function(x) x
    ),
    ggplot2::theme_bw(),
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ),
    ggplot2::labs(
      title = title
    )
  )
}


