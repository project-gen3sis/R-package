# Copyright (c) 2020, ETH Zurich

#' Create an landscape input
#'
#' @param landscapes list of named list(s) of raster(s) or raster file(s) name(s). Starting from the present towards the past.
#' The list names are important since these are the environmental names
#' @param cost_function function that returns a cost value between a pair of cell (neighbours) that should have the following signature:
#' #' cost_fuinction <- function(src, src_habitable, dest, dest_habitable){
#'  #rules for envoriolmental factors to be considered (e.g. elevation)
#'  #return(cost value)
#' }
#' where src is a vector of environmental conditions for the origin cell, src_habitable (TRUE or FALSE) for habitable condition of the origin cell, dest is a vector of environmental conditions for the destination cell, dest_habitable  (TRUE or FALSE) for habitable condition of the destination cell
#' @param directions 4, 8, 16 oder adjacency matrix (see gdistance package)
#' @param output_directory path for storing the gen3sis ready landscape 
#' @param timesteps vector of names for every tiumestep to represent the timestep at gen3sis ready landscape. if NULL, timsteps are sequentialy numbered from 0 (present)
#' @param habitability_masks either (1) a list of files or rasters starting from the present indicating the habitability of cell at that timestep.
#' #' or (2) NULL, every cell with at least one NA as environmentalvalue(s) will be considered inhabitable 
#' @param calculate_full_distance_matrices TRUE or FALSE. If TRUE calculates the entire distance matrix for every timestep and between all habitable cells (faster CPU time, higher storage required). If FALSE, only local distances are calculated (slower CPU time when simulating but smaller gen3sis landscape size)
#' @param crs the coordinate reference system in crs format (see rater::crs)
#' @param overwrite_output TRUE or FALSE
#' @param verbose print distance calculation progress
#'
#' @importFrom gdistance transition costDistance
#' @example inst/examples/create_input_help.R
#' @export

create_input <- function( landscapes,
                          cost_function,
                          directions,
                          output_directory,
                          timesteps = NULL,
                          habitability_masks = NULL,
                          calculate_full_distance_matrices = F,
                          crs = NULL,
                          overwrite_output = F,
                          verbose = F) {
  # # in case outpu_directory is NULL, use the same directory as the input
  # if (is.null(output_directory)){
  #   output_directory <- dirname(path)
  # }
  
  # prepare directories
  create_directories(output_directory, overwrite_output, calculate_full_distance_matrices)

  if(is.null(timesteps)){
    timesteps <- 0:(length(landscapes[[1]]) - 1)
  }

  # prepare and save landscapes (formerly all_go_hab)
  compiled_landscapes <- compile_landscapes(landscapes,
                                            timesteps,
                                            habitability_masks)
  saveRDS(compiled_landscapes, file.path(output_directory, "landscapes.rds"))

  # create local distances
  # iterate over timesteps
  for( step in 1:length(timesteps) ) {
    if (verbose){
      cat(paste("starting distance calculations for timestep", step, '\n'))
    }
    landscape_stack <- stack_landscapes(landscapes, step)
    habitable_mask <- get_habitable_mask(habitability_masks, landscape_stack, step)

    distance_local <- get_local_distances(landscape_stack, habitable_mask, cost_function, directions, crs)

    file_name <- paste0("distances_local_", as.character(step-1), ".rds")
    saveRDS(distance_local, file = file.path(output_directory, "distances_local", file_name))

    if(calculate_full_distance_matrices){
      # transpose to preserve src/dest relation for efficent local traversal in get_distance_matrices function
      distance_local <- t(distance_local)

      habitable_cells <- which(habitable_mask[])

      dist_matrix <- get_distance_matrix(habitable_cells,
                                         length(habitable_mask[]),
                                         distance_local@p,
                                         distance_local@i,
                                         distance_local@x,
                                         Inf)
      file_name <- paste0("distances_full_", as.character(step-1), ".rds")
      saveRDS(dist_matrix, file = file.path(output_directory, "distances_full", file_name))
      rm(dist_matrix)
      gc()
    }
  }
}


#' compile the landscapes for storing
#'
#' @param landscapes full list of landscasep over all timesteps
#' @param timesteps given names / identifiers for timesteps
#' @param habitabiliy_masks full list of habitability masks (if available)
#'
#' @return a list of compiled landscapes ready to be saved
#' @noRd
compile_landscapes <-  function(landscapes, timesteps, habitability_masks) {
  compiled <- list()
  landscape_stack <- stack_landscapes(landscapes, 1)

  # setup coordinates
  coords <- as.data.frame(landscape_stack, xy = T)[,c("x", "y")]
  rownames(coords) <- 1:nrow(coords)
  for (name in names(landscapes)) {
    compiled <- append(compiled, list(coords))
  }
  names(compiled) <- names(landscapes)

  # collect environments for every timestep
  for (i in 1:length(timesteps)) {
    # collect landscapes
    landscape_stack <- stack_landscapes(landscapes, i)
    landscape_df <- as.data.frame(landscape_stack)

    # remove inconsistencies between different landscapes
    habitability <- get_habitable_mask(habitability_masks, landscape_stack, i)[]
    landscape_df[!habitability, ] <- NA

    for (name in colnames(landscape_df)) {
      old_names <- colnames(compiled[[name]])
      compiled[[name]] <- cbind(compiled[[name]], landscape_df[, name])
      colnames(compiled[[name]]) <- c(old_names, timesteps[i])
    }
  }
  landscapes <- compiled
  return(landscapes)
}

#' get_local_distances creates the cost adjecency matrix with
#' the local cost calculated by the user provided cost_function
#'
#' @param landscape_stack a raster stack with the environments for the current step
#' @param cost_function the user provided cost function
#' @param directions the number and layout of the direct local to consider, 4,8,16 or adj according to the gdistance package
#'
#' @return an adjecency matrix with the local distances
#' @importFrom gdistance geoCorrection transition
#' @importFrom methods as
#' @noRd
get_local_distances <- function(landscape_stack, habitable_mask, cost_function, directions, crs) {
  ext <- extent(landscape_stack)
  rs <- res(landscape_stack)

  if(!is.null(crs)) {
    crs(landscape_stack) <- crs
  }

  # the reference raster is used to get all local relations, filling it with the cell ids can be used for debugging
  # -> important in case of asymmetrical distances
  ref_raster <- raster(ext = ext, resolution = rs, crs = crs(landscape_stack), vals = 1:prod(dim(landscape_stack)[1:2]))
  transition_index <- gdistance::transition(ref_raster,
                                            transitionFunction = function(x){x[1]},
                                            directions = directions,
                                            symm = F)

  # calculate the correction for distortions and keep separate to apply after the users cost function
  correction <- gdistance::geoCorrection(transition_index, type = "c", multpl = T)
  correction@transitionMatrix@x <- (1/1000) / correction@transitionMatrix@x

  transition_matrix <- as(transition_index@transitionMatrix, "dgCMatrix")
  transition_cells <- summary(transition_matrix)

  tmp_cost <- numeric(nrow(transition_cells))

  habitable_mask <- as.logical(as.vector(habitable_mask))
  landscape_stack <- as.matrix(landscape_stack)

  for( k in 1:nrow(transition_cells)){
    ind_i <- transition_cells[k, "i"]
    ind_j <- transition_cells[k, "j"]

    cell_i <- landscape_stack[ind_i,]
    cell_j <- landscape_stack[ind_j,]

    habitable_i <- habitable_mask[ind_i]
    habitable_j <- habitable_mask[ind_j]

    # IMPORTANT
    # We want efficient itrerations over sparse matrices
    # sparse matrices are column compressed, We therefore flip the indices and local distances are
    # indexed local_distance[dest, src].
    cost <- cost_function(cell_j, as.logical(habitable_j), cell_i, as.logical(habitable_i))
    if(cost == Inf){
      cost <- 0
    }
    tmp_cost[k] <- cost
  }

  transition_matrix <- sparseMatrix(i = transition_cells[, "i"],
                                    j = transition_cells[, "j"],
                                    x = tmp_cost)
  transition_matrix <- drop0(transition_matrix) * correction@transitionMatrix

  rownames(transition_matrix) <- 1:dim(landscape_stack)[1]
  colnames(transition_matrix) <- 1:dim(landscape_stack)[1]
  return(transition_matrix)
}

#' get_habitable_mask either extracts or computes the habitability mask.
#'
#' @details if no mask is given it will be calculated from the landscapes. Every cell that has at
#' least on environmental value missing (set to NA) will be considere uninhabitable
#'
#' @param habitability_masks null, or a list of rasters or paths to raster files
#' @param landscape_stack the current stack of landscapes
#' @param i index into the masks if needed
#'
#' @return returns a logical raster indicating which cells are habitable
#' @noRd
get_habitable_mask <- function(habitability_masks, landscape_stack, i) {
  if(is.null(habitability_masks)) {
    habitable_mask <- calc(landscape_stack, fun = function(x, na.rm) { !any(is.na(x)) } )
  } else if( is.character(habitability_masks[[i]])){
    habitable_mask <- raster(habitability_masks[[i]])
  } else if( "RasterLayer" %in% class(habitability_masks[[i]])) {
    habitable_mask <- habitability_masks[[i]]
  } else {
    stop("unknown habitability_mask; it has to be null, a list of rasters, or paths to raster files")
  }
  habitable_mask <- as.logical(habitable_mask)
  return(habitable_mask)
}


#' stack_landscapes creates the i-th stack of landscape rasters from the provided sources
#'
#' @param landscapes the full landscapes, a named list of lists of raster files or rasters
#' @param i the index into the landscapes parameter
#'
#' @return a raster stack containing the i-th landscapes
#' @noRd
stack_landscapes <- function(landscapes, i) {
  all_names <- names(landscapes)
  new_stack <- stack()
  for( landscape in landscapes){
    if(is.character(landscape[i])) {
      ras <- raster(landscape[i])
    } else if( "RasterLayer" %in% class(landscape[[i]])) {
      ras <- landscape[i]
    } else {
      stop("unknown landscapes; it has to be a named list of list of either rasters or raster files")
    }
    new_stack <- addLayer(new_stack, ras)
  }
  new_brick <- as(new_stack, "RasterBrick")
  names(new_brick) <- all_names
  return(new_brick)
}

#' creates the directories to store the results in
#'
#' @param directory the target directory
#' @param overwrite logical, overwrite existing files?
#' @param full_matrices logical, calculate the full distnace matrices
#'
#' @noRd
create_directories <- function(directory, overwrite, full_matrices) {
  # check directory
  if(file.exists(directory) && !overwrite) {
    stop("output directory already exists")
  } else if(file.exists(directory) && overwrite) {
    unlink(directory, recursive = T)
  }
  # create directories
  dir.create(file.path(directory), recursive=TRUE, showWarnings = FALSE)
  dir.create(file.path(directory, "distances_local"), recursive=TRUE, showWarnings = FALSE)
  if(full_matrices) {
    dir.create(file.path(directory, "distances_full"), recursive=TRUE, showWarnings = FALSE)
  }
}
