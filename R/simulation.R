# Copyright (c) 2020, ETH Zurich

#---------------------------------------#
######## START OF INITIALIZATION ########
#---------------------------------------#

#' Initializes the landscape inputs
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
setup_inputs <- function(config, data, vars) {
  data[["inputs"]] <- list()
  landscapes <- readRDS(file.path(config$directories$input, "landscapes.rds"))
  landscape_names <- names(landscapes)

  # environmental matrices
  environments <- list()
  for (name in landscape_names){
    tmp <- as.matrix(landscapes[[name]][, -c(1:2)])
    # scaling
    if (name %in% names(config[["gen3sis"]][["general"]][["environmental_ranges"]]) ) {
      range <- config[["gen3sis"]][["general"]][["environmental_ranges"]][[name]]
      if( any(is.na(range)) ) {
        r_min <- min(tmp, na.rm=TRUE)
        r_max <- max(tmp, na.rm=TRUE)
        range <- c(r_min, r_max)
      } else {
        range <- range
      }
      tmp <- ( tmp - range[1] ) / ( range[2] - range[1] )
    } else {
      # not in list, do not scale
    }
    environments[[name]] <- as.matrix(tmp)
  }
  data[["inputs"]][["environments"]] <- environments

  # time-steps
  data[["inputs"]][["timesteps"]] <- colnames(environments[[1]])

  # coordinates
  coords <- as.matrix(landscapes[[1]][, 1:2])
  data[["inputs"]][["coordinates"]] <- coords

  # extent / resolution
  env_raster <- rasterFromXYZ(landscapes[[1]][, 1:3])
  data[["inputs"]][["extent"]] <- extent(env_raster)
  data[["inputs"]][["resolution"]] <- res(env_raster)


  return(list(config = config, data = data, vars = vars))
}

#' Initializes the time-step-variables and flags
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
setup_variables <- function(config, data, vars) {
  # time-steps
  # -1 as time-steps are 0-based
  if (is.na(config$gen3sis$general$start_time)) {
    config$gen3sis$general$start_time <- length(data[["inputs"]][["timesteps"]]) - 1
  } else if (is.character(config$gen3sis$general$start_time)) {
    config$gen3sis$general$start_time<- which(data[["inputs"]][["timesteps"]] == config$gen3sis$general$start_time) - 1
  } else {
    # user supplied numerical time-step
  }

  if(is.na(config$gen3sis$general$end_time)) {
    config$gen3sis$general$end_time <- 0
  } else if (is.character(config$gen3sis$general$end_time)) {
    config$gen3sis$general$end_time <- which(data[["inputs"]][["timesteps"]] == config$gen3sis$general$end_time) - 1
  } else {
    # user supplied numerical time-step
  }

  # put in start time for create_landscape
  vars$ti <- config$gen3sis$general$start_time

  # flag
  vars$flag <- "OK"

  return(list(config = config, data = data, vars = vars))
}


#' Calls the creation for the initial species and prepares further data storage
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#'
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics par
#' @import raster
#' @noRd
init_attribute_ancestor_distribution <- function(config, data, vars) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  all_species <- config$gen3sis$initialization$create_ancestor_species(data$landscape, config)
  for (i in 1:length(all_species)){
    force(i)
    all_species[[i]][["id"]] <- as.character(i)
  }

  data$all_species <- all_species

  #plot starting_richness
  grDevices::pdf(file=file.path(config$directories$output, "starting_richness.pdf"), width=10, height=6)
  par(mfrow=c(1,1))
  plot_richness(all_species, data$landscape)
  grDevices::dev.off()
  
  #plot starting_ranges
  grDevices::pdf(file=file.path(config$directories$output, "starting_ranges.pdf"), width=10, height=6)
  par(mfrow=c(1,1))
  plot_ranges(all_species, data$landscape)
  grDevices::dev.off()
  
 # par(mfrow=c(1,2))
 # plot_richness(all_species, data$landscape)
 # plot_ranges(all_species, data$landscape)
  
  # n_sp <- ncol(geo_sp_ti)
  n_sp <- length(data$all_species)
  vars$n_sp <- n_sp
  vars$n_sp_alive <- n_sp

  data$phy <- data.frame(
    "Ancestor" = rep(1, n_sp), # c(1:n_sp),
    "Descendent" = c(1:n_sp),
    "Speciation.Time" = config$gen3sis$general$start_time,
    "Extinction.Time" = rep(config$gen3sis$general$start_time, n_sp),
    "Speciation.Type" = c("ROOT", rep("GENETIC", n_sp -1)) # "ROOT"
  )
  return(list(config = config, data = data, vars = vars))
  
}


#' Prepares summary statistics to be accumulated in the simulation (e.g. geo_richness object)
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
init_simulation <- function(config, data, vars) {
  # internal variables
  steps <- (config$gen3sis$general$start_time:config$gen3sis$general$end_time)

  # create matrix for turnover
  data$turnover <- matrix(NA, ncol=3, nrow=length(steps), dimnames=list(steps,c("n_new_sp_ti", "n_ext_sp_ti", "n_sp_alive")) )

  #..... add the first turnover at t0 (t_start)....
  data$turnover[1,] <- c(vars$n_sp,0,vars$n_sp)

  # create matrix for geographic total species richness geo_richness
  geo_richness <- matrix(NA, nrow=nrow(data[["inputs"]][["coordinates"]]), ncol=length(steps) + 2)
  geo_richness[,1:2] <- data[["inputs"]][["coordinates"]]
  colnames(geo_richness) <- c(colnames(data[["inputs"]][["coordinates"]]), steps)
  rownames(geo_richness) <- rownames(data[["inputs"]][["coordinates"]])
  #..... add the first species distribution at t0 (t_start)....
  geo_richness[rownames(data[["landscape"]][["coordinates"]]), as.character(config$gen3sis$general$start_time)] <- get_geo_richness(data$all_species, data[["landscape"]])

  data$geo_richness <- geo_richness

  return(list(config = config, data = data, vars = vars))
}

#---------------------------------------#
######## END OF INITIALIZATION ########
#---------------------------------------#


#----------------------------------#
######## START OF LOOP BODY ########
#----------------------------------#


#-------------------------#
######## -> LOOP SETUP #######
#-------------------------#

#' Prepares the landscape for the current time step
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
setup_landscape <- function(config, data, vars) {
  index <- vars$ti + 1
  habitable_cells <- data$inputs$environments[[1]][, index, drop=FALSE]
  habitable_cells <- habitable_cells[which(!is.na(habitable_cells)), , drop=FALSE]
  habitable_cells <- rownames(habitable_cells)
  envir <- do.call(cbind, lapply(data$inputs$environments, "[", habitable_cells, index, drop=FALSE))
  colnames(envir) <- names(data[["inputs"]][["environments"]])
  landscape <- create_landscape(id = vars$ti,
                                environment = envir,
                                coordinates = data[["inputs"]][["coordinates"]][habitable_cells, ],
                                timestep = data[["inputs"]][["timesteps"]][index],
                                extent = data[["inputs"]][["extent"]],
                                resolution = data[["inputs"]][["resolution"]])

  data[["landscape"]] <- landscape

  return(list(config = config, data = data, vars = vars))
}


#' Iterates over all species and calls
#' limit_species_to_cells to restrict them to the currently habitable cells
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
restrict_species <- function(config, data, vars) {
  data$all_species <- lapply(data$all_species, limit_species_to_cells, rownames(data[["landscape"]][["coordinates"]]))
  return(list(config = config, data = data, vars = vars))
}


#' Loads and prepares the distance matrix for the current time step
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
loop_setup_geo_dist_m_ti <- function(config, data, vars) {
  # loading geo_dist_m_ti that is the internal distance matrices
  #load(paste0(config$directories$input,"/geo_dist_m/geo_dist_m_ti/geo_dist_m_ti_t_", vars$ti, ".RData"))
  geo_dist_m_ti <- readRDS(file = file.path(config$directories$input,
                                            "distance_matrices",
                                            paste0("geo_dist_m_ti_t_", vars$ti, ".rds")))

  cell_names <- rownames(data$landscape[["coordinates"]])
  rownames(geo_dist_m_ti) <- cell_names
  colnames(geo_dist_m_ti) <- cell_names

  data$geo_dist_m_ti <- geo_dist_m_ti
  return(list(config = config, data = data, vars = vars))
}


#' Load or calculates the distance matrix used for clustering and dispersal
#'
#' @details If a full matrix is found in input/distance_matrices it will be used. Otherwise the local distances
#' are loaded from the distance_neighbours directory are loaded and a distance_matrix is internally calculated
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
setup_distance_matrix <- function(config, data, vars) {
  matrix_file <- file.path(config$directories$input,
                          "distances_full",
                          paste0("distances_full_", vars$ti, ".rds"))
  if(base::file.exists(matrix_file)) {
    distance_matrix <- readRDS(file = matrix_file)
  } else {
    neighbour_file <- file.path(config$directories$input,
                            "distances_local",
                            paste0("distances_local_", vars$ti, ".rds"))
    distance_neighbours <- readRDS(neighbour_file)

    habitable_cells <- as.integer(rownames(data$landscape$coordinates))
    num_cells <- nrow(distance_neighbours)
    distance_matrix <- get_distance_matrix(habitable_cells,
                                           num_cells,
                                           distance_neighbours@p,
                                           distance_neighbours@i,
                                           distance_neighbours@x,
                                           config$gen3sis$dispersal$max_dispersal)
  }

  data$distance_matrix <- distance_matrix
  return(list(config = config, data = data, vars = vars))
}

#-------------------------------------------#
######## -> UPDATE Extinction Times  #######
#-------------------------------------------#
#' Updates the extinction times 
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
update_extinction_times <- function(config, data, vars) {
  for (sp in data$all_species) {
    if(length(sp[["abundance"]])) {
      data$phy$"Extinction.Time"[as.integer(sp[["id"]])] <- vars$ti - 1
    }
  }
  return(list(config = config, data = data, vars = vars))
  # update turnover
  #data$turnover[toString(vars$ti),] <- c(vars$n_new_sp_ti, sum(data$phy$"Extinction.Time"==(vars$ti+1)), vars$n_sp_alive)

  # update geo_richness
  #data$geo_richness[rownames(data[["landscape"]][["coordinates"]]), as.character(vars$ti)] <- get_geo_richness(data$all_species, data[["landscape"]])
  # data$geo_richness <- update.geo.richness(geo_sp_ti=data$geo_sp_ti, ti=vars$ti, geo_richness = data$geo_richness )
  #data[["eco_by_sp"]] <- get_eco_by_sp(data$all_species)
}

#--------------------------------#
######## END OF LOOP BODY ########
#--------------------------------#


#' Updates the phylogeny with the survival and extinctions from the current time step
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
update.phylo<- function(config, data, vars) {
  # update  phylo with the survive info
  # add MRCA to the levels of Speciation.Type
  levels(data$phy$Speciation.Type) <- c(levels(data$phy$Speciation.Type))
  # bind extinction information
  #data$phy <- cbind(data$phy[,c(1,2,3)], Extinction.Time=vars$ext, Speciation.Type=data$phy[,4])
  data$phy$Ancestor <- as.integer(data$phy$Ancestor)
  data$phy$Descendent <- as.integer(data$phy$Descendent)
  data$phy$Speciation.Time <- as.integer(data$phy$Speciation.Time)
  # end update phy
  return(list(config = config, data = data, vars = vars))
}
