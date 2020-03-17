# Copyright (c) 2020, ETH Zurich

# DO NOT USE ' IN THIS CONFIG
skeleton_config <- '

########################
### General settings ###
########################
# set the random seed for the simulation
random_seed = NA

# set the starting time step or leave NA to use the earliest/highest timestep
start_time = NA

# set the end time step or leave as NA to use the lates/lowest timestep (0)
end_time = NA

# maximum total number of species in the simulation before it is aborted
max_number_of_species = 50000

# maximum number of species within one cell before the simulation is aborted
max_number_of_coexisting_species = 1e5

# a list of traits to include with each species
# a "dispersal" trait is implictly added in any case
trait_names = c("t_min", "competition", "dispersal")

# ranges to scale the input environemts with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# lsited with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
environmental_ranges = list("temp" = c(-45, 25) )

# a place to inspect the internal state of the simulation and collect additional information if desired
end_of_timestep_observer = function(data, vars, config){
  # a list of all species can be found in data$all_species
  # the current landscape can be found in data$landscape
}


######################
### Initialization ###
######################
# the intial abundace of a newly colonized cell, both during setup and later when colonizing a cell during the dispersal
initial_abundance = 1

# place speices within rectangle:
create_initial_species <- function(landscape, config) {
  range <- c(-180, 180, -90, 90)
  co <- landscape$coordinates
  selection <- co[, "x"] >= range[1] &
               co[, "x"] <= range[2] &
               co[, "y"] >= range[3] &
               co[, "y"] <= range[4]
  initial_cells <- rownames(co)[selection]
  new_species <- create_species(NA, initial_cells, config)
  t_min <- min(landscape$environment[initial_cells, "temp", drop = F])
  new_species$traits[ , "dispersal"] <- 1
  return(list(new_species))
}

create_initial_species <- function(landscape, config) {
  spacing <- 3220
  world <- raster::rasterFromXYZ(cbind(landscape$coordinates, landscape$environment[, 1, drop = F]))
  resolution <- raster::res(world)
  xs <- sort(unique(landscape$coordinates[, "x", drop = F]))
  ys <- sort(unique(landscape$coordinates[, "y", drop = F]))
  step_x <- spacing / (resolution[1] * 111)
  step_y <- spacing / (resolution[2] * 111)

  all_species <- list()

  for( x in seq(1, length(xs), step_x)){
    force(x)
    for( y in seq(1, length(ys), step_y)){
      force(y)
      if( !is.na(world[y, x])) {
        cell <- as.character(raster::cellFromRowCol(world, y, x))
        new_species <- create_species(NA, as.character(cell), config)
        new_species$traits[ , "dispersal"] <- 1
        all_species <- append(all_species, list(new_species))
      }
    }
  }
  return(all_species)
}


#################
### Dispersal ###
#################
max_dispersal <- Inf
# returns n dispersal values
get_dispersal_values <- function(n, species, landscape, config) {
  dispersal_range = c(0, 900)
  weighted_dispersal <- sum(species[["traits"]][, "dispersal"] * species[["abundance"]])
  disp_factor <- weighted_dispersal/sum(species[["abundance"]])
  scale <- ((dispersal_range[2] - dispersal_range[1]) * disp_factor ) + dispersal_range[1]

  values <- rweibull(n, shape = 2.5, scale = scale)
  return(values)
}


##################
### Speciation ###
##################
# threshold for genetic distance after which a speciation event takes place
divergence_threshold = 18.9

# factor by which the divergence is increased between geographicaly isolated population
# can also be a matrix between the different population clusters
get_divergence_factor <- function(species, cluster_indices, landscape, config) {
  return(1)
}


################
### Mutation ###
################
# mutate the traits of a species and return the new traits matrix
apply_evolution <- function(species, cluster_indices, landscape, config) {
  traits <- species[["traits"]]
  # your mutations
  return(traits)
}


###############
### Ecology ###
###############
# called for every cell with all occuring species, this functin calculates the who survives in the current cells
# returns a vector of abundances
# set the abundance to 0 for every species supposed to die
apply_ecology <- function(abundance, traits, environment, config) {
  return(species[, "abd"])
}

' # DO NOT REMOVE THIS ->'<-. IT IS IMPORTANT
