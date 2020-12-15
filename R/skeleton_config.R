# Copyright (c) 2020, ETH Zurich

# DO NOT USE ' IN THIS CONFIG

#' empty skeleton config
#' 
#' @return compiled string
#' @noRd
skeleton_config <- function(){
paste0(c('
######################################
###            METADATA            ###
######################################
# gen3sis configuration
#
# Version: 1.0
#
# Author:
#', paste0("# Date: ", format(Sys.Date(), format="%d.%m.%Y")),                          
'#
# Landscape:
#
# Publications:
#
# Description: 
#
######################################


######################################
###         General settings       ###
######################################

# set the random seed for the simulation.
random_seed = NA

# set the starting time step or leave NA to use the earliest/highest time-step.
start_time = NA

# set the end time step or leave as NA to use the latest/lowest time-step (0).
end_time = NA

# maximum total number of species in the simulation before it is aborted.
max_number_of_species = 25000

# maximum number of species within one cell before the simulation is aborted.
max_number_of_coexisting_species = 2500

# a list of traits to include with each species
# a "dispersal" trait is implicitly added in any case
trait_names = c("dispersal")

# ranges to scale the input environments with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# listed with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
environmental_ranges = list( )


######################################
###            Observer            ###
######################################

# a place to inspect the internal state of the simulation and collect additional information if desired.
end_of_timestep_observer = function(data, vars, config){
  # the list of all species can be found in data$all_species
  # the current landscape can be found in data$landscape
  
  # saving functions example:
    # save_landscape()
    # save_species()
  
  # plotting functions example:
    # plot environmental conditions
    # plot_landscape(data$landscape)
    # plot richness
    # plot_richness(data$all_species, data$landscape)
    # plot a specific environmental condition
    # plot_raster_single(data$landscape$environment[,"temp"], data$landscape, "temp", NA)
    # plot species 1 range
    # plot_species_presence(data$all_species[[1]], data$landscape)
    # plot(0,type="n",axes=FALSE,ann=FALSE)

}


######################################
###         Initialization         ###
######################################

# the initial abundance of a newly colonized cell, both during setup and later when 
# colonizing a cell during the dispersal.
initial_abundance = 1

# place species in the landscape:
create_ancestor_species <- function(landscape, config) {
 stop("create the initial species here")
}


######################################
###             Dispersal          ###
######################################

# the maximum range to consider when calculating the distances from local distance inputs.
max_dispersal <- Inf

# returns n dispersal values.
get_dispersal_values <- function(n, species, landscape, config) {
  stop("calculate dispersal values here")
}


######################################
###          Speciation            ###
######################################

# threshold for genetic distance after which a speciation event takes place.
divergence_threshold = NULL

# factor by which the divergence is increased between geographically isolated population.
# can also be a matrix between the different population clusters.
get_divergence_factor <- function(species, cluster_indices, landscape, config) {
  stop("calculate divergence factor here")
}


######################################
###            Evolution           ###
######################################

# mutate the traits of a species and return the new traits matrix.
apply_evolution <- function(species, cluster_indices, landscape, config) {
  stop("mutate species traits here")
}


######################################
###             Ecology            ###
######################################

# called for every cell with all occurring species, this function calculates abundances and/or 
# who survives for each sites.
# returns a vector of abundances.
# set the abundance to 0 for every species supposed to die.
apply_ecology <- function(abundance, traits, environment, config) {
  stop("calculate species abundances and deaths here")
}

')) # DO NOT REMOVE THIS ->'<-. IT IS IMPORTANT
}
