

########################
### General settings ###
########################
# set seed for the simulation
random_seed = 666

# set the starting time step or leave NA to use the earliest/highest timestep
start_time = 100

# set the end time step or leave as NA to use the lates/lowest timestep (0)
end_time = NA

# maximum total number of species in the simulation before it is aborted
max_number_of_species = 200000

# maximum number of species within one cell before the simulation is aborted
max_number_of_coexisting_species = 1e5

# a list of traits to include with each species
# a "dispersal" trait is implictly added in any case
trait_names = c("temp",  "prec", "dispersal")


# ranges to scale the input environemts with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# lsited with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
#environmental_ranges = list("temp" = c(-45, 25), "prec" = NA )

# a place to inspect the internal state of the simulation and collect additional information if desired
end_of_timestep_observer = function(data, vars, config){
  # a list of all species can be found in data$all_species
  # the current landscape can be found in data$landscape
  save_landscape()
  save_species()
}


######################
### Initialization ###
######################
# the intial abundace of a newly colonized cell, both during setup and later when colonizing a cell during the dispersal
initial_abundance = 10

# place speices within rectangle:

create_ancestor_species <- function(landscape, config) {
  
  all_species <- list()    
   suitablecells<- c(449, 450, 480, 481)
  for(cellID in 1: length(suitablecells)){ 
    cell <- as.character(suitablecells[cellID])
    new_species <- create_species(NA, as.character(cell), config)  # use this function to create species, one can provide directly the initial cells
    new_species$traits[ , "dispersal"] <- 1
    new_species$traits[ , "temp"] <-  rnorm(1,20,0.5) 
    new_species$traits[ , "prec"] <- rnorm(1,500,50) 
    all_species <- append(all_species, list(new_species))
  }
  
  return(all_species)
}

# environmental_ranges = list("temp" = c(17, 23), "prec" = c(250,750) )

#################
### Dispersal ###
#################
# returns n dispersal values
get_dispersal_values <- function(n, species, landscape, config) {
  dispersal_range = c(0, 1)
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
divergence_threshold = 18#runif(1,min=5, max=5.5) ### Arbitrary value to ensure enough diversification wihtout stopping the simulation midway

# factor by which the divergence is increased between geographicaly isolated population
# can also be a matrix between the different population clusters
get_divergence_factor <- function(species, cluster_indices, landscape, config) {
  return(1)
}


################
### Mutation ###
################
#Scenarios (these 3 scenarios were repeated for each experiment):
#I) mutation rates = temporal environmental variance (control):
#SD for temperature: 0.5
#SD for precipitation: 50
#II) mutation rates < temporal environmental variance (higher phylo constraint): 
#SD for temperature: 0.3
#SD for precipitation: 30
#III) mutation rates > temporal environmental variance (less phylo constraint):
#SD for temperature: 0.7
#SD for precipitation: 70
# mutate the traits of a species and return the new traits matrix
apply_evolution <- function(species, cluster_indices, landscape, config) {
  traits <- species[["traits"]]
####  HERE CHANGES BETWEEN CONFIGS #####
  traits[, "temp"] <- traits[, "temp"] + rnorm(length(traits[, "temp"]), mean = 0, sd = 0.5) # sd value: control = 0.5; less phylo constraint=0.7; higher phylo constraint=0.3 
  traits[, "prec"] <- traits[, "prec"] + rnorm(length(traits[, "prec"]), mean = 0, sd = 50)  # sd value: control = 50; less phylo constraint=70; higher phylo constraint=30
  # your mutations
  return(traits)
}


###############
### Ecology ###
###############
# called for every cell with all occuring species, this functin calculates the who survives in the current cells
# returns a vector of abundances
# set the abundance to 0 for every species supposed to die
apply_ecology <- function(abundance, traits, landscape, config) {
  abundance <- ( abs( traits[, "temp"] - landscape[, "temp"]) < 1.5 ) & #1.5
    ( abs( traits[, "prec"] - landscape[, "prec"]) < 150 ) # 150
  return(abundance)
}

#In the sample file there are things wrong

