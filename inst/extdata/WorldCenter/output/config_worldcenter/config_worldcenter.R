########################
### General settings ###
########################
# set the random seed for the simulation
random_seed = 666

# set the starting time step or leave NA to use the earliest/highest timestep
start_time = NA

# set the end time step or leave as NA to use the lates/lowest timestep (0)
end_time = NA

# maximum total number of species in the simulation before it is aborted
max_number_of_species = 50000

# maximum number of species within one cell before the simulation is aborted
max_number_of_coexisting_species = 10000

# a list of traits to include with each species
# a "dispersion" trait is implictly added in any case
#trait_names = c("t_min", "a_min", "competition", "dispersion")
trait_names = c("temp",  "dispersal") # "prec",

# ranges to scale the input environemts with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# lsited with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
environmental_ranges = list("temp" = c(-45, 55), "area"=c(151067.6, 196948.4), "prec"=c(1,0.5))#c(6895.094, 196948.4))#, "prec" = NA )

# a place to inspect the internal state of the simulation and collect additional information if desired
end_of_timestep_observer = function(data, vars, config){
  par(mfrow=c(2,2))
  plot_raster_single(data$landscape$environment[,"temp"], data$landscape, "temp", NA)
  plot_raster_single(data$landscape$environment[,"prec"], data$landscape, "prec", NA)
  # plot_raster_single(data$landscape$environment[,"prec"], data$landscape, "prec", NA)
  plot_richness(data$all_species, data$landscape)
  plot_species_presence(data$all_species[[1]], data$landscape)
  #plot_landscape(data$landscape)
  # a list of all species can be found in data$all_species
  # the current landscape can be found in data$landscape
}



######################
### Initialization ###
######################
# the intial abundace of a newly colonized cell, both during setup and later when colonizing a cell during the dispersal
initial_abundance = 1

# place species within rectangle, our case entire globe
create_ancestor_species <- function(landscape, config) {
  range <- c(-180, 180, -90, 90)
  co <- landscape$coordinates
  selection <- co[, "x"] >= range[1] &
    co[, "x"] <= range[2] &
    co[, "y"] >= range[3] &
    co[, "y"] <= range[4]
  initial_cells <- rownames(co)[selection]
  new_species <- create_species(NA, initial_cells, config)
  #set local adaptation to max optimal temp equals local temp
  new_species$traits[ , "temp"] <- landscape$environment[,"temp"]
  new_species$traits[ , "dispersal"] <- 1
  
  return(list(new_species))
}



#################
### Dispersal ###
#################
# returns n dispersal values
get_dispersal_values <- function(n, species, landscape, config) {
  # scale <- 750 #FABIAN, this is the lowest limmit! ### VARY
  values <- 900 #rweibull(n, shape = 2, scale = scale) ### VARY
  return(values)
}


##################
### Speciation ###
##################
# threshold for genetic distance after which a speciation event takes place
divergence_threshold = 2 #72 ### VARY

# factor by which the divergence is increased between geographicaly isolated population
# can also be a matrix between the different population clusters
get_divergence_factor <- function(species, cluster_indices, landscape, config) {
  return(1)
}


################
### Mutation ###
################
# mutate the traits of a species and return the new traits matrix
# source("./modules/trait_homogenization_random_mutation.R", local = T)
# apply_evolution <- trait_homogenization_random_mutation(trait_evolutionary_power = 0.001)
# 


apply_evolution <- function(species, cluster_indices, landscape, config) {
  
  trait_evolutionary_power <- 0.001 ### VARY
  
  traits <- species[["traits"]]
  cells <- rownames(traits)
  
  # browser()
  
  
  ### WIP visualize cluster#####
  #    clusters <- cbind(landscape$coordinates, cluster_indices )
  #    print(paste("# clusters:", length(unique(clusters[,3]))))
  #    plot(rasterFromXYZ(clusters), col=rainbow(length(unique(clusters[,3]))))
  #    # #plot(rasterFromXYZ(clusters[clusters[,"cluster_indices"]%in%as.character(c(1:10)),]))
  #    points(clusters[clusters[,"cluster_indices"]==11,c(1,2), drop=F])
  #    # #points(landscape$coordinates[rownames(landscape$coordinates)%in%cells_cluster,])
  ### end WIP ###
  
  #homogenize trait based on abundance
  for(cluster_index in unique(cluster_indices)){
    # cluster_index <- 1
    cells_cluster <- cells[which(cluster_indices == cluster_index)]
    # hist(traits[cells_cluster, "temp"], main="before")
    mean_abd <- mean(species$abundance[cells_cluster])
    weight_abd <- species$abundance[cells_cluster]/mean_abd
    traits[cells_cluster, "temp"] <- mean(traits[cells_cluster, "temp"]*weight_abd)
    # hist(traits[cells_cluster, "temp"], main="after")
  }
  
  #mutations
  mutation_deltas <-rnorm(length(traits[, "temp"]), mean=0, sd=trait_evolutionary_power)
  traits[, "temp"] <- traits[, "temp"] + mutation_deltas
  
  return(traits)
}






# apply_evolution <- function(species, cluster_indices, landscape, config) {
#   print(cluster_indices)
#   traits <- species[["traits"]]
#   traits[, "temp"] <- traits[, "temp"] + rnorm(length(traits[, "temp"]), mean = 0, sd =0.001) #evt. erhöhen
#   #traits[, "prec"] <- traits[, "prec"] + rnorm(length(traits[, "prec"]), mean = 0, sd =0.001) #evt. erhöhen
#   return(traits)
# }


###############
### Ecology ###
###############
# called for every cell with all occuring species, this functin calculates the who survives in the current cells
# returns a vector of abundances
# set the abundance to 0 for every species supposed to die

apply_ecology <- function(abundance, traits, landscape, config, abundance_scale = 10, abundance_threshold = 1) {
  #abundance treashold
  abundance <- as.numeric(!abundance<abundance_threshold)
  abundance <- (( 1-abs( traits[, "temp"] - landscape[, "temp"]))*abundance_scale)*abundance
  #abundance thhreashold
  abundance[abundance<abundance_threshold] <- 0
  #   ( abs( traits[, "prec"] - landscape[, "prec"]) < 0.3 ) 
  #carring capacity
  #browser()
  are <- (landscape[,"area"]*(landscape[,"prec"]+0.1)*(landscape[,"temp"]+0.1))*abundance_scale^3 ### VARY 2 and 3
  total_ab <- sum(abundance)
  subtract <- total_ab-are
  # print(abundance)
  if (subtract>0){
    # print(paste("should:", are, "is:", total_ab))
    # if (are==0){browser()} #this should never happen, so leave this here as a warning of wrong carring camapcity!
    # if (total_ab-are>10){
    #   browser()
    # }
    # print("carring capacity reached!")
    # print(abundance)
    while (total_ab>are){
      # print("loop")
      alive <- abundance>0
      loose <- sample(1:length(abundance[alive]),1)
      abundance[alive][loose] <- abundance[alive][loose]-1
      total_ab <- sum(abundance)
      # print("abundance")
    }
    #set negative abundances to zero
    abundance[!alive] <- 0
    # print(abundance)
  }
  return(abundance)
}
