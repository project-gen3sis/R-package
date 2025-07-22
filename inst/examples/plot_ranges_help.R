\dontshow{
  #TODO change this to a working example
}

\dontrun{
  ## plot from saved outputs
  # get path containing outputs
  datapath <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")
  # get species at t0
  species_t_50 <- readRDS(file.path(datapath,
                                    "output/config_worldcenter/species/species_t_50.rds"))
  # get space at t0
  space_t_50 <- readRDS(file.path(datapath,
                                      "output/config_worldcenter/spaces/space_t_50.rds"))
  # plot range
  plot_ranges(species_t_50, space_t_50)
  
  # get species at t0
  species_t_25 <- readRDS(file.path(datapath, 
                                    "output/config_worldcenter/species/species_t_25.rds"))
  # get space at t0
  space_t_25 <- readRDS(file.path(datapath, 
                                      "output/config_worldcenter/spaces/space_t_25.rds"))
  # plot ranges at intermediate time-step
  plot_ranges(species_t_25, space_t_25, disturb = 2, max_sps = 20)
  
  ## plot from within observer
  # call plot_richness from inside the end_of_timestep_observer function 
  # at the config file:
  plot_ranges(data$all_species, data$space)
}
