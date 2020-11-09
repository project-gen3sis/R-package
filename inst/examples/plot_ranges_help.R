## plot from saved outputs
# get path containing example rasters
datapath <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")
# get species at t0
species_t_300 <- readRDS(file.path(datapath, 
                        "output/config_worldcenter/species/species_t_300.rds"))
# get landscape at t0
landscape_t_300 <- readRDS(file.path(datapath, 
                          "output/config_worldcenter/landscapes/landscape_t_300.rds"))
# plot range
plot_ranges(species_t_300, landscape_t_300)


# get species at t0
species_t_150 <- readRDS(file.path(datapath, 
                                   "output/config_worldcenter/species/species_t_150.rds"))
# get landscape at t0
landscape_t_150 <- readRDS(file.path(datapath, 
                                     "output/config_worldcenter/landscapes/landscape_t_150.rds"))

# plot ranges at intermediate time-step
plot_ranges(species_t_150, landscape_t_150, distrurbance = 1.5, max_sps = 9)






## plot from within observer
# call plot_richness from inside the end_of_timestep_observer function 
# at the config file:
\dontrun{
plot_ranges(data$all_species, data$landscape)
} 