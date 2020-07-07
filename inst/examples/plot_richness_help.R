## plot from saved outputs
# get path containing example rasters
datapath <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")
# get species at t0
species_t_0 <- readRDS(file.path(datapath, 
                        "output/config_worldcenter/species/species_t_0.rds"))
# get landscape at t0
landscape_t_0 <- readRDS(file.path(datapath, 
                          "output/config_worldcenter/landscapes/landscape_t_0.rds"))
# plot richness
plot_richness(species_t_0, landscape_t_0)


## plot from within observer
# call plot_richness from inside the end_of_timestep_observer function 
# at the config file:
\dontrun{
  plot_richness(data$all_species, data$landscape)
}  