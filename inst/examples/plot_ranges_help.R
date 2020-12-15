## plot from saved outputs
# get path containing outputs
datapath <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")
# get species at t0
species_t_50 <- readRDS(file.path(datapath,
                        "output/config_worldcenter/species/species_t_50.rds"))
# get landscape at t0
landscape_t_50 <- readRDS(file.path(datapath,
                          "output/config_worldcenter/landscapes/landscape_t_50.rds"))
# plot range
plot_ranges(species_t_50, landscape_t_50)

# get species at t0
species_t_25 <- readRDS(file.path(datapath, 
                        "output/config_worldcenter/species/species_t_25.rds"))
# get landscape at t0
landscape_t_25 <- readRDS(file.path(datapath, 
                        "output/config_worldcenter/landscapes/landscape_t_25.rds"))
# plot ranges at intermediate time-step
plot_ranges(species_t_25, landscape_t_25, disturb = 2, max_sps = 20)

## plot from within observer
# call plot_richness from inside the end_of_timestep_observer function 
# at the config file:
\dontrun{
plot_ranges(data$all_species, data$landscape)
} 
