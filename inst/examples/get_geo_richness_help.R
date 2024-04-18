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

# get geo richness, i.e. richness per sites
richness_t_0 <- get_geo_richness(species_t_0, landscape_t_0)

# histogram of richness at t0
hist(richness_t_0)