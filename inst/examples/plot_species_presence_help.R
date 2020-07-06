# get path to output objects
datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")

# load landscape and species at time step zero
landscape_t_0 <- readRDS(
  file.path(datapath, "output/config_worldcenter", "landscapes", "landscape_t_0.rds"))
species_t_0 <- readRDS(
  file.path(datapath, "output/config_worldcenter", "species", "species_t_0.rds"))

# plot species 21 range
plot_species_presence(species_t_0[[21]], landscape_t_0)
# oh, a South American one!

# plot ranges of 3 species (i.e. 1, 21 and 32)
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(1,3))
plot_species_presence(species_t_0[[1]], landscape_t_0)
plot_species_presence(species_t_0[[21]], landscape_t_0)
plot_species_presence(species_t_0[[32]], landscape_t_0)
par(oldpar)
