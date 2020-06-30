\dontrun{
# get path containing example rasters
datapath <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")
# get species
species_t_0 <- readRDS(file.path(datapath, "output/config_worldcenter/species/species_t_0.rds"))
# get divergence matrix from species 1
divergence_sp1_t0 <- get_divergence_matrix(species_t_0[[1]])
# get divergence matrix from species 12
divergence_sp12_t0 <- get_divergence_matrix(species_t_0[[12]])
# note that species 1 has no divergence between it's populations, while 12 has.
}