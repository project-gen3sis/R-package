\dontrun{
  # get path containing example rasters
  datapath <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")
  # get species at t0
  species_t_0 <- readRDS(file.path(datapath, 
                          "output/config_worldcenter/species/species_t_0.rds"))
  # get landscape at t0
  landscape_t_0 <- readRDS(file.path(datapath, 
                            "output/config_worldcenter/landscapes/landscape_t_0.rds"))
  # get geo richness
  richness_t_0 <- get_geo_richness(species_t_0, landscape_t_0)
  
  # histogram of richness at t0
  hist(richness_t_0)
  
  ## plot richness using raster and gen3sis color_richness (see plot_richness for alternative)
  # combine richness and geographical coordinates
  geo_richness_t_0 <- cbind(landscape_t_0$coordinates, richness_t_0)
  plot(rasterFromXYZ(geo_richness_t_0), col=color_richness(20))
}