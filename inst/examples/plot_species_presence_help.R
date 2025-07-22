\dontshow{
  #TODO change this to a working example
}

\dontrun{
  # get path to output objects
  datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")
  
  # load space and species at time step zero
  space_t_0 <- readRDS(
    file.path(datapath, "output/config_worldcenter", "spaces", "space_t_0.rds"))
  species_t_0 <- readRDS(
    file.path(datapath, "output/config_worldcenter", "species", "species_t_0.rds"))
  
  # plot species 13 range
  plot_species_presence(species_t_0[[13]], space_t_0)
  # oh, a South American one!
  
  # plot ranges of 3 species (i.e. 1, 21 and 32)
  oldpar <- par(no.readonly = TRUE)
  par(mfrow=c(1,3))
  plot_species_presence(species_t_0[[1]], space_t_0)
  plot_species_presence(species_t_0[[7]], space_t_0)
  plot_species_presence(species_t_0[[11]], space_t_0)
  par(oldpar)
}
