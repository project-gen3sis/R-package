\dontshow{
  #TODO change this to a working example
}

\dontrun{
# get path to output objects
datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")

# plot environmental variables at a given step
space_t_25 <- readRDS(
  file.path(datapath, "output", "config_worldcenter", "spaces", "space_t_25.rds"))
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(1,2))
plot_raster_single(space_t_25$environment[,"temp"], space_t_25, "Temperature", NA)
# use col to change the color
plot_raster_single(space_t_25$environment[,"arid"], space_t_25, "Aridity", NA, 
                   col=topo.colors(5))
par(oldpar)
# note that these values were scaled by the configuration object
}
