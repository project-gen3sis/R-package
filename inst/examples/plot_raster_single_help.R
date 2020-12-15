# get path to output objects
datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")

# plot environmental variables at a given step
landscape_t_25 <- readRDS(
  file.path(datapath, "output", "config_worldcenter", "landscapes", "landscape_t_25.rds"))
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(1,2))
plot_raster_single(landscape_t_25$environment[,"temp"], landscape_t_25, "Temperature", NA)
# use col to change the color
plot_raster_single(landscape_t_25$environment[,"arid"], landscape_t_25, "Aridity", NA, 
                   col=topo.colors(5))
par(oldpar)
# note that these values were scaled by the configuration object