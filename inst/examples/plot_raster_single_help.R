# get path to output objects
datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")

# plot environmental variables at a given step
landscape_t_150 <- readRDS(
  file.path(datapath, "output", "config_worldcenter", "landscapes", "landscape_t_150.rds"))
par(mfrow=c(1,2))
plot_raster_single(landscape_t_150$environment[,"temp"], landscape_t_150, "Temperature", NA)
# use col to change the color
plot_raster_single(landscape_t_150$environment[,"prec"], landscape_t_150, "Aridity", NA, 
                   col=topo.colors(5))
# note that these values were scaled by the configuration object