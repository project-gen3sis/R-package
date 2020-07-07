\dontrun{
# run simulation
output <- run_simulation(config, landscape)
# plot output summary
plot_summary(output)
}

# load existing summary example
datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")
output <- readRDS(file.path(datapath, "output/config_worldcenter/sgen3sis.rds"))
# plot output summary
plot_summary(output)
