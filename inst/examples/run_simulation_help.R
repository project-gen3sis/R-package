\donttest{
# get path or correct input objects
spaces <- system.file(file.path("extdata", "TestSpaces","geostatic_spaces","raster"), package="gen3sis2")
config_file <- system.file(file.path("extdata", "TestConfigs","TestConfig.R"), package="gen3sis2")
# run simulation and store summary obejct to sim
sim <- run_simulation(
  config = config_file, 
  space = spaces,
  output_directory = tempdir()
)

# plot summary object
plot_summary(sim)
}
