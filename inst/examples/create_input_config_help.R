# create empty config object
config_empty <- create_input_config(config_file = NA)

# create a config object from config_file
# get path to example config
datapath <- system.file(file.path("extdata/TestSpaces/geodynamic_spaces"), package="gen3sis2")
path_config <- system.file(file.path("extdata/TestConfigs/TestConfig.R"), package="gen3sis2")
config_object <- create_input_config(config_file = path_config, config_name = "your_config")

# change seed of config_worldcenter config object
config_object$gen3sis$general$random_seed <- 2025

# run the model for config_object
\donttest{
  sim <- run_simulation(config = config_object, 
                        space = file.path(datapath, "raster"), 
                        output_directory = tempdir())
}
