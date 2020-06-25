# create empty config object
config_empty <- create_input_config(config_file = NA)

# create a config object from config_file
# get path to example config
datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")
path_config <- file.path(datapath, "config/config_worldcenter.R")
config_object <- create_input_config(config_file = path_config)

# change seed of config_worldcenter config object
config_object$gen3sis$general$random_seed <- 999


# run the model for config_object
\dontrun{
  sim <- run_simulation(config = config_object, landscape = file.path(datapath, "landscape"))
}