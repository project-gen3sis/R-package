\dontrun{
  # this is an internal function used to attribute directories by deduction
  # called at the start of a simulation run
  datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")
  # deducing input directory and setting output directory
  prepare_directories(config_file = file.path(datapath, "config/config_worldcenter.R"))
  # setting output directory
  prepare_directories(config_file = file.path(datapath, "config/config_worldcenter.R"), 
                      input_directory = file.path(datapath, "landscape"))
}