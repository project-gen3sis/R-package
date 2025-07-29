\dontrun{
  # this is an internal function used to attribute directories by deduction
  # called at the start of a simulation run

  prepare_directories(
    config_file = "./any/config.R", # must be provided
    input_directory = "./any/space/dir/", # if NA, will be derived from config_file
    output_directory = "./any/output/dir" # if NA, will be derived from config_file
  )
}