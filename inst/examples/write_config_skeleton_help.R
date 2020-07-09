# set config_empty.R file path
config_file_path <- file.path(tempdir(), "config_empty.R")
#writes out a config skeleton
write_config_skeleton(config_file_path)