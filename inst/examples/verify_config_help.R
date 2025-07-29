library(gen3sis2)
# get path to input config
datapath <- system.file(file.path("extdata", "TestConfigs"), package="gen3sis2")
path_config <- file.path(datapath, "TestConfig.R")
# create config object
config_object <- create_input_config(path_config)
# check class
class(config_object)
# verify config
verify_config(config_object) # TRUE! this is a valid config

# break config_object, change name random_seed to r4nd0m_s33d
names(config_object$gen3sis$general)[1] <- "r4nd0m_s33d"
verify_config(config_object) # FALSE! this is an invalid config
