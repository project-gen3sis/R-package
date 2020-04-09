## running a simple simulation
library(gen3sis)

#get correct path or correct input objects
datapath <- system.file(file.path("extdata", "CaseStudy1"), package="gen3sis")

#run simulation
run_simulation(config = file.path(datapath,"config/config.R"), 
                input_directory = file.path(datapath,"landscape"))