## running a simple simulation
library(gen3sis)

#get path or correct input objects
datapath <- system.file(file.path("extdata", "CaseStudy1"), package="gen3sis")

#run simulation and store summary to sim
\dontrun{
sim <- run_simulation(config = file.path(datapath,"config/config_fast.R"), 
                landscape = file.path(datapath,"landscape"))
}