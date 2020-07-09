\donttest{
# get path or correct input objects
datapath <- system.file(file.path("extdata", "CaseStudy1"), package="gen3sis")

# run simulation and store summary obejct to sim
sim <- run_simulation(config = file.path(datapath,"config/config_fast.R"), 
                landscape = file.path(datapath,"landscape"),
                output_directory = tempdir())

# plot summary object
plot_summary(sim)
}
