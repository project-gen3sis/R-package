\dontshow{
  #TODO change this to a working example
}

\dontrun{
  # load existing summary example
  datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")
  output <- readRDS(file.path(datapath, "output/config_worldcenter/sgen3sis.rds"))
  # plot output summary
  plot_summary(output)
  
  plot_summary(output, summary_title="Example")
  
  ## run simulation and plot summary
  # get path or correct input objects
  datapath <- system.file(file.path("extdata", "CaseStudy1"), package="gen3sis")
  # run simulation and store summary object to output
  output <- run_simulation(config = file.path(datapath,"config/config_fast.R"), 
                           landscape = file.path(datapath,"landscape"),
                           output_directory = tempdir())
  # plot output summary
  plot_summary(output)
}