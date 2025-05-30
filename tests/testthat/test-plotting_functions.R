# Auxiliary function for testing 
prepare_landcape_for_testing <- function(config,landscape,output_directory) {
  directories <- prepare_directories(config_file = config,
                                     input_directory = landscape,
                                     output_directory = output_directory)
  config <- create_input_config(config_file = config)
  config[["directories"]] <- directories
  val <- list("data" = list(),
              "vars" = list(),
              "config" = config)
  
  val$config <- complete_config(val$config)
  val$config$gen3sis$general$verbose <- 1
  val <- setup_inputs(val$config, val$data, val$vars)
  val <- setup_variables(val$config, val$data, val$vars)
  val <- setup_landscape(val$config, val$data, val$vars)
  return(val)
}

base_dir <- system.file(file.path("extdata"), package="gen3sis2")

# plot_species_presence

# plot_species_abundance

# plot_landscape
test_that("plot_landscape works", {
  # raster spacescape
  withr::with_tempdir({
    val <- prepare_landcape_for_testing(
      config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
      landscape = file.path(base_dir, "TestSpacescapes", "geodynamic_spaces", "raster"),
      output_directory = getwd()
    )
  })

  vdiffr::expect_doppelganger("plot_landscape_raster", plot_landscape(landscape = val$data$landscape))
  
  # h3 spacescape
  withr::with_tempdir({
    val <- prepare_landcape_for_testing(
      config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
      landscape = file.path(base_dir, "TestSpacescapes", "geodynamic_spaces", "h3"),
      output_directory = getwd()
    )
  })

  vdiffr::expect_doppelganger("plot_landscape_h3", plot_landscape(landscape = val$data$landscape))
})

# plot_landscape_overview
test_that("plot_landscape_overview works",{
  skip()
})

# plot_summary

# plot_richness

# set_color

# plot_ranges

# plot_single

# plot_multiple