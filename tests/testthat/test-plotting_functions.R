# # Auxiliary function for testing 
# prepare_landcape_for_testing <- function(config,space,output_directory) {
#   directories <- prepare_directories(config_file = config,
#                                      input_directory = space,
#                                      output_directory = output_directory)
#   config <- create_input_config(config_file = config)
#   config[["directories"]] <- directories
#   val <- list("data" = list(),
#               "vars" = list(),
#               "config" = config)
#   
#   val$config <- complete_config(val$config)
#   val$config$gen3sis$general$verbose <- 1
#   val <- setup_inputs(val$config, val$data, val$vars)
#   val <- setup_variables(val$config, val$data, val$vars)
#   val <- setup_space(val$config, val$data, val$vars)
#   return(val)
# }
# 
# base_dir <- system.file(file.path("extdata"), package="gen3sis2")
# 
# # plot_species_presence
# test_that("plot_species_presence works",{
#   config <- file.path(base_dir, "TestConfigs", "TestConfig.R") |>
#     create_input_config()
#   
#   # gen3sis_space_raster
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "raster"),
#       output_directory = getwd()
#     )
#   })
#   
#   sp <- gen3sis2::create_species(as.character(c(7,19,43)),
#                                  config)
#   
#   vdiffr::expect_doppelganger("plot_species_presence_raster", plot_species_presence(sp,val$data$space))
#   
#   # gen3sis_space_h3
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "h3"),
#       output_directory = getwd()
#     )
#   })
#   
#   sp <- gen3sis2::create_species(as.character(c(7,19,43)),
#                                  config)
#   
#   vdiffr::expect_doppelganger("plot_species_presence_h3", plot_species_presence(sp,val$data$space))
#   
#   # gen3sis_space_points
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "points"),
#       output_directory = getwd()
#     )
#   })
#   
#   sp <- gen3sis2::create_species(as.character(c(7,19,43)),
#                                  config)
#   
#   vdiffr::expect_doppelganger("plot_species_presence_points", plot_species_presence(sp,val$data$space))
# })
# 
# # plot_species_abundance
# test_that("plot_species_presence works",{
#   config <- file.path(base_dir, "TestConfigs", "TestConfig.R") |>
#     create_input_config()
#   
#   # gen3sis_space_raster
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "raster"),
#       output_directory = getwd()
#     )
#   })
#   
#   sp <- gen3sis2::create_species(as.character(c(7,19,43)),
#                                  config)
#   
#   vdiffr::expect_doppelganger("plot_species_abundance_raster", plot_species_abundance(sp,val$data$space))
#   
#   # gen3sis_space_h3
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "h3"),
#       output_directory = getwd()
#     )
#   })
#   
#   sp <- gen3sis2::create_species(as.character(c(7,19,43)),
#                                  config)
#   
#   vdiffr::expect_doppelganger("plot_species_abundance_h3", plot_species_abundance(sp,val$data$space))
#   
#   # gen3sis_space_points
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "points"),
#       output_directory = getwd()
#     )
#   })
#   
#   sp <- gen3sis2::create_species(as.character(c(7,19,43)),
#                                  config)
#   
#   vdiffr::expect_doppelganger("plot_species_abundance_points", plot_species_abundance(sp,val$data$space))
# })
# 
# # plot_space
# test_that("plot_space works", {
#   # raster space
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "raster"),
#       output_directory = getwd()
#     )
#   })
# 
#   {
#     set.seed(13)
#     mock_variable <- runif(nrow(val$data$space$environment))
#   }
# 
#   mock_variable <- matrix(mock_variable, ncol = 1, nrow = nrow(val$data$space$environment))
#   colnames(mock_variable) <- "any_var"
#   val$data$space$environment <- cbind(val$data$space$environment, mock_variable)
#   
#   vdiffr::expect_doppelganger("plot_space_raster", plot_space(space = val$data$space))
#   
#   # h3 space
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "h3"),
#       output_directory = getwd()
#     )
#   })
#   
#   vdiffr::expect_doppelganger("plot_space_h3", plot_space(space = val$data$space))
#   
#   # points space
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "points"),
#       output_directory = getwd()
#     )
#   })
#   
#   vdiffr::expect_doppelganger("plot_space_points", plot_space(space = val$data$space))
# })
# 
# # plot_space_overview
# # TODO
# 
# # plot_summary
# test_that("plot_summary works",{
#   config <- create_input_config(file.path(base_dir,"TestConfigs/TestConfig.R"))
#   
#   withr::with_tempdir({
#     # geostatic
#     ## raster
#     input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","raster") 
#     capture.output({
#       s <- run_simulation(
#         config = config,
#         space = input_variables,
#         output_directory = getwd()
#       )
#     }) |> suppressWarnings()
#     
#     vdiffr::expect_doppelganger("plot_summary_raster", plot_summary(s))
#     
#     # ## h3
#     # input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","h3")
#     # capture.output({
#     #   s <- run_simulation(
#     #     config = config,
#     #     space = input_variables,
#     #     output_directory = getwd()
#     #   )
#     # }) |> suppressWarnings()
#     # 
#     # vdiffr::expect_doppelganger("plot_summary_h3", plot_summary(s))
#     # 
#     # ## points
#     # input_variables <- file.path(base_dir,"TestSpaces","geostatic_spaces","points")
#     # capture.output({
#     #   s <- run_simulation(
#     #     config = config,
#     #     space = input_variables,
#     #     output_directory = getwd()
#     #   )
#     # }) |> suppressWarnings()
#     # 
#     # vdiffr::expect_doppelganger("plot_summary_points", plot_summary(s))
#   })
# })
# 
# # plot_richness
# test_that("plot_richness works",{
#   config <- file.path(base_dir, "TestConfigs", "TestConfig.R") |>
#     create_input_config()
#   
#   # gen3sis_space_raster
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "raster"),
#       output_directory = getwd()
#     )
#   })
#   
#   species_list <- list(
#     gen3sis2::create_species(as.character(c(7,15,19)), config),
#     gen3sis2::create_species(as.character(c(7,19,43)), config),
#     gen3sis2::create_species(as.character(c(7,43,47)), config)
#   )
#   
#   
#   vdiffr::expect_doppelganger("plot_richness_raster", plot_richness(species_list, val$data$space))
#   
#   # gen3sis_space_h3
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "h3"),
#       output_directory = getwd()
#     )
#   })
#   
#   species_list <- list(
#     gen3sis2::create_species(as.character(c(7,15,19)), config),
#     gen3sis2::create_species(as.character(c(7,19,43)), config),
#     gen3sis2::create_species(as.character(c(7,43,47)), config)
#   )
#   
#   
#   vdiffr::expect_doppelganger("plot_richness_h3", plot_richness(species_list, val$data$space))
#   
#   # gen3sis_space_h3
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "points"),
#       output_directory = getwd()
#     )
#   })
#   
#   species_list <- list(
#     gen3sis2::create_species(as.character(c(7,15,19)), config),
#     gen3sis2::create_species(as.character(c(7,19,43)), config),
#     gen3sis2::create_species(as.character(c(7,43,47)), config)
#   )
#   
#   
#   vdiffr::expect_doppelganger("plot_richness_points", plot_richness(species_list, val$data$space))
# })
# 
# # set_color
# 
# # plot_ranges
# test_that("plot_ranges works",{
#   config <- file.path(base_dir, "TestConfigs", "TestConfig.R") |>
#     create_input_config()
#   
#   # gen3sis_space_raster
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "raster"),
#       output_directory = getwd()
#     )
#   })
#   
#   species_list <- list(
#     gen3sis2::create_species(as.character(c(7,15,19)), config),
#     gen3sis2::create_species(as.character(c(7,19,43)), config),
#     gen3sis2::create_species(as.character(c(7,43,47)), config)
#   )
#   
#   
#   vdiffr::expect_doppelganger("plot_ranges_raster", plot_ranges(species_list, val$data$space, disturb = 1))
#   
#   # gen3sis_space_h3
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "h3"),
#       output_directory = getwd()
#     )
#   })
#   
#   species_list <- list(
#     gen3sis2::create_species(as.character(c(7,15,19)), config),
#     gen3sis2::create_species(as.character(c(7,19,43)), config),
#     gen3sis2::create_species(as.character(c(7,43,47)), config)
#   )
#   
#   
#   vdiffr::expect_doppelganger("plot_ranges_h3", plot_ranges(species_list, val$data$space, disturb = 1))
#   
#   # gen3sis_space_points
#   withr::with_tempdir({
#     val <- prepare_landcape_for_testing(
#       config = file.path(base_dir, "TestConfigs", "TestConfig.R"),
#       space = file.path(base_dir, "TestSpaces", "geodynamic_spaces", "points"),
#       output_directory = getwd()
#     )
#   })
#   
#   species_list <- list(
#     gen3sis2::create_species(as.character(c(7,15,19)), config),
#     gen3sis2::create_species(as.character(c(7,19,43)), config),
#     gen3sis2::create_species(as.character(c(7,43,47)), config)
#   )
#   
#   
#   vdiffr::expect_doppelganger("plot_ranges_points", plot_ranges(species_list, val$data$space, disturb = 1))
# })
# 
# # plot_single
# 
# # plot_multiple
