\dontrun{
library(terra)
library(gen3sis2)
# Get some rasters for each timestep
temperature <- terra::rast("your/temperature_raster.tif")
aridity <- terra::rast("your/aridity_raster.tif")
precipitation <- terra::rast("your/precipitation_raster.tif")
  
# Organize them
environmental_variables <- list(
  temperature = temperature,
  aridity = aridity,
  precipitation = precipitation
)
  
create_spaces_raster(
  raster_list = environmental_variables,
  cost_function = function(source, dest) { # any cost function
    if (!all(source$habitable, dest$habitable)) {
      return(2 / 1000)
    } else {
      return(1 / 1000)
    }
  },
  directions = 8,
  output_directory = "./where/to/save",
  full_dists = TRUE, # save full distance matrices
  overwrite_output = TRUE,
  verbose = TRUE,
  duration = list(from = 65, to = 0, by = -1, unit = "Ma"),
  geodynamic = TRUE
)
}

\dontshow{
  # TODO deprecated?
  # # # get path containing example rasters
  # # datapath <- "C:/temp/4ds/world60by10at4d/temp_rasters/" #system.file(file.path("extdata", "WorldCenter"), package="gen3sis")
  # #
  # #
  # #
  # # # create list of raster bricks
  # # envs_list <- list(temp=brick(file.path(datapath, "temp.grd")), arid=brick(file.path(datapath, "prec.grd")))
  # #
  # #
  # # # ## WIP
  # datapath <- "C:/Program Files/R/R-4.2.2/library/gen3sis/extdata/WorldCenter/input_rasters/"
  # #envs_list <- list(temp=brick(file.path(datapath, "temp_rasters.grd")), arid=brick(file.path(datapath, "arid_rasters.grd")))
  # 
  # temperature_brick <- brick(file.path(datapath, "temp_rasters.grd"))
  # aridity_brick <-  brick(file.path(datapath, "arid_rasters.grd"))
  # #spaces_sub_list <- list(temp=temperature_brick, arid=aridity_brick)
  # 
  # spaces_sub_list <- list(temp=NULL, arid=NULL)
  # for(i in 1:4){ #nlayers(envs_list[[1]])
  #   spaces_sub_list$temp <- c(spaces_sub_list$temp, temperature_brick[[i]])
  #   spaces_sub_list$arid <- c(spaces_sub_list$arid, aridity_brick[[i]])
  # }
  # 
  # 
  # #create sub-list of environmental variables for fast example
  # # (i.e. 4 time-steps)
  # # spaces_sub_list <- list(temp=NULL, arid=NULL)
  # # for(i in 1:nlayers(envs_list[[1]])){
  # #   spaces_sub_list$temp <- c(spaces_sub_list$temp, envs_list$temp[[i]])
  # #   spaces_sub_list$arid <- c(spaces_sub_list$arid, envs_list$arid[[i]])
  # # }
  # 
  # # spaces <- spaces_sub_list
  # 
  # 
  # # END WIP
  # 
  # 
  # # define cost function, crossing water as double as land sites
  # cost_function_water <- function(source, habitable_src, dest, habitable_dest) {
  #   if(!all(habitable_src, habitable_dest)) {
  #     return(2/1000)
  #   } else {
  #     return(1/1000)
  #   }
  # }
  # 
  # \dontrun{
  # # create input space ready for gen3sis from sub-list
  # # (i.e. 10 time-steps) and only local-distances.
  # 
  # create_spaces_raster(raster_list = spaces_sub_list,
  #                     cost_function = cost_function_water,
  #                     directions = 16, # surrounding sites for each site
  #                     output_directory = file.path("C:/temp/test_spaces", "spaceS2"),
  #                     full_dists = TRUE,
  #                     overwrite_output = TRUE,
  #                     duration=list(from=-60, to=-30, by=10, unit="Ma"))
  # 
  # 
  # my_description <- "Temperatures on current koppen bands were extracted for each Koppen band (5Ma resolution)(Hagen et al. 2019)
  # and had a focal mean applied at a strength that mimics empirical present temperature(WorldClim2 2018) spread for the koppen zones.
  # These were first interpolated at a resolution of 1Ma. Lapse rates for each zones (Hagen et al. 2019) were applied to the
  # respective elevation maps (Straume et al. 2020) also available at a resolution of 1Ma. Air surface temperatures were applied.
  # LGM and LTG had strength corrected to match reference global air surface temperature maps (Westerhold et al. 2020).
  # Global temperature differences were calculated using entire koppen band to account for sea surface temperature
  # (Westerhold et al. 2020). For more details see (Hagen et al 2020/2021)."
  # 
  # create_spaces_raster(raster_list= spaces_sub_list, # old spaces
  #                                 cost_function = cost_function_water,
  #                                 output_directory = file.path("C:/temp/test_spaces", "spaceS3"),
  #                                 full_dists = TRUE,
  #                                 overwrite_output = TRUE,
  #                                 duration=list(from=-60, to=-30, by=10, unit="Ma"),
  #                                 author="Oskar Hagen",
  #                                 source="10.1371/journal.pbio.3001340",
  #                                 description=list(env="temperature in degree celcius;
  #                                     humidity proxy from zero to one (respectively dry and arid)",
  #                                                    methods=my_description))
  # 
  # 
  # create_space_raster(env = spaces_sub_list,
  #   cost_function = cost_function_water,
  #   output_directory = file.path(tempdir(), "space_sub"),
  #   directions = 8, # surrounding sites for each site
  #   calculate_full_distance_matrices = FALSE) # full distance matrix
  # 
  # 
  # # create list of all environmental variables available
  # spaces_list <- list(temp=NULL, arid=NULL, area=NULL)
  # for(i in 1:nlayers(temperature_brick)){
  #   spaces_list$temp <- c(spaces_list$temp, temperature_brick[[i]])
  #   spaces_list$arid <- c(spaces_list$arid, aridity_brick[[i]])
  #   spaces_list$area <- c(spaces_list$area, area_brick[[i]])
  # }
  # 
  # # # create input space ready for gen3sis (~ 3min run-time)
  # # # and full distance matrix
  # # create_input_space(
  # #   spaces = spaces_list,
  # #   cost_function = cost_function_water,
  # #   output_directory = file.path(tempdir(), "space_WorldCenter_5"),
  # #   directions = 8, # surrounding sites for each site
  # #   timesteps = paste0(round(150:100,2), "Ma"),
  # #   crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
  # #   calculate_full_distance_matrices = FALSE) # full distance matrix
  # }  
}
