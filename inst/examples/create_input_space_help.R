# # TODO use rasters?
# # load needed library
# library(raster)
# 
# # get path containing example rasters
# datapath <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")
# 
# # create raster bricks
# temperature_brick <- brick(file.path(datapath, "input_rasters/temp_rasters.grd"))
# aridity_brick <-  brick(file.path(datapath, "input_rasters/arid_rasters.grd"))
# area_brick <-  brick(file.path(datapath, "input_rasters/area_rasters.grd"))
# 
# # create sub-list of environmental variables for fast example
# # (i.e. 4 time-steps)
# spaces_sub_list <- list(temp=NULL, arid=NULL, area=NULL)
# for(i in 1:4){
#   spaces_sub_list$temp <- c(spaces_sub_list$temp, temperature_brick[[i]])
#   spaces_sub_list$arid <- c(spaces_sub_list$arid, aridity_brick[[i]])
#   spaces_sub_list$area <- c(spaces_sub_list$area, area_brick[[i]])
# }
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
# # \dontrun{
# # # create input space ready for gen3sis from sub-list
# # # (i.e. 10 time-steps) and only local-distances.
# # create_input_space(
# #   spaces = spaces_sub_list,
# #   cost_function = cost_function_water,
# #   output_directory = file.path(tempdir(), "space_sub"),
# #   directions = 8, # surrounding sites for each site
# #   timesteps = paste0(round(150:147,2), "Ma"),
# #   calculate_full_distance_matrices = FALSE) # full distance matrix
# #
# #
# # # create list of all environmental variables available
# # spaces_list <- list(temp=NULL, arid=NULL, area=NULL)
# # for(i in 1:nlayers(temperature_brick)){
# #   spaces_list$temp <- c(spaces_list$temp, temperature_brick[[i]])
# #   spaces_list$arid <- c(spaces_list$arid, aridity_brick[[i]])
# #   spaces_list$area <- c(spaces_list$area, area_brick[[i]])
# # }
# #
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
# # }
