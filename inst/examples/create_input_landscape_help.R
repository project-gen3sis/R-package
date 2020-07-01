\dontrun{
# load needed library
library(raster)

# get path containing example rasters
datapath <- system.file(file.path("extdata", "InputRasters"), package="gen3sis")

# create raster bricks
temperature_brick <- brick(file.path(datapath, "WorldCenter/temp_rasters.grd"))
aridity_brick <-  brick(file.path(datapath, "WorldCenter/arid_rasters.grd"))
area_brick <-  brick(file.path(datapath, "WorldCenter/area_rasters.grd"))

# create list of environmental variables
landscapes_list <- list(temp=NULL, arid=NULL, area=NULL)
for(i in 1:nlayers(temperature_brick)){
  landscapes_list$temp <- c(landscapes_list$temp, temperature_brick[[i]])
  landscapes_list$arid <- c(landscapes_list$arid, aridity_brick[[i]])
  landscapes_list$area <- c(landscapes_list$area, area_brick[[i]])
}

# define cost function, crossing water as double as land sites
cost_function_water <- function(source, habitable_src, dest, habitable_dest) {
  if(!all(habitable_src, habitable_dest)) {
    return(2)
  } else {
    return(1)
  }
}

# CAUTION! THIS TAKES A LONG TIME!
# create input landscape ready for gen3sis
create_input_landscape(
  landscapes = landscapes_list,
  cost_function = cost_function_water(),
  output_directory = "OUTPUT_DIRECTORY_HERE",
  directions = 8, # surrounding sites for each site
  timesteps = paste0(round(seq(150, 100, length.out = 301),2), "Ma"),
  calculate_full_distance_matrices = TRUE) # full distance matrix

}