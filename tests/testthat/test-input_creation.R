# create_spaces_raster
## Tests for general workink
test_that("create_spaces_raster works properly",{
  # create a 3 layers temp raster
  r <- rep(terra::rast(nrows = 5, 
                       ncols = 5,
                       xmin = 0,
                       xmax = 5,
                       ymin = 0,
                       ymax = 5,
                       vals = runif(25)),3)
  
  # define a cost function
  cf <- function(source, dest){
    if(!all(source$habitable, dest$habitable)) {
      return(2 / 1000)
    } else {
      return(1 / 1000)
    }
  }
  
  # test core
  # Geostatic
  withr::with_tempdir({
    create_spaces_raster(
      raster_list = list(some_var = r),
      cost_function = cf,
      directions=4,
      output_directory = file.path(getwd(), "output"),
      full_dists = TRUE,
      overwrite_output = TRUE,
      verbose = FALSE,
      duration=list(from=100, to=0, by=-50, unit="Ma"),
      geodynamic=FALSE
    )
    
    files_created <- list.files(file.path(getwd(), "output"), recursive = T)
    files_expected <- c("distances_full/distances_full_0.rds","distances_local/distances_local_0.rds","spaces.rds")
    expect_true(all(files_created %in% files_expected))
  })
  
  # Geodynamic
  
  # modify the raster
  values(r[[2]])[13,1] <- NA
  values(r[[3]])[12,1] <- NA
  values(r[[3]])[13,1] <- NA
  values(r[[3]])[14,1] <- NA
  
  withr::with_tempdir({
    create_spaces_raster(
      raster_list = list(some_var = r),
      cost_function = cf,
      directions=4,
      output_directory = file.path(getwd(), "output"),
      full_dists = TRUE,
      overwrite_output = TRUE,
      verbose = FALSE,
      duration=list(from=100, to=0, by=-50, unit="Ma"),
      geodynamic=TRUE
    )
    
    files_created <- list.files(file.path(getwd(), "output"), recursive = T)
    files_expected <- c("distances_full/distances_full_0.rds"
                        ,"distances_full/distances_full_1.rds"  
                        ,"distances_full/distances_full_2.rds"
                        ,"distances_local/distances_local_0.rds"
                        ,"distances_local/distances_local_1.rds"
                        ,"distances_local/distances_local_2.rds"
                        ,"spaces.rds")
    expect_true(all(files_created %in% files_expected))
  })
})

## Tests for wrong usage
test_that("NA duration in create_spaces_raster",{
  # create a 3 layers temp raster
  r <- rep(terra::rast(nrows = 5, 
                       ncols = 5,
                       xmin = 0,
                       xmax = 5,
                       ymin = 0,
                       ymax = 5,
                       vals = runif(25)),3)
  
  # define a cost function
  cf <- function(source, dest){
    if(!all(source$habitable, dest$habitable)) {
      return(2 / 1000)
    } else {
      return(1 / 1000)
    }
  }
  
  # test core
  # Geostatic
  withr::with_tempdir({
    expect_warning(
      create_spaces_raster(
        raster_list = list(some_var = r),
        cost_function = cf,
        directions=4,
        output_directory = file.path(getwd(), "output"),
        full_dists = TRUE,
        overwrite_output = TRUE,
        verbose = FALSE,
        duration=NA,
        geodynamic=FALSE
      ), "Duration is ideally informed as a list with from, to, by and unit. 
            Assuning default duration from -latest time to zero by 1 Ma")
  })
})

## test if it recognizes geodynamic landscapes
test_that("geodynamic is FALSE but environment says otherwise", {
  # create a 3 layers temp raster
  r <- rep(terra::rast(
    nrows = 5,
    ncols = 5,
    xmin = 0,
    xmax = 5,
    ymin = 0,
    ymax = 5,
    vals = runif(25)
  ),
  3)
  
  values(r[[2]])[13, 1] <- NA
  values(r[[3]])[12, 1] <- NA
  values(r[[3]])[13, 1] <- NA
  values(r[[3]])[14, 1] <- NA
  
  # define a cost function
  cf <- function(source, dest){
    if(!all(source$habitable, dest$habitable)) {
      return(2 / 1000)
    } else {
      return(1 / 1000)
    }
  }
  
  withr::with_tempdir({
      create_spaces_raster(
        raster_list = list(some_var = r),
        cost_function = cf,
        directions = 4,
        output_directory = file.path(getwd(), "output"),
        full_dists = TRUE,
        overwrite_output = TRUE,
        verbose = FALSE,
        duration = list(
          from = 100,
          to = 0,
          by = -50,
          unit = "Ma"
        ),
        geodynamic = FALSE
      ) |> capture_warnings() -> warned
    
    expect_equal("geodynamic is set to FALSE but environment says otherwise. \n          changing geodynamic to TRUE",
                 warned) # TODO for some reason, expect_warning was not working here, need to try again
  })
})

## tests for landscape internal handling
test_that("stack_landscapes from raster objects works", {
  # create rasters
  v1t1 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  v1t2 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  v2t1 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  v2t2 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  
  landscapes = list(v1 = c(v1t1, v1t2) , v2 = c(v2t1, v2t2))
  
  # check "time-step 1"
  stacked <- stack_landscapes(landscapes, 1)
  
  expect_true(all(names(stacked) == c("v1", "v2")))
  expect_true(all(terra::values(v1t1) == terra::values(stacked[["v1"]])))
  expect_true(all(terra::values(v2t1) == terra::values(stacked[["v2"]])))
  
  # check "time-step 2"
  stacked <- stack_landscapes(landscapes, 2)
  
  expect_true(all(names(stacked) == c("v1", "v2")))
  expect_true(all(terra::values(v1t2) == terra::values(stacked[["v1"]])))
  expect_true(all(terra::values(v2t2) == terra::values(stacked[["v2"]])))
})

test_that("habitability_mask works" , {
  r1 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  h1 <- runif(25) > 0.2
  r1[!h1] <- NA
  r2 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  h2 <- runif(25) > 0.2
  r2[!h2] <- NA
  h <- terra::rast(nrows = 5, ncols = 5, vals = runif(25) < 0.2)
  
  # get landscapes
  landscapes = list(l1 = r1, l2 = r2)
  landscape_stack <- stack_landscapes(landscapes , 1)
  
  # habitability mask
  mask1 <- get_habitable_mask(list(h), landscape_stack, 1)
  expect_true(all.equal(mask1, h))
  
  # TODO calculate habitability mask
  # mask2 <- get_habitable_mask(NULL, landscape_stack, 1)
  # expect_true(all.equal(mask2[], h1 & h2))
})

test_that("compile_landscapes works", {  # create rasters
  r11 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  r12 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  r21 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  r22 <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  
  landscapes = list(l1 = c(r11, r12) , l2 = c(r21, r22))
  timesteps <- c(1,2)
  
  compiled_landscapes <- compile_landscapes(landscapes = landscapes,
                                            timesteps = timesteps,
                                            habitability_masks = NULL)
  
  expect_true(all.equal(names(compiled_landscapes), c("l1", "l2")))
  expect_true(all.equal(colnames(compiled_landscapes[["l1"]]), c("x", "y",  timesteps)))
  expect_true(all.equal(colnames(compiled_landscapes[["l2"]]), c("x", "y",  timesteps)))
  expect_true(all(terra::values(r11) == terra::values(terra::rast(compiled_landscapes[["l1"]][, c("x", "y", 1)], type="xyz"))))
  expect_true(all(terra::values(r12) == terra::values(terra::rast(compiled_landscapes[["l1"]][, c("x", "y", 2)], type="xyz"))))
  expect_true(all(terra::values(r21) == terra::values(terra::rast(compiled_landscapes[["l2"]][, c("x", "y", 1)], type="xyz"))))
  expect_true(all(terra::values(r22) == terra::values(terra::rast(compiled_landscapes[["l2"]][, c("x", "y", 2)], type="xyz"))))
  expect_true(all.equal(rownames(compiled_landscapes[["l1"]]),
                        rownames(compiled_landscapes[["l2"]]),
                        as.character(1:25)))
})

# TODO needs to be improved
test_that("get_local_distances works", {
  crs <- NULL
  directions <- 8
  d <- 5
  cost_function <- function(source, dest){
    return(source$index)
  }
  
  r1 <- terra::rast(nrows = d, ncols = d, vals = runif(d*d))
  r2 <- terra::rast(nrows = d, ncols = d, vals = runif(d*d))
  
  landscapes = list(l1 = c(r1) , l2 = c(r2))
  timesteps <- c(1)
  
  landscape_stack <- stack_landscapes(landscapes, 1)
  habitable_mask <- get_habitable_mask(habitability_masks = NULL, landscape_stack, 1)
  
  distance_local <- get_local_distances(landscape_stack, habitable_mask, cost_function, directions, crs)
  
  expect_true(all(c(25,25)==dim(distance_local)))
  expect_s4_class(distance_local,"dgCMatrix")
  #   tr <- transition(r1, function(x){1/x[1]}, 8, symm = FALSE)
  #   co <- geoCorrection(tr, "c", multpl = TRUE)
  # 
  #   # the gdistance transition is in transition[src, dest] format
  #   # for efficiency reasons the local distances are in distnace_local[dest, src] format
  #   # for the comparison we transpose the transition matrix
  #   # we take the reciprocal of the transition matrix as it contains conductance values
  #   # while we use cost/resistance values in the local distances
  #   local_tr <- t((tr*co)@transitionMatrix)
  #   local_tr@x <- 1 / local_tr@x
  # 
  #   # correct input is produced?
  #   expect_true(isTRUE(all.equal(unname(distance_local), local_tr)))
  # 
  #   # check row/colnames, are they identical?
  #   expect_true(identical(rownames(distance_local), as.character(1:(d*d))))
  #   expect_true(identical(colnames(distance_local), as.character(1:(d*d))))
})

# Old tests
test_that("create_directories overwrite protection works", {
  skip("can't mock file.exists")
  # mocking base functions is no longer feasible, overwrite is not tested as it calls "unlink"
  new_dirs <- list()
  local_mock(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )
  # test overwrite protection
  local_mock(file.exists = function(...){ return(TRUE)} )
  expect_error(create_directories("test", overwrite = FALSE, full_matrices = FALSE),
               "output directory already exists", fixed = TRUE)
})

test_that("create_directories works", {
  # mocking base functions is no longer feasible, overwrite is not tested as it calls "unlink"
  new_dirs <- list()
  local_mock(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )

  create_directories("test", overwrite = FALSE, full_matrices = FALSE)
  expect_true("test" %in% new_dirs)
  expect_true(!("test/distances_full" %in% new_dirs))

  # create directory for full matrices
  new_dirs <- list()
  create_directories("test", overwrite = FALSE, full_matrices = TRUE)
  expect_true("test" %in% new_dirs)
  expect_true("test/distances_local" %in% new_dirs)
  expect_true("test/distances_full" %in% new_dirs)

})