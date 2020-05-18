test_that("create_directories overwrite protection works", {
  skip("can't mock file.exists")
  # mocking base functions is no longer feasible, overwrite is not tested as it calls "unlink"
  new_dirs <- list()
  local_mock(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )
  # test overwrite protection
  local_mock(file.exists = function(...){ return(TRUE)} )
  expect_error(create_directories("test", overwrite = F, full_matrices = F),
               "output directory already exists", fixed = TRUE)
})


test_that("create_directories works", {
  # mocking base functions is no longer feasible, overwrite is not tested as it calls "unlink"
  new_dirs <- list()
  local_mock(dir.create = function(new_dir, ...){ new_dirs <<- append(new_dirs, new_dir)} )

  create_directories("test", overwrite = F, full_matrices = F)
  expect_true("test" %in% new_dirs)
  expect_true(!("test/distances_full" %in% new_dirs))

  # create directory for full matrices
  new_dirs <- list()
  create_directories("test", overwrite = F, full_matrices = T)
  expect_true("test" %in% new_dirs)
  expect_true("test/distances_local" %in% new_dirs)
  expect_true("test/distances_full" %in% new_dirs)

})


test_that("stack_landscapes from raster objects works", {
  # create rasters
  r11 <- raster(nrows = 5, ncols = 5, vals = runif(25))
  r12 <- raster(nrows = 5, ncols = 5, vals = runif(25))
  r21 <- raster(nrows = 5, ncols = 5, vals = runif(25))
  r22 <- raster(nrows = 5, ncols = 5, vals = runif(25))
  landscapes = list(l1 = c(r11, r12) , l2 = c(r21, r22))

  # check "timestep 1"
  stacked <- stack_landscapes(landscapes, 1)
  expect_true(all(names(stacked) == c("l1", "l2")))
  expect_true(all.equal(r11, stacked[["l1"]]))
  expect_true(all.equal(r21, stacked[["l2"]]))

  # check "timestep 2"
  stacked <- stack_landscapes(landscapes, 2)
  expect_true(all(names(stacked) == c("l1", "l2")))
  expect_true(all.equal(r12, stacked[["l1"]]))
  expect_true(all.equal(r22, stacked[["l2"]]))
})


test_that("habitability_mask works" , {
  r1 <- raster(nrows = 5, ncols = 5, vals = runif(25))
  h1 <- runif(25) > 0.2
  r1[!h1] <- NA
  r2 <- raster(nrows = 5, ncols = 5, vals = runif(25))
  h2 <- runif(25) > 0.2
  r2[!h2] <- NA
  h <- raster(nrows = 5, ncols = 5, vals = runif(25) < 0.2)

  # get landscapes
  landscapes = list(l1 = list(r1), l2 = list(r2))
  landscape_stack <- stack_landscapes(landscapes , 1)

  # habitability mask
  mask1 <- get_habitable_mask(list(h), landscape_stack, 1)
  expect_true(all.equal(mask1, h))

  # calculate habitability mask
  mask2 <- get_habitable_mask(NULL, landscape_stack, 1)
  expect_true(all.equal(mask2[], h1 & h2))
})


test_that("compile_landscapes works", {  # create rasters
  r11 <- raster(nrows = 5, ncols = 5, vals = runif(25))
  r12 <- raster(nrows = 5, ncols = 5, vals = runif(25))
  r21 <- raster(nrows = 5, ncols = 5, vals = runif(25))
  r22 <- raster(nrows = 5, ncols = 5, vals = runif(25))

  landscapes = list(l1 = c(r11, r12) , l2 = c(r21, r22))
  timesteps <- c(1,2)

  compiled_landscapes <- compile_landscapes(landscapes = landscapes,
                                            timesteps = timesteps,
                                            habitability_masks = NULL)

  expect_true(all.equal(names(compiled_landscapes), c("l1", "l2")))
  expect_true(all.equal(colnames(compiled_landscapes[["l1"]]), c("x", "y",  timesteps)))
  expect_true(all.equal(colnames(compiled_landscapes[["l2"]]), c("x", "y",  timesteps)))
  expect_true(all.equal(r11, rasterFromXYZ(compiled_landscapes[["l1"]][, c("x", "y", 1)])))
  expect_true(all.equal(r12, rasterFromXYZ(compiled_landscapes[["l1"]][, c("x", "y", 2)])))
  expect_true(all.equal(r21, rasterFromXYZ(compiled_landscapes[["l2"]][, c("x", "y", 1)])))
  expect_true(all.equal(r22, rasterFromXYZ(compiled_landscapes[["l2"]][, c("x", "y", 2)])))
  expect_true(all.equal(rownames(compiled_landscapes[["l1"]]),
                        rownames(compiled_landscapes[["l2"]]),
                        as.character(1:25)))
})


test_that("get_local_distances works", {
  crs <- NULL
  directions <- 8
  d <- 5
  cost_function <- function(src, habitable_src, dest, habitable_dest){
    return(src[1])
  }

  r1 <- raster(nrows = d, ncols = d, vals = runif(d*d))
  r2 <- raster(nrows = d, ncols = d, vals = runif(d*d))

  landscapes = list(l1 = list(r1) , l2 = list(r2))
  timesteps <- c(1)

  landscape_stack <- stack_landscapes(landscapes, 1)
  habitable_mask <- get_habitable_mask(habitability_masks = NULL, landscape_stack, 1)

  distance_local <- get_local_distances(landscape_stack, habitable_mask, cost_function, directions, crs)

  tr <- transition(r1, function(x){1/x[1]}, 8, symm = F)
  co <- geoCorrection(tr, "c", multpl = T)

  # the gdistance transition is in transition[src, dest] format
  # for efficiency reasons the local distances are in distnace_local[dest, src] format
  # for the comparison we transpose the transistion matrix
  # we take the reciprocal of the transition amtrix as it contains conductance values
  # while we use cost/resistance values in the local distances
  local_tr <- t((tr*co)@transitionMatrix)
  local_tr@x <- 1 / local_tr@x

  # correct input is produced
  expect_true(isTRUE(all.equal(unname(distance_local), local_tr)))

  # check row/colnames
  expect_true(identical(rownames(distance_local), as.character(1:(d*d))))
  expect_true(identical(colnames(distance_local), as.character(1:(d*d))))
})
