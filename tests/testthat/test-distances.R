# skip_on_cran()
# skip_on_travis()

test_that("distance matrix creation works", {
  skip("Need to find alternatives to gdistance")
  num_cells <- 10
  ras <- raster(nrows = num_cells, ncols = num_cells, vals = 1+runif(num_cells*num_cells))
  ras <- terra::rast(nrows = num_cells, ncols = num_cells, vals = 1+runif(num_cells*num_cells))
  coords <- rasterToPoints(ras)[, c("x", "y")]

  # reference solution from gdistance
  # cost function is 1/src as gdistance works with conductance and not cost/resistance values
  tr <- transition(ras, function(x){1/x[1]}, 8, symm = FALSE)
  co <- geoCorrection(tr, "c", multpl = TRUE)
  gdist_m <- costDistance(tr*co, coords, coords)

  # create dist matrix from neighbours
  spaces <- stack_spaces(list("r1" = list(ras)), 1)
  h_mask <- get_habitable_mask(NULL, spaces, 1)
  local_distance <- get_local_distances(spaces, h_mask, function(src, h_src, dest, h_dest){src},8, NULL)
  dist_m <- get_distance_matrix(habitable_cells = 1:prod(dim(ras)),
                                num_cells = prod(dim(ras)),
                                dist_p = local_distance@p,
                                dist_i = local_distance@i,
                                dist_x = local_distance@x,
                                max_distance = Inf )

  expect_true(isTRUE(all.equal(unname(dist_m), gdist_m)))

  # check row/colnames
  expect_false(is.null(rownames(dist_m)))
  expect_false(is.null(colnames(dist_m)))
})
