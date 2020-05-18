# Copyright (c) 2020, ETH Zurich


test_that("run_simulation works", {
  skip_on_cran()
  s <- run_simulation(....elt())
  expect_true(!isnull(s))
})