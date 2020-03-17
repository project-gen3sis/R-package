# Copyright (c) 2020, ETH Zurich


percentage_inhabited <- function(species_list, landscape) {
  richness <- get_geo_richness(species_list, landscape)
  inhabited <- sum(which(richness != 0))
  return(inhabited / length(richness))
}


percentage_species_alive <- function(species_list){
  alive <- sum( sapply(species_list, function(sp){ifelse(length(sp[["abundance"]]), 1, 0) }))
  return(alive / length(species_list))
}


min_max_traits <- function(species){
  min_max <- matrix(data = NaN,
                    nrow = 2,
                    ncol = ncol(species[["traits"]]),
                    dimnames = list(c("min", "max"), colnames(species[["traits"]])))
  if(!length(species[["abundance"]])) {
    min_max["min", ] <- Inf
    min_max["max", ] <- -Inf
  } else {
    min_max["min",] <- apply(species[["traits"]], 2, min)
    min_max["max",] <- apply(species[["traits"]], 2, max)
  }
  return(min_max)
}


trait_ranges <- function(species_list) {
  all_ranges <- lapply(species_list, min_max_traits)
  ranges <- do.call(pmin, all_ranges)
  ranges["max", ] <- do.call(pmax, all_ranges)["max", ]
  return(ranges)
}


habitat_extents <- function(species_list, exclude_extinct = T, plot_hist = F) {
  extents <- sapply(species_list, function(sp){length(sp[["abundance"]])})
  names(extents) <- sapply(species_list, function(sp){as.character(sp[["id"]])})
  if(exclude_extinct) {
    extents <- extents[extents != 0]
  }
  return(extents)
}
