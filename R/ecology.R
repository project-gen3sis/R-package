# Copyright (c) 2020, ETH Zurich


#' apply_ecology allows the user to define the ecology of the species that takes place within each cell,
#' which define species survival and species abundance. 
#'
#' @details The arguments of the function allows to apply abiotic and biotic ecological rules to species in each 
#' cell. Based on those rules, the function updates the abundance of each species in each cell. If the abundance 
#' is null, the species is absent or extinct. Ecology can account for local environmental conditions, the abundance of
#' species, or their traits.
#'
#' @param abundance a named vector of abundances with one abundance value per species
#' @param traits a named matrix containg the species traits, one row per species
#' @param local_environment the environmental values for the given cell
#' @param config the config of the simulation
#'
#' @return an abundance vector with the new abundace values for every species.
#' An abundance value of 0 indicates species death, any other values survival.
#' @export
apply_ecology <- function(abundance, traits, local_environment, config) {
  stop("this function documents the user function interface only, do not use it.")
}



#' loop_ecology is the orchestrator for applying the ecology function to all cells.
#'
#' @details The ecology is applied on a per cell basis over all species occurring in each cell.
#' Therefore this function iterates over all cells and collects the abundance and traits of any species occuring there.
#' It then calls the user supplied apply_ecology function to this collection and apply ecology to each cell.
#'
#' @param config the general config of the simulation
#' @param data the general data list
#' @param vars the general variables list
#'
#' @return returns the standard val(config, data, vars) list
#' @noRd
loop_ecology <- function(config, data, vars) {
  # skip ecology function if config$exp$enable_eco_mec is FALSE
  if(config$gen3sis$general$verbose>=3){
    cat(paste("entering ecology module @ time", vars$ti, "\n"))
  }

  all_cells <- rownames(data$landscape$environment)
  all_species_presence <- do.call( cbind, lapply(data$all_species, FUN = function(sp) {all_cells %in% names(sp$abundance)}))
  rownames(all_species_presence) <- all_cells

  # take ids that have at least one species...
  #occupied_cells <- rownames(geo_sp_ti[rowSums(data$geo_sp_ti)>0, ,drop=F])
  occupied_cells <- rownames(all_species_presence)[rowSums(all_species_presence)>0]

  for (cell in occupied_cells) { # strat loop over ids with at least one species...
    local_environment = data$landscape[["environment"]][cell, , drop = F]

    coo_sp <- which(all_species_presence[cell,])
    #create coocuring species traits for idi #maybe use Reduce("rbind", eco[[coo_sp_ti_idi]][idi,], accumulate=T) ?
    traits <- matrix(nrow = length(coo_sp), ncol = length(config$gen3sis$general$trait_names))
    abundance <- numeric(length(coo_sp))

    #colnames(tr_sp) <- colnames(data$eco[[1]])[1:(length(config$exp$eco$trait_names)+1)]
    colnames(traits) <- config$gen3sis$general$trait_names

    i <- 1
    for (spi in coo_sp){ #loop over co-ocurring species @ idi
      # tr_sp_ti_idi[i,] <- data$eco[[spi]][idi,-(length(config$exp$eco$trait_names)+2)]
      traits[i,] <- data$all_species[[spi]][["traits"]][cell, config$gen3sis$general$trait_names]
      abundance[i] <- data$all_species[[spi]][["abundance"]][cell]
      i <- i+1
    }
    max_n_sp_idi <- config$gen3sis$general$max_number_of_coexisting_species
    if (length(coo_sp) > max_n_sp_idi) {
      stop(paste0("Maximum number of species per cell (i.e. max_n_sp_idi) reached. Specifically ",
                  length(coo_sp),"(>", max_n_sp_idi,") species @ t",vars$ti, " idi",cell ))
    }

    rownames(traits) <- coo_sp
    names(abundance) <- coo_sp

    #species <- traits[, c("abd", config$gen3sis$general$trait_names), drop = F]
  

    NEW_abd <- config$gen3sis$ecology$apply_ecology(abundance, traits, local_environment, config)

    # colnames(NEW_abd) <- coo_sp_ti_idi
    # TODO check if colnames(geo_sp_ti[,coo_sp_ti_idi]) should be used see line 622+-
    names(NEW_abd) <- coo_sp
    #abd_threshold <- config$exp$abundance_threshold
    shalldie <- NEW_abd == 0

    for (spi in coo_sp){
      data$all_species[[spi]][["abundance"]][cell] <- NEW_abd[[toString(spi)]]
    }

    die_sure <- as.integer(names(NEW_abd)[NEW_abd == 0])

    if (length(die_sure)>0) { #check if there are any species that should go extinct
      chars <- as.character(die_sure)
    } #end of check if any die_sure
  } #end loop over ids with at least one species...
  species_list <- list()
  for (species in data$all_species) {
    cells <- names(species[["abundance"]])[species[["abundance"]] != 0]
    updated_species <- limit_species_to_cells(species, cells)
    species_list <- append(species_list, list(updated_species))
  }
  data$all_species <- species_list

  if(config$gen3sis$general$verbose>=3){
    cat(paste("exiting ecology module @ time", vars$ti, "\n"))
  }
  return(list(config = config, data = data, vars = vars))
}


