# Copyright (c) 2020, ETH Zurich

#' Allows the user to define the rate at which geographic clusters accumulate differentiation
#' with each other.
#'
#' @details This function determines the increase in divergence between separated clusters of a species. This function
#' should return either (i) a single value if there is an homogeneous divergence, or (ii) a matrix indicating the divergence that
#' should be accumulated between specific pairwise geographic clusters.
#' 
#' The function can either return a single value or a full cluster by cluster matrix. If only one value is returned it will be used 
#' to increment divergence between any given distinct cluster pairs. If a matrix is returned it has to be in the dimension of
#' cluster x cluster, in which case the divergence values will be increased according to the cluster membership of any cell pairs.
#'
#' For every time step, the divergence between geographic clusters can increase by a defined number. The divergence values can be 
#' scaled optionally using the species or space information. For instance, the divergence between clusters could be higher under
#' warmer temperature, or difference in ecological traits could promote faster divergence between clusters.
#' 
#' Oppositely, for every time-step, if cluster are merged their divergence is reduced by one (1). 
#'
#' @param species the species of the current time step
#' @param cluster_indices an index vector indicating the cluster every occupied site is part of
#' @param space the space of the current time step
#' @param config the config of the simulation
#'
#' @return a single value or a matrix of divergences between all clusters occurring in clusters_indices
#' @export
get_divergence_factor <- function(species, cluster_indices, space, config){
  stop("this function documents the user function interface only, do not use it!")
}

#' Orchestrates the speciation of any species alive in the simulation
#'
#' @param config the current config object
#' @param data the current data object
#' @param vars the current vars object
#'
#' @return an expanded species list including all newly created species
#' @noRd
loop_speciation <- function(config, data, vars) {
  if(config$gen3sis$general$verbose>=3){
    cat(paste("entering speciation module \n"))
  }
  for(spi in 1:vars$n_sp){ # loop over existing species
    # get compressed genetic distance for spi
    species <- data$all_species[[spi]]

    if(!length(species[["abundance"]])) {
      next()
    }
    # define occupied cells by species
    species_presence <- names(species[["abundance"]])

    ##calling RCPP function to define physical clusters
    if ( length(species_presence)==1 ){#check if only one cell is occupied
      clu_geo_spi_ti <- 1
    }else{
      distances <- config$gen3sis$dispersal$get_dispersal_values(length(species_presence), species, data$space, config)

      permutation <- sample(1:length(species_presence), length(species_presence))
      clu_geo_spi_ti <- Tdbscan_variable(data$distance_matrix[species_presence[permutation],species_presence[permutation],
                                                            drop=FALSE], distances, 1)
      clu_geo_spi_ti <- clu_geo_spi_ti[order(permutation)]
    }

    gen_dist_spi <- decompress_divergence(species[["divergence"]])
    # update genetic distances
    ifactor <- config$gen3sis$speciation$get_divergence_factor(species, clu_geo_spi_ti, data[["space"]], config)
    gen_dist_spi <- update_divergence(gen_dist_spi, clu_geo_spi_ti, ifactor = ifactor )

    gen_dist_spi <- compress_divergence(gen_dist_spi)

    species[["divergence"]] <- gen_dist_spi

    clu_gen_spi_ti_c <- Tdbscan(gen_dist_spi$compressed_matrix, config$gen3sis$speciation$divergence_threshold, 1)
    clu_gen_spi_ti <- clu_gen_spi_ti_c[gen_dist_spi$index]
    n_new_sp <- max(clu_gen_spi_ti)-1

    # update count of new species at this time-step
    vars$n_new_sp_ti <- vars$n_new_sp_ti + n_new_sp

    if ( n_new_sp > 0 ){ #if a speciation occured
      if(config$gen3sis$general$verbose>=3){
        cat(paste("[!]   Wellcome Strange  Thing   [!] \n"))
        cat(paste(n_new_sp,"speciation event(s) happened \n"))
      }

      #attributing the final names of the species in a vector
      desc_unique <- unique(clu_gen_spi_ti)[-1]+vars$n_sp+vars$n_sp_added_ti-1

      #udpate phy
      data$phy <- rbind(data$phy,data.frame("Ancestor"=rep(spi,n_new_sp),
                                            "Descendent"=desc_unique,
                                            "Speciation.Time"=rep(vars$ti,n_new_sp),
                                            "Extinction.Time" = rep(vars$ti, n_new_sp),
                                            "Speciation.Type"=rep("Genetic", n_new_sp)))

      #required for proper initialiaztion of new species
      full_gen_dist <- gen_dist_spi

      gen_dist_spi$index <- gen_dist_spi$index[clu_gen_spi_ti == 1]
      ue <- unique(gen_dist_spi$index)
      gen_dist_spi$compressed_matrix <- gen_dist_spi$compressed_matrix[ue,ue, drop=FALSE]
      #update names
      if (length(ue)>0) {
        fullrange <- 1:length(ue)
        dimnames(gen_dist_spi$compressed_matrix) <- list(fullrange, fullrange)
        for (i in 1:length(gen_dist_spi$index)){
          gen_dist_spi$index[i] <- fullrange[ue==gen_dist_spi$index[i]]
        }
      }

      for (desci in desc_unique) {
        #get the value of current gen_clu
        tep_clu_gen_desci_index <- which(desc_unique==desci)+1
        new_species <- create_species_from_existing(species,
                                                    desci,
                                                    names(species[["abundance"]][clu_gen_spi_ti == tep_clu_gen_desci_index]),
                                                    config)
        data$all_species <- append(data$all_species, list(new_species))

      } # end loop over descendents

      species <- limit_species_to_cells(species = species,
                                        cells = names(species[["abundance"]][clu_gen_spi_ti == 1]))

      #update number of species added
      vars$n_sp_added_ti <- vars$n_sp_added_ti+n_new_sp

      # taking the physical clusters, but removing the already speciated species from spi (i.e. the "mother species")
      clu_geo_spi_ti <- clu_geo_spi_ti[clu_gen_spi_ti==1]

    }# end of creating new species

    data$all_species[[spi]] <- species

  } # end loop over existing species
  if(config$gen3sis$general$verbose>=3){
    cat(paste("exiting speciation module \n"))
  }
  if(config$gen3sis$general$verbose>=3 && vars$n_sp_added_ti > 0){
    cat(paste(vars$n_sp_added_ti,"new species created \n"))
  }
  return(list(config = config, data = data, vars = vars))
}


#' Updates a given divergence matrix
#'
#' @param gen_dist_spi a divergence matrix
#' @param clu_geo_spi_ti a cluster index
#' @param ifactor the divergence factor by which the clusters distances are to be increased
#'
#' @return an updated divergence matrix
#' @noRd
update_divergence <- function(divergence, cluster_indices, ifactor) {
  #udpate genetic distance
  clusters <- unique(cluster_indices)
  if( length(ifactor) == 1 ) {
    # scalar ifactor
    divergence <- divergence + ifactor
    dfactor <- 1+ifactor
  } else {
    # matrix ifactor
    divergence <- divergence + ifactor[cluster_indices, cluster_indices]
    dfactor <- 1
  }
  for ( i in clusters ){
    #in case they belong to same clusters, subtract -2 (for the default case), to that final diference is -1 given previous addition!
    divergence[cluster_indices == i, cluster_indices == i] <-
      divergence[cluster_indices == i, cluster_indices== i] - dfactor
  }
  #setting -1 to zero. Genetic differences can not be negative
  divergence[divergence < 0] <- 0
  ##end updating genetic distance##
  return(divergence)
}


#' Updates the total number of species
#'
#' @param config the current config object
#' @param data the current data list
#' @param vars the current vars list
#'
#' @return the updated vals list
#' @noRd
update1.n_sp.all_geo_sp_ti <- function(config, data, vars) {
  # update number of species
  vars$n_sp <- vars$n_sp+vars$n_sp_added_ti
  return(list(config = config, data = data, vars = vars))
}


#' Updates the total number of species alive
#'
#' @param config the current config object
#' @param data the current data list
#' @param vars the current vars list
#'
#' @return the updated vals list
#' @noRd
update2.n_sp_alive.geo_sp_ti <- function(config, data, vars) {
  # update number of species alive
  vars$n_sp_alive <- sum( sapply(data$all_species, function(sp){ifelse(length(sp[["abundance"]]), 1, 0) }))
  return(list(config = config, data = data, vars = vars))
}
