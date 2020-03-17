# Copyright (c) 2020, ETH Zurich

# to be merged with compress divergence
compress.gen.dist <- function(gen_dist) {
  #compresses genetic distance from all locations to all locations
  #into clusters that are similar. Therefore creating a gen distance
  #of clusters against clusters (i.e. $gen_dist_ent) and
  #a list of clusters named by their idi / geografical location (i.e. $clusters)
  clusters <- getEntities(gen_dist)
  if(length(clusters)==0){
    return(list(gen_dist_ent = matrix(0,0,0,dimnames=list(NULL,NULL)), clusters = clusters))
  }
  num_clusters <- max(clusters)

  gen_dist_ent <- matrix(NA, nrow = num_clusters, ncol = num_clusters)
  index = !duplicated(clusters)
  gen_dist_ent <- gen_dist[index, index, drop = F]
  rownames(gen_dist_ent) <- c(1:num_clusters)
  colnames(gen_dist_ent) <- c(1:num_clusters)
  names(clusters) <- rownames(gen_dist)

  return(list("gen_dist_ent" = gen_dist_ent, "clusters" = clusters))
}


#' compress_divergence conmpresses the divergence matrix into an index and smaller
#' matrix by storing only unique rows/columns
#'
#' @param divergence_matrix the divergence matrix to compress
#'
#' @return the compressed divergence matrix
#' @noRd
compress_divergence <- function(divergence_matrix){
  gen_dist_old <- compress.gen.dist(divergence_matrix)
  gen_dist_new <- list("clusters" = gen_dist_old[["clusters"]],
                       "cluster_divergence" = gen_dist_old[["gen_dist_ent"]])

  return(gen_dist_new)
}



#' decompress_divergence rebuilds the full divergence matrix from it's compressed form
#'
#' @param divergence the compressed form of the divergence matrix
#'
#' @return a full divergence matrix
#' @noRd
decompress_divergence <- function(divergence) {
  #expand compressed gen_dist_ent into full cell x cell gen dist
  #index selection replicates
  if(length(divergence$clusters)==0){
    return(matrix(0,0,0,dimnames=list(NULL,NULL)))
  }
  gen_dist_full <- divergence$cluster_divergence[divergence$clusters, divergence$clusters, drop = F]
  ne <- names(divergence$clusters)
  dimnames(gen_dist_full) <- list(ne,ne)
  return(gen_dist_full)
}


#' limits the given compressed divergence to a given set of cells
#'
#' @param divergence a compressed divergence matrix
#' @param cells a list of cells to limit the divergence to
#'
#' @return a reduced and compressed divergence matrix
#' @noRd
limit_divergence_to_cells <- function(divergence, cells) {
  new_clusters <- divergence[["clusters"]][cells]
  unique_clusters <- unique(new_clusters)
  new_gen_dist_clusters <- divergence[["cluster_divergence"]][unique_clusters, unique_clusters, drop = F]
  if(length(unique_clusters)) {
    new_range <- 1:length(unique_clusters)
    dimnames(new_gen_dist_clusters) <- list(new_range, new_range)
    for( i in 1:length(new_clusters)) {
      new_clusters[i] <- new_range[unique_clusters == new_clusters[i]]
    }
  }

  return(invisible(list("clusters" = new_clusters, "cluster_divergence" = new_gen_dist_clusters)))
}


#' consolidate divergence checks and possibly merges identical divergence clusters that may arise when removing cells from a species
#'
#' @param divergence a compressed divergence matrix
#'
#' @return returns a consolidated and compressed divergence matrix
#' @noRd
consolidate_divergence <- function(divergence) {
  if(length(divergence[["clusters"]])==0){
    return( invisible( list(cluster_divergence = matrix(0,0,0,dimnames=list(NULL,NULL)),
                            clusters = integer()) ) )
  }

  new_compressed <- compress_divergence(divergence[["cluster_divergence"]])
  cells <- as.character(divergence[["clusters"]])
  new_clusters <- new_compressed[["clusters"]][cells]
  names(new_clusters) <- names(divergence[["clusters"]])

  return(invisible(list("clusters" = new_clusters,
                        "cluster_divergence" = new_compressed[["cluster_divergence"]])))
}

