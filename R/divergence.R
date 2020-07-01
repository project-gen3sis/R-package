# Copyright (c) 2020, ETH Zurich

#' The function compress_divergence compresses the divergence matrix into an index and smaller
#' matrix by storing only unique rows/columns
#'
#' @param divergence_matrix the divergence matrix to compress
#'
#' @return the compressed divergence matrix
#' @noRd
compress_divergence <- function(divergence_matrix){
  #compresses genetic distance from all locations to all locations
  #into index that are similar. Therefore creating a gen distance
  #of index against index (i.e. $compressed_matrix) and
  #a list of index named by their idi / geografical location (i.e. $index)
  index <- getEntities(divergence_matrix)
  if(length(index)==0){
    return(list(compressed_matrix = matrix(0,0,0,dimnames=list(NULL,NULL)), index = index))
  }
  num_indices <- max(index)

  compressed_matrix <- matrix(NA, nrow = num_indices, ncol = num_indices)
  unique_index = !duplicated(index)
  compressed_matrix <- divergence_matrix[unique_index, unique_index, drop = F]
  rownames(compressed_matrix) <- c(1:num_indices)
  colnames(compressed_matrix) <- c(1:num_indices)
  names(index) <- rownames(divergence_matrix)

  return(list("index" = index, "compressed_matrix" = compressed_matrix))
}



#' The function decompress_divergence rebuilds the full divergence matrix from its compressed form
#'
#' @param divergence the compressed form of the divergence matrix
#'
#' @return a full divergence matrix
#' @noRd
decompress_divergence <- function(divergence) {
  #expand compressed gen_dist_ent into full cell x cell gen dist
  #index selection replicates
  if(length(divergence$index)==0){
    return(matrix(0,0,0,dimnames=list(NULL,NULL)))
  }
  divergence_full <- divergence$compressed_matrix[divergence$index, divergence$index, drop = F]
  ne <- names(divergence$index)
  dimnames(divergence_full) <- list(ne,ne)
  return(divergence_full)
}


#' The function limit_divergence_to_cells limits the given compressed divergence to a given set of cells
#'
#' @param divergence a compressed divergence matrix
#' @param cells a list of cells to limit the divergence to
#'
#' @return a reduced and compressed divergence matrix
#' @noRd
limit_divergence_to_cells <- function(divergence, cells) {
  new_index <- divergence[["index"]][cells]
  unique_indices <- unique(new_index)
  new_compressed_matrix <- divergence[["compressed_matrix"]][unique_indices, unique_indices, drop = F]
  if(length(unique_indices)) {
    new_range <- 1:length(unique_indices)
    dimnames(new_compressed_matrix) <- list(new_range, new_range)
    for( i in 1:length(new_index)) {
      new_index[i] <- new_range[unique_indices == new_index[i]]
    }
  }

  return(invisible(list("index" = new_index, "compressed_matrix" = new_compressed_matrix)))
}


#' The function consolidate_divergence checks and possibly merges identical divergence clusters that may arise when removing cells from a species
#'
#' @param divergence a compressed divergence matrix
#'
#' @return returns a consolidated and compressed divergence matrix
#' @noRd
consolidate_divergence <- function(divergence) {
  if(length(divergence[["index"]])==0){
    return( invisible( list(index = integer(),
                            compressed_matrix = matrix(0,0,0,dimnames=list(NULL,NULL)) ) )
            )
  }

  new_compressed <- compress_divergence(divergence[["compressed_matrix"]])
  cells <- as.character(divergence[["index"]])
  new_index <- new_compressed[["index"]][cells]
  names(new_index) <- names(divergence[["index"]])

  return(invisible(list("index" = new_index,
                        "compressed_matrix" = new_compressed[["compressed_matrix"]])))
}

