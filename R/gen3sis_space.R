#' create empty gen3sis_spaces
#'
#' @param env named list of environmental variables in a with X,Y, and time-steps from
#' the most recent of future time-step
#' #' @param type string with type of gen3sis spaces. Accepted values are \code{check_spaces()$type}
#' @param duration list containing information on temporal dimension list(from, to, by, unit)
#' *from* is the oldest time-step; negative number if starting in the past, zero if starting in the present
#' *to* CAN ONLY BE smaller than *from*, since *to* is the latest time
#' *by* is the time interval increment. Note that this is constant and can only be positive;
#' *unit* is the time unit used. Accepted units are \code{check_spaces()$duration}
#' e.g. list(-20, 0, 1, "Ma") has a landscape that covers the last 20 Ma until the present, every 1 Ma.
#'      list(-800, 300, 10, "mil") has a landscape that covers the last 800 kyra or mil for millions of years
#'      and goes until the future 300 kya at every 100 mil years.
#' @param area list containing information on the 2D spacial dimension: list(total_area, n_sites, unit)
#' *total_area* the covered area by the points
#' Accepted units are square meter (m2) and square kilometer (km2) \code{check_spaces()$area}
#' @param crs Coordinate Reference Systems, as string and PROJ.4 format. Default is,
#' WGS 84 -- WGS84 - World Geodetic System 1984 crs="+proj=longlat +datum=WGS84 +no_defs"

#' @param cost_function list of cost_function(s) used to calculate the cost distances between sites.
#' Depends on type and other methods used to calculate the cost distances.
#' @param geo_dynamic boolean for if sites change location or disappear over time
#' ,default is NULL and deduces from NA over time. If false, only one cost_distance is calculated and used
#' i.e. whencost_distances are the same, i.e. no geodynamic changes in the space
#' @param author string with the name of the author. Default is NULL which gives the system user name obtained from \code{Sys.info()["user"]}
#' This is far from ideal, but it is better than nothing. Please fill this up and even consider leaving a contact information
#' @param source string with the source of the data, ideally should contain a publication with DOI and a valid URL
#' Default is list(env="missing", methods="missing")
#' @param description list with "env" and "methods" containing information on the environmental data,
#' methods used and other relevant information such as source to raw data. Default is list("missing"
#' The "env" should describe the environmental data used, including it's units
#'
#' @return an informal and empty gen3sis_spaces object
#' @export
#'
#' @examples print("TODO")
create_spaces <- function(env=list(NA),
                         type="raster",
                         duration=list(from=NA, to=NA, by=NA, unit="Ma"),
                         area=list(total_area=NA, n_sites=NA, unit="km2"),
                         crs="+proj=longlat +datum=WGS84 +no_defs",
                         cost_function=list(NA),
                         geo_dynamic=NULL,
                         author=NULL,
                         source="missing",
                         description=list(envar="missing", methods="missing")
                         ){
  spaces <- list()
  spaces$"env" <- env
  spaces$meta$"type" <- type
  spaces$meta$"duration" <- duration
  spaces$meta$"area" <- area
  spaces$meta$"crs" <- crs
  # attributes(cost_function) <- NULL
  spaces$meta$"cost_function" <- cost_function
  if(is.null(geo_dynamic)&!is.null(dim(env[[1]]))){
    geo_dynamic <- any(unlist(lapply(env, function(x){
      #  if all lines have either only NA or values though time, then it is not geodynamic
      r <- any(apply(is.na(x[,-c(1,2)]), 1, all))
      return(r)
    })))
  }
  spaces$meta$"geo_dynamic" <- geo_dynamic
  if (is.null(author)){
    author <- Sys.info()["user"]
    # remove attributes
    attributes(author) <- NULL
  }
  spaces$meta$"author" <- author
  spaces$meta$"source" <- source
  spaces$meta$"description" <- description
  # spaces <- structure(spaces, class=c("gen3sis_spaces", "list"))
  return(invisible(spaces))
}



#' Check gen3sis_spaces
#'
#' @param spaces either a gen3sis_spaces object to be checked, or NULL
#'
#' @return If a spaces object is provided, either a stop with printed error
#' report or a pass statement.
#' If spaces=NULL, this function returns lists of accepted values categories
#' according to \code{check_spaces()}
#' @export
#'
#' @examples print("TODO")
check_spaces <- function(spaces=NULL){

  accepted <- list()
  accepted[["type"]] <- c("raster", "points", "h3")
  dur_units <- c("day", "wk", "mon", "yr", "dec", "cen", "mil", "Ma")
  accepted[["duration_unit"]] <- dur_units[dur_units%in%measurements::conv_unit_options$duration]
  area_units <- c("m2", "km2", "ha")
  accepted[["area_unit"]]<- area_units[area_units%in%measurements::conv_unit_options$area]

  if (is.null(spaces)){
    return(accepted)
  }

  error_report <- NULL
  sp_ref <- create_spaces()


  error_report <- check_names(reference="env", datags=spaces, error_report)
  for (n_i in names(sp_ref$meta)){
    # n_i <- names(sp_ref$meta)[5]
    error_report <- check_names(reference=n_i, datags=spaces$meta, error_report)
    n_sub_e <- names(sp_ref$meta[[n_i]])
    if (length(n_sub_e)>1){
      for (s_i in n_sub_e){
        # s_i <- n_sub_e[1]
        error_report <- check_names(reference=s_i, datags=spaces$meta[[n_i]][s_i], error_report)
      }
    }
  }
  # stop in case of errors reported
  if (!is.null(error_report)){
    stop(error_report)
  }
  return("gen3sis_spaces [OK]")
}

#' Check names of spaces
#'
#' @param reference string with the variable name to be tested, e.g. type, env
#' @param datags list of which \code{names(datags) is contrastet for reference}
#' @param error_report an error report that increases in case mismatches are 
#' found for each sub-list.
#' default is NULL
#'
#' @return an error report with the mismatches found
#' @export
check_names <- function(reference, datags, error_report=NULL){
  target=names(datags)
  if (!reference%in%target) { # if variables is missing
    error_report <- paste(
      error_report,
      (paste("The following spaces data is missing:", reference)),
      "\n")
  } # end if var name is missing

  if (reference=="env"){ # check env
    if (!is.list(datags[[reference]])){ # if env is not a list
      error_report <- paste(
        error_report,
        (paste0("! >", reference, "< has to be a list of environmental variable(s)")),
        "\n")
    }
    # check if NAs are the same
    mask_NAs <- lapply(datags$env, function(x){
      is.na(x[,!colnames(x)%in%c("x","y"), drop=FALSE])
    })
    for (env_i in names(mask_NAs)){
      # env_i <- names(mask_NAs)[2]
      if (!identical(mask_NAs[[1]], mask_NAs[[env_i]])){

        error_report <- paste(
          error_report,
          (paste0("! >please check your env data. NAs must match for all env's: \n
         i.e. environmental variables stored in env as a list.")),
          "\n")
      }
    } # end NA comparison loop
  } else if (any(is.na(datags[[reference]]))){ # if there is NA
    error_report <- paste(
      error_report,
      (paste0("! >", reference, "< can not be NA! please specify")),
      "\n")
  } # end if NA# end if env

  return(error_report)
}


#' Prepare input and output directories
#'
#' @param dir_input "path to dir_input"
#' @param dir_output "path to dir_output"
#' @return no value is returned
#' @noRd
#' @examples print("TODO")
prepare_dirs <- function(dir_input, dir_output){
  if(!dir.exists(dir_input)){
    stop(paste("Input directory does not exist:", dir_input))
  }
  cat(paste0("Input directory found: \n [", dir_input, "] \n"))
  dir.create(dir_output, showWarnings = FALSE)
  cat(paste0("Output directory: \n [", dir_output, "] \n"))
}

