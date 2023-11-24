# Copyright (c) 2020, ETH Zurich

#' Checks if the necessary directories exist, and otherwise creates them
#' 
#' @details This function will be called by the simulation, but is made available if the directories should be created
#' manually beforehand, for example to redirect the stdout to a file in the output directory.
#' @param config_file path to the config file, if NA the default config will be used
#' @param input_directory path to input directory, if NA it will be derived from the config file path
#' @param output_directory path to output directory, if NA it will be derived from the config file path
#' @return returns a named list with the paths for the input and output directories
#'
#' @importFrom tools file_path_sans_ext
#' @example inst/examples/prepare_directories_help.R
#' @export
#' 
prepare_directories <- function(config_file = NA,
                                input_directory = NA,
                                output_directory = NA) {
  #no default config, config file must be given
  if(is.na(config_file)[1]) {
    stop("no config file provided!")
  } else if (is(config_file,"gen3sis_config")){
    print("config found: using config object")
  } else if(!file.exists(config_file)){
    stop("config file does not exist!")
  } else {
    print(paste("config found:", config_file))
  }

  if(is.na(input_directory)) {
    path <- strsplit(config_file, "/")[[1]]
    path <- paste(path[1:(length(path)-2)], collapse="/")
    input_dir <- sub("[cC]onfig", "input", path)
  } else {
    input_dir <- input_directory
  }
  if(!dir.exists(input_dir)){
    stop(paste("input directory does not exist!:", input_dir))
  }
  print(paste("landscape found:", input_directory))

  if(is.na(output_directory)) {
    if (is(config_file, "gen3sis_config")){
      path <- strsplit(input_dir, "/")[[1]]
      path <- paste(path[1:(length(path)-1)], collapse="/")
      output_dir <- sub("[lL]andscape", "output", path) 
    } else if (is(config_file, "character")) {
      path <- strsplit(config_file, "/")[[1]]
      path <- paste(path[1:(length(path)-1)], collapse="/")
      output_dir <- sub("[cC]onfig", "output", path)
    }
  } else {
    output_dir <- output_directory
  }

  #set and create directories
  #input data
  dir <- list()
  dir$input <- input_dir

  #output folders
  if(is.na(config_file)[1]|is(config_file, "gen3sis_config")) {
    config_name <- file.path("default_config", paste0(format(Sys.time(), "%Y%m%d%H%m"), "-", formatC(sample(1:9999,1), digits=4, flag="0")))
  } else {
    config_name <- tools::file_path_sans_ext(basename(config_file))
  }
  dir$output <- file.path(output_dir, config_name)
  dir.create(dir$output, recursive=TRUE, showWarnings = FALSE)
  print(paste("output directory is:", dir$output))

  #dir$output_species <- file.path(dir$output, "species")
  #dir.create(dir$output_species, recursive=TRUE, showWarnings = FALSE)
  #dir$output_landscapes <- file.path(dir$output, "landscapes")
  #dir.create(dir$output_landscapes, recursive=TRUE, showWarnings = FALSE)
  dir$output_plots <- file.path(dir$output, "plots")
  dir.create(dir$output_plots, recursive=TRUE, showWarnings = FALSE)
  #dir$output_val <- file.path(dir$output, "val")
  #dir.create(dir$output_val, recursive=TRUE, showWarnings = FALSE)

  return(dir)
}




#' Creates either an empty configuration or a pre-filled configuration object from a config file
#'
#' @param config_file the path to a valid configuration file. if NA it creates an empty config
#' @return list of configuration elements, similar generated from reading a config_file.R. The internal elements 
#' of this list are: "general", "initialization", "dispersal", "speciation", "mutation" and "ecology"
#' @example inst/examples/create_input_config_help.R
#' @export
create_input_config <- function(config_file = NA) {
  new_config <- create_empty_config()
  if(is.na(config_file)) {
    # return empty config
    return(invisible(new_config))
  } else if( !file.exists(config_file)){
    # config file does not exist, abort
    stop(paste("config file:", config_file, "does not exist") )
  } else {
    # populate config
    config <- populate_config(new_config, config_file)
    return(invisible(config))
  }
}


internal_categories <- c("general",
                         "initialization",
                         "dispersal",
                         "speciation",
                         "mutation",
                         "ecology"
                         )


#' Initializes a config with the values from a provided config file
#'
#' @param config config object to fill
#' @param config_file config file to retrieve settings from
#'
#' @return empty list as in a config object
#' @noRd
populate_config <- function(config, config_file) {
  user_config_env <- new.env()
  source(config_file, chdir=TRUE, local=user_config_env)
  for ( category in internal_categories) {
    config[["gen3sis"]][[category]] <- populate_settings_list(config[["gen3sis"]][[category]], user_config_env)
  }
  user_settings <- ls(user_config_env)
  presence <- rep(FALSE, length(user_settings))
  for (category in internal_categories){
    presence <- presence | (user_settings %in% names(config[["gen3sis"]][[category]]))
  }
  if(any(!presence)){
    for( i in user_settings[which(!presence)] ) {
      config[["user"]][[i]] <- user_config_env[[i]]
    }
  }
  return(invisible(config))
}

#' Helper function taking on a set of user options for the given category
#'
#' @details This function is a helper function to take on a set of user options for the given category.
#' @param config_list a named list of settings to look for
#' @param user_env an environment containing all the user provided config options
#'
#' @return returns the config list with either the user provided options or the original values intact
#' @noRd
populate_settings_list <- function(config_list, user_env) {
  general_settings <- names(config_list)
  user_settings <- ls(user_env)
  for( setting in user_settings) {
    if ( setting %in% general_settings) {
      config_list[[setting]] <- user_env[[setting]]
    }
  }
  return(invisible(config_list))
}


#' Verifies if all required config fields are provided
#'
#' @param config a config object
#' @return Returns TRUE for a valid config, FALSE otherwise, in which case a list of
#' missing parameters will be printed out as well
#' @seealso \code{\link{create_input_config}}    \code{\link{write_config_skeleton}}   
#' @example inst/examples/verify_config_help.R
#' @export
verify_config <- function(config) {
  missing_settings <- list()
  unset_settings <- list()
  reference <- create_empty_config()
  for(category in internal_categories) {
    presence <- names(reference[["gen3sis"]][[category]]) %in%  names(config[["gen3sis"]][[category]])
    if( !all( presence ) ) {
      missing_settings <- append(missing_settings, names(reference[["gen3sis"]][[category]])[presence])
    }
  }
  if(length(missing_settings)){
    print(paste("missing settings in the configuration:", paste(missing_settings, collapse = ", ")))
    return(FALSE)
  }
  for(category in internal_categories) {
    settings <- names(config[["gen3sis"]][[category]])
    null_settings <- sapply(config[["gen3sis"]][[category]], is.null)
    if( any( as.logical(null_settings) ) ) {
      unset_settings <- append(unset_settings, settings[null_settings])
    }
  }
  if(length(unset_settings)) {
    print(paste("these settings must be set in the configuration:", paste(unset_settings, collapse = ", ")))
    return(FALSE)
  }
  return(TRUE)
}


#' Creates an empty config object
#'
#' @details All config fields are created and set to NA if they can be omitted by the user
#' or set to NULL if they must be provided before starting a simulation.
#' @return returns an empty config structure
#' @noRd
create_empty_config <- function(){
  config <- list()
  config[["gen3sis"]] <- list("general" = list( "random_seed" = NA,
                                              "start_time" = NA,
                                              "end_time" = NA,
                                              "max_number_of_species" = NA,
                                              "max_number_of_coexisting_species" = NA,
                                              "end_of_timestep_observer" = function(...){},
                                              "trait_names" = list(),
                                              "environmental_ranges" = list(),
                                              "verbose" = FALSE
                                              ),
                            "initialization" = list( "initial_abundance" = NULL,
                                                     "create_ancestor_species" = NULL
                                                     ),
                            "dispersal" = list( "max_dispersal" = Inf,
                                                "get_dispersal_values" = NULL
                                                 ),
                            "speciation" = list( "divergence_threshold" = NULL,
                                                 "get_divergence_factor" = NULL
                                                 ),
                            "mutation" = list( "apply_evolution" = NULL
                                               ),
                            "ecology" = list("apply_ecology" = NULL
                                             )
                            )
  config[["user"]] <- list()
  config[["directories"]] <- list()
  class(config) <- "gen3sis_config"
  return(config)
}


#' Completes the settings of a given config
#'
#' @details This function conducts the final checks and settings before the simulations runs.
#' currently these include setting the random seed and adding a dispersal trait if not done by the user
#' @param config the current config for this simulation run
#' @noRd
complete_config <- function(config) {
  # random seed
  seed <- config[["gen3sis"]][["general"]][["random_seed"]]
  if( !is.null(seed) && !is.na(seed) ) {
    set.seed(seed)
  }

  # dispersal trait
  config[["gen3sis"]][["general"]][["trait_names"]] <- unique(c(config[["gen3sis"]][["general"]][["trait_names"]], "dispersal"))

  return(invisible(config))
}


#' Writes out a config skeleton
#'
#' @details This function writes out a config skeleton, that is, an empty config file to be edited by the user.
#' @param file_path file path to write the file into
#' @param overwrite overwrite existing file defaults to FALSE
#'
#' @return returns a boolean indicating success or failure
#' @example inst/examples/write_config_skeleton_help.R
#' @export
write_config_skeleton <- function(file_path = "./config_skeleton.R", overwrite = FALSE) {
  if( file.exists(file_path) & !overwrite) {
    warning(file_path, "exists, file not written")
    return(FALSE)
  } else {
    new_file <- file(file_path, open = "w")
    writeLines(skeleton_config(), new_file)
    close(new_file)
    return(TRUE)
  }
}
