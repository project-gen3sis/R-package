# Copyright (c) 2020, ETH Zurich

#---------------------------------------------------#
##########        Internal functions        #########
#---------------------------------------------------#


#' selects the habitable cells from the input
#' @details a leftover from when the habitable cells were intended to be changed in the fly
#'
#' @param what the input to be subset
#'
#' @return a boolean vector indicating which inputs are habitable
#' @noRd
select_habitable_hab <- function(what) {
  # what should be a string! NA`s are considered unsuitable
  # selected <- eval(parse(text = paste(what, condition, "& !is.na(", what, ")")))
  selected <- !is.na(what)
  return(selected)
}


#' calculate the richness of a list of species over a given landscape
#'
#' @param species_list a list of species to include in the richness calculations
#' @param landscape the landscape to calculate the richnness over
#'
#' @return a vector with the richness for every cell in the input landscape
#' @export
get_geo_richness <- function(species_list, landscape){
  cell_names <- rownames(landscape[["coordinates"]])
  presences <- sapply(species_list,
                      function(sp, cell_names){ cell_names %in% names(sp[["abundance"]])},
                      cell_names)
  richness <- rowSums(presences)
  names(richness) <- cell_names
  return(richness)
}


#' Calculate the individual average traits for all given species
#'
#' @param species_list a list
#'
#' @return a matrix filled with average traits vs all species
#' @noRd
get_eco_by_sp <- function(species_list) {
  averages <- t(sapply(species_list, function(sp) {colMeans(sp[["traits"]])} ))
  return(invisible(averages))
}



#' save_summary saves a numer of summry statistics at the end of a simulation run. Contents tbd
#'
#' @param config the current config object
#' @param data the current data oject
#' @param vars the current vars object
#' 
#' @importFrom utils packageVersion write.table
#'
#' @noRd
save_summary <- function(config, data, vars){
  # function that creates a summary file, mainly sgen3sis.RData at the end of the simulation
  path <- strsplit(config$directories$input, "/")[[1]]
  world <- path[length(path)]

  # sgen3sis <- list(parameters=list(gasm_version=config$gasm_version,
  #                               gasm_version=config$gasm_nickname))

  sgen3sis <- list("parameters" = list())

  sgen3sis$parameters <- c(sgen3sis$parameters, config)

  sgen3sis <- c(sgen3sis, list(
                         packageVersion=paste0( "gen3sis_", packageVersion("gen3sis")),
                         turnover=data$turnover,
                         phy = data$phy,
                         geo_richness = data$geo_richness,
                         # eco_by_cell=NULL#,    #Possible fix, add: eco_by_cell=data$eco_by_sp [Oskar]
                         #eco_by_sp_tf0=data$eco_by_sp_tf0,
                         eco_by_sp = data$eco_by_sp
                         #cpu_time=difftime(system_time_stop, system_time_start, units = "hours")[[1]]
                         ))
  
  #### START WIPOBSERVER ####
  #add to sgen3sis all summary objects created with observer_summary
  #### END WIPOBSERVER ####

  sgen3sis <- c(sgen3sis, flag=vars$flag)
  save(sgen3sis, file=file.path(config$directories$output,"sgen3sis.RData"))
}


#' saves the current phylogeny in nex format(?)
#'
#' @param phy the phylogeny up to this point
#' @param label a lable
#' @param output_location the file path and name to store the result
#'
#' @importFrom stringr str_replace str_extract
#' @importFrom utils write.table
#' @noRd
write_nex <- function(phy, label="sp", output_location){

  #    phy <- sgen3sis$phy  phy <- duplo  phy <- simples

  #check if we start with more than one ancestor, i.e. more than one root.
  rootphy <- phy$Speciation.Type=="ROOT"

  if (sum(rootphy)>1){
    phy$Ancestor[rootphy] <- 0
    phy$Speciation.Type <- as.character(phy$Speciation.Type)
    phy$Speciation.Type[rootphy] <- "COMB"
    addroot <- c("0","0",phy$Speciation.Time[1],0, "ROOT")
    names(addroot) <- colnames(phy)
    phy <- rbind(addroot, phy)
    phy$Ancestor <- as.integer(phy$Ancestor)
    phy$Descendent <- as.integer(phy$Descendent)
    phy[,c("Ancestor", "Descendent")] <- phy[,c("Ancestor", "Descendent")]+1
    phy$Ancestor <- as.integer(phy$Ancestor)
    phy$Descendent <- as.integer(phy$Descendent)
    phy$Speciation.Time <- as.integer(phy$Speciation.Time)
    phy$Extinction.Time <- as.numeric(phy$Extinction.Time)
    phy$Speciation.Type <- as.factor(phy$Speciation.Type)
  }

  #remove root
  phy_no_root <- phy[-1, , drop=F]

  if (nrow(phy_no_root)==0){
    #following TreeSimGM and TreeSim convertion
    if (phy[1,"Extinction.Time"]==0){
      String_final <- "1" # tree with only root
    } else {
      String_final <- "0" # tree with only root that got extinct
    }
  }else{

    Ancestral<-phy_no_root[,"Ancestor"]
    Derived<-phy_no_root[,"Descendent"]

    #Time calibrated tree
    root <- phy[phy$Speciation.Type=="ROOT", "Speciation.Time"]

    String<-paste0(label,"1:", root)
    Age<-c(root,phy_no_root[,"Speciation.Time"])
    #Age_new<-Age
    #unique(phy$Ancestor)

    Ancestral_age <- phy[unique(phy$Ancestor), "Speciation.Time"]
    names(Ancestral_age) <- phy[!duplicated(phy$Ancestor), "Ancestor"]

    Extinction <- c(0,phy_no_root[,"Extinction.Time"])


    for(i in 1:(max(phy_no_root[,"Descendent"])) ){
      String<-str_replace(String,

                          paste(label,Ancestral[i],":",Ancestral_age[toString(Ancestral[i])], sep="") ,

                          paste("(", label,Ancestral[i],":",Age[i+1],",",label,Derived[i],":",Age[i+1],"):",Ancestral_age[toString(Ancestral[i])]-Age[i+1], sep="")
      )

      Ancestral_age[toString(Ancestral[i])] <- Age[i+1]
        # print("-------")
        # print(String)
    }
    #adding ext times!
    extsps <- phy[phy$Extinction.Time>0,c("Descendent", "Extinction.Time")]
    if (nrow(extsps)>0){
      for (i in 1:nrow(extsps)){
        spi <- paste0(label,extsps$Descendent[i],":" )
        # print("-------")
        # print(spi)
        splited <- strsplit(String, spi )[[1]]
        oldnumb <- str_extract(splited[2], "\\-*\\d+\\.*\\d*")
        newnumb <- as.numeric(oldnumb)-extsps$Extinction.Time[i]
        #newnumb <- extsps$Extinction.Time[i]
        splited[2] <- sub(oldnumb, newnumb, splited[2] )
        String <- paste0(splited[1], spi, splited[2])

      }
    }


    String_final <- paste("tree = ", String, ";", sep="")
    String_final <- (paste("#NEXUS",
                           "begin trees;",
                           String_final,
                           "end;",
                           "[!Tree generated with the General Allele Simulation Model (GaSM), following convention of TreeSim and TreeSimGM r-packages]",
                           sep="\n"))
  } #end test if there is a tree

  write.table(String_final,

              file.path(output_location, "phy.nex"),

              quote=FALSE, row.names = F, col.names=F)

  #read phylo
  # t <- read.nexus(file.path(output_location, "phy.nex"))
  # plot(t)
}
