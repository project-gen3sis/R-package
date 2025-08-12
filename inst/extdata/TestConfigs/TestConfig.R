#------------------------#
#### General settings ####
#------------------------#
comb_vector <- list(seed=28015, dispersal=240, t_evo=0.03) # 

random_seed = comb_vector$seed #28015
start_time = NA
end_time = NA
max_number_of_species =20000
max_number_of_coexisting_species =20000
initial_abundance =  10

# ecological local equilibria variable J*
get_J <- function(a_ff, a_fh, K_f){
  J <- sum((a_ff*K_f)/(a_ff-a_fh), na.rm=T)/(1+sum((a_fh/(a_ff-a_fh)), na.rm=T)) # new
  return(J)
}

# ecological environmental function
fg <- function(x,a,b,c,ns=1){
  a <- a/c
  v <- a*exp(-((x-b)^2/(2*c^2)))
  return(v)
}

apply_trs_tradeoff <- function(traits){
  #balance out traits in case they are above the traitoff surface
  cd <- traits[,c("dispersal", "competition")]
  # dispersal values on trade-off linear function. e.g. lines(x=c(0,1), y=c(0.9,1))
  dsurf <- -10*cd[,"competition"]+10
  csurf <- (10-cd[,"dispersal"])/10
  Dy <- cd[,"dispersal"]-dsurf
  # Dx <- cd[,"competition"]-csurf
  # get traits above and to the right of this linear function
  above_surface_mask <- Dy>0
  if (any(above_surface_mask)){ # then push them back to surface
    # print("traits were above the trait tradeoff surface... bringing them back to traide-off surface")
    # sample if either surface should be corresponding to d or c. 
    c_or_d <- sample(list(c(T,F), c(F,T)), size=sum(above_surface_mask), replace=TRUE)
    c_or_d <- do.call(rbind, c_or_d)
    cd[above_surface_mask,][c_or_d] <-   cbind(dsurf[above_surface_mask], csurf[above_surface_mask])[c_or_d]
    new_traits <- traits
    new_traits[, colnames(cd)] <- cd
    
    return(new_traits)
  }else{ #do nothing
    
    return(traits)
  }
}

# a list of traits to include with each species, traits with ss_eff_ are hack traits to extract in site processes
trait_names = c("dispersal", "t_opt", "t_range", "competition")

# ranges to scale the input environemts with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# lsited with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
# environmental_ranges = list("mean_temp"=c(9,26), "min_temp"=c(9,26),  "max_temp"=c(9,26))

#-------------------------#
#### Observer Function ####
#-------------------------#

end_of_timestep_observer = function(data, vars, config){
  plot_richness(data$all_species, data$space)
  save_species()
  save_abundance()
  save_divergence()
  save_occupancy()
  save_phylogeny()
  save_traits()
  # jpeg(paste0(config$directories$output_plots, "/ranges_t", vars$ti, ".jpeg"))
  # {
  #   par(mfrow=c(1,1))
  #   plot_ranges(data$all_species[c(1:3)], data$space)
  # }
  # dev.off()
  
  #plot_richness(data$all_species, data$space)
  #plot_richness( data$all_species, data$space)
  #abline(h=15)
  #abline(v=-20)
  
  # jpeg(paste0(config$directories$output_plots, "/ranges_t", vars$ti, ".jpeg"))
  #   par(mfrow=c(2,2), mai=c(0.2,0.5,0.3,0.5))
  #   plot_richness(data$all_species, data$space)
  # plot_species_abundance(data$all_species[[1]], data$space)
  #   plot_species_abundance(data$all_species[[3]], data$space)
  #   plot_species_abundance(data$all_species[[4]], data$space)
  # dev.off()
  # save_species()
  # save_abundance()
  # save_divergence()
  # save_occupancy()
  # save_phylogeny()
  # save_traits()
  #plot_species_abundance(data$all_species[[1]], data$space)
  # make p/a matrices if necessary
  # if(!file.exists(file.path(config$directories$output, "abs"))){dir.create(file.path(config$directories$output, "abs"))}
  # # site names
  # all_sites <- rownames(data$space$coordinates)
  # # get 0 for absence and 1 for presence in each grid site
  # all_species_abundance <- do.call( cbind, lapply(data$all_species, FUN = function(x) {ifelse(all_sites %in% names(x$abundance), x$abundance, NA)}))
  # # colnames are species names
  # colnames(all_species_abundance ) <- unlist(lapply(data$all_species, function(x){x$id}))
  # # column bind with x/y coordinates
  # abundance_matrix <- cbind(data$space$coordinates, all_species_abundance)
  # abundance_matrix <- abundance_matrix[order(abundance_matrix[,"x"]),]
  # abundance_matrix <- abundance_matrix[abundance_matrix[, "x"]<=-20, ]
  # # Select rows where all values are NA
  # rows_with_all_NA <- apply(abundance_matrix[,-c(1,2)], 1, function(x) all(is.na(x)))
  # # Subset the data frame to keep only those rows
  # abundance_matrix <- abundance_matrix[!rows_with_all_NA, ]
  # #abundance_matrix <- cbind(unique_id=1:nrow(abundance_matrix), abundance_matrix)
  # ncols <- ncol(abundance_matrix)
  # df <- as.data.frame(abundance_matrix)
  # abundance_long <- reshape(as.data.frame(df),
  #                    varying = list(names(df)[3:ncols]), # Columns to melt into long format
  #                    v.names = "N_density", # Name of the variable that holds values
  #                    timevar = "sp_id", # The new variable that will hold the column names (species IDs in this case)
  #                    times = names(df)[3:ncols], # The original column names to use as species IDs
  #                    idvar = c("x", "y"), # ID variables
  #                    direction = "long")
  # abundance_long <- abundance_long[!is.na(abundance_long$N_density),]
  # #if x>=15 then set continent to North America else South America
  # abundance_long$continent <- ifelse(abundance_long$x>=15, "North America", "South America")
  # abundance_long$sim_id <- basename(config$directories$output)
  # abundance_long$time_kya <- vars$ti*10000 # scalling 10Ma to kya.
  # new_cols <- config$user$comb_vector
  # abundance_long <- do.call(cbind, c(abundance_long, new_cols))
  # saveRDS(abundance_long, file=file.path(config$directories$output,"abs",  paste0("abd_Saupe_t_",vars$ti, ".rds")))

  # if(!file.exists(file.path(config$directories$output, "mean_traits_sp"))){dir.create(file.path(config$directories$output, "mean_traits_sp"))}
  # saveRDS(data$eco_by_sp, file=file.path(config$directories$output,"mean_traits_sp",  paste0("mean_trs_t_",vars$ti, ".rds")))
}


#----------------------#
#### Initialization ####
#----------------------#

create_ancestor_species <- function(space, config) {
  # make yls latitudinal lines for the 5 species: 2 at extremes, 2 inbetween and 1 at the equator
  co <- space$coordinates
  co <- co[rownames(co)%in%rownames(space$environment),]
  # polar line y coordinate
  plyc <- min(abs(range(co[,"y"])))
  # temp line
  tlyc <- co[,"y"][which.min(abs(co[,"y"]-plyc/2))]  # get closest avaiable y to the pl/2
  # equator line
  elyc <- co[,"y"][which.min(abs(co[,"y"]-0))]  # get closest avaiable y to the 0
  yls <- c("polN"=as.numeric(plyc), "tempN"=as.numeric(tlyc), "Eq"=as.numeric(elyc), "tempS"=as.numeric(-tlyc), "polS"=as.numeric(-plyc))
  new_species <- list()
  for(i in 1:length(yls)){
    # i <- 3
    initial_sites <- rownames(co[abs(co[,"y"]-yls[i])<30,]) #tolerance of 5 degrees
    # initial_sites <- sample(initial_sites, 1)
    print(paste("i", i, "yls", yls[i], "n_sites", length(initial_sites)))
    new_species[[i]] <- create_species(initial_sites, config)
    #set local adaptation to max optimal temp equals local temp
    new_species[[i]]$traits[ , "dispersal"] <- 1
    new_species[[i]]$traits[ , "competition"] <- 0.92
    new_species[[i]]$traits[ , "t_opt"] <- space$environment[initial_sites,"temperature"]
    new_species[[i]]$traits[ , "t_range"] <- 0.4
    #plot_species_presence(new_species[[i]], space)
  }
  return(new_species)
}


#-----------------#
#### Dispersal ####
#-----------------#

# returns n dispersal values (proba distrib function)
get_dispersal_values <- function(n, species, space, config) {
  # mean_abd <- mean(species$abundance)
  # weight_abd <- species$abundance/mean_abd
  # # if shape =1 then it is an exponential distribution
  # values <- rweibull(n, shape = 1, scale = 100+(comb_vector$dispersal*(mean(species$traits[,"dispersal"]*weight_abd))))  #from 1 to 50
  values <- rep(9999,n)
  return(values)
}


#------------------#
#### Speciation ####
#------------------#

# threshold for genetic distance after which a speciation event takes place
divergence_threshold = 1 # between 10 and 50 ? as 0.1 to 0.5 Myrs or 100 - 500 kyrs

# adds a value of 1 to each geographic population cluster
get_divergence_factor <- function(species, cluster_indices, space, config) {
  return(1)
}

#-----------------------#
#### Trait Evolution ####
#-----------------------#

apply_evolution <- function(species, cluster_indices, space, config) {
  trait_evolutionary_power <-comb_vector$t_evo
  pw_tr_hom <- 0.5 # percentage of movement of local trait towards weighted trait mean within each population cluster 
  # pw_tr_hom = ZERO means no change while a value of ONE means that traits are equal within each populations cluster)
  traits <- species[["traits"]]
  # site names
  sites <- rownames(traits)
  #homogenize trait based on abundance
  # Homogenize all traits by weighted abundance, attention, exclude here any trait that should be more neutral
  # Bring traits closer to 50% of it's distance to the abundance weighted mean of the demographically connected 
  # population cluster
  trn <- config$gen3sis$general$trait_names
  for(cluster_index in unique(cluster_indices)){
    # cluster_index <- 1
    sites_cluster <- sites[which(cluster_indices == cluster_index)]
    B_t <- sum(species$abundance[sites_cluster])# total biomass
    B_r <- species$abundance[sites_cluster]/B_t# relative biomass
    for (ti in trn){
      # par(mfrow=c(1,2))
      # hist(traits[sites_cluster, ti], main="before")
      traits_ti <- traits[sites_cluster, ti]
      mean_trait <- sum(traits_ti*B_r)
      tr_hom <- mean_trait-traits_ti
      traits[sites_cluster, ti] <- traits_ti+(tr_hom*pw_tr_hom)
      # hist(traits[sites_cluster, ti], main="after")
    }
  }
  
  #mutate all traits except dispersal and competitive ability M0 and summary effective traits, which where excluded previously
  for (ti in c("t_opt", "t_range", "competition", "dispersal")){#trn[!trn%in%"dispersal"]){ # do not evolve dispersal
    if(ti=="competition"){ # check if competition and scale proportionally 0.1 variance /10 = 1, while 1 variance /1 = 1
      tep_sd=trait_evolutionary_power/10
    } else {
      tep_sd=trait_evolutionary_power
    }
    mutation_deltas <-rnorm(length(traits[, ti]), mean=0, sd=tep_sd)
    traits[, ti] <- traits[, ti] + mutation_deltas
  }
  # set bounds so that the species cant evolve a niche beyond that present in the data 
  # note that all temperature values are scaled between 0 and 1
  if(any(traits[, "t_range"] > 1)){traits[which(traits[,"t_range"]>1), "t_range"] <- 1}
  if(any(traits[, "t_range"] <= 0)){traits[which(traits[,"t_range"]<=0), "t_range"] <- 0.001} #limit of zero to avoid zero divisions on specialist/generalist trade-off
  if(any(traits[, "competition"] > 1)){traits[which(traits[,"competition"]>1), "competition"] <- 1}
  if(any(traits[, "competition"] < 0.9)){traits[which(traits[,"competition"]<0.9), "competition"] <- 0.9}
  if(any(traits[, "dispersal"] > 1)){traits[which(traits[,"dispersal"]>1), "dispersal"] <- 1}
  if(any(traits[, "dispersal"] < 0)){traits[which(traits[,"dispersal"]<0), "dispersal"] <- 0}
  
  return(traits)
}

#-------------------------------------------------#
#### Environmental and Ecological Interactions ####
#-------------------------------------------------#
apply_ecology <- function(abundance, traits, space, config, abundance_scale = 10, abundance_threshold = 8) {
  # #abundance threshold
  # abundance <- as.numeric(!abundance<abundance_threshold)
  # abundance <- (( 1-abs( traits[, "t_opt"] - space[, "temperature"]))*abundance_scale)*abundance
  # #abundance threshold
  # abundance[abundance<abundance_threshold] <- 10
  return(abundance)
}
# apply_ecology <- function(abundance, traits, space, config) {
#   ns <- length(abundance)
#   #### get rf, here r_f is the per capita growth rate of biomass that depends on the local site conditions 
#   # set env niche
#   env_gs <- fg(x=space[,"temperature"], a=100, b=traits[, "t_opt"], c=traits[, "t_range"])
#   # set growth rate
#   g <- .1
#   # abundance_tii first is only what the env. determines to be the new abundances
#   r_f <- g*env_gs
#   ###### get (a_ff) = species interaction coefficient and (afh)= heterospecific interaction coefficient 
#   # get traits Competition
#   c_c <- rep(0.8,ns) # intra competition
#   c_l <- traits[,"competition"]
#   # set a_ff and a_fh
#   a_ff <- 1-c_c
#   a_fh <- 1-c_l
#   # check if conditions are met in order to continue
#   if (any(a_ff<=a_fh)){
#     stop("a_ff has to be bigger than a_fh for this equilibrium function to be used! Check your intial and evolutionary conditions of the competition traits.")
#   }
#   ####### get K_f = the carrying capacity of species f (that is the equilibrium for the case without heterospecific biomass
#   K_f <- r_f/a_ff
#   ###### get J = the total biomass J* of the community in equilibrium
#   J <- get_J(a_ff, a_fh, K_f)
#   wistop <- FALSE
#   keep_on_while <- rep(TRUE, ns)
#   while(wistop==FALSE){
#     shall_live <- (a_ff*K_f)>(a_fh*J)
#     if (all(!shall_live)){ # in case all shall die
#       B_f <- as.numeric(shall_live) # set all to zero, since we only have shall_live==FALSE
#       wistop <- TRUE
#     }
#     if (all(shall_live[keep_on_while])){
#       B_f <- ((a_ff*K_f)-(a_fh*J))/(a_ff-a_fh)
#       B_f[!shall_live] <- 0
#       wistop <- TRUE
#     } else{
#       a_ff[!shall_live] <- 0
#       a_fh[!shall_live] <- 0
#       K_f[!shall_live] <- 0
#       keep_on_while <- shall_live
#       if (sum(keep_on_while)==0){
#         B_f[!shall_live] <- 0
#         wistop <- TRUE
#       }
#       J <- get_J(a_ff, a_fh, K_f)
#     }
#   }
#   names(B_f) <- names(abundance)
#   B_f[B_f<0.06] <- 0 # set extinct species abundance to zero
#   return(B_f)
# }
