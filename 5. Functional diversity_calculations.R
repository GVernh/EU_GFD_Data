libs <- c("plyr","ape","hypervolume","geometry","abind","rJava","dplyr","foreach", "fundiversity", "purrr",
  "doSNOW", "corpcor", "vegan", "igraph")

installed_libs <- libs %in% rownames(
  installed.packages())

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))
rm(list=ls())

parallel=TRUE
options(scipen = 100)
options(future.globals.maxSize= 891289600) # Increase memory limit for future

# TO DO:
  # Mean and maximum of trait diversity at 1km cell is needed.
  # CWV of each 1km cell is needed (mean and maximum).

#################################################
################# FUNCTIONS #####################
#################################################
source("./Functions/snow_cluster.r") # Only for Parallel processing
source("./Functions/FD_matrix.R")
source('./Functions/fn_estim_meanDist.r') # Functional distance from mean of each trait to the centroid of occupied space (dissimilarity)
source('./Functions/fn_estim_meanGeod.R') # Functional geodestic distance THIS DRASTICALLY INCREASES PROCESSING TIME
source('./Functions/fn_estim_nMST.R') # Functional evenness
source('./Functions/fn_estim_convHullCont.r') # Functional richness, TAKES FOREVER ON AUGUSTA
#source('./Functions/fn_estim_hyperVolCont.R') # Hypervolume as described by Blonder et al. 2014 CURRENTLY NOT WORKING
source('./Functions/fn_estim_entMSTpalencia.r') 
source('./Functions/fn_estim_eVar.r') # effective variance V_e and SD_e as described by Pe{\~n}a & Rodriguez 2003
source('./Functions/fn_estim_eDep.r') # effective dependence D_e as described by Pena & Rodriguez 2003:
source('./Functions/fn_estim_eRDC.r') # effective RDC analogue to the effective dependence
# source('./Functions/fn_estim_eMIC.r') # causes fatal error due to rjava (reported here: https://github.com/rstudio/rstudio/issues/11076), a patch to resolve is being created
source('./Functions/fn_makeDepTraits.r')
source('./Functions/FD_functions.r')


#####################################################################
############################ TRAIT DATA #############################
#####################################################################

traits <- read.csv("./Raw_data/Biodiversity/Trait/Selected_trait_data.csv") # no missing values!
ntraits <- 7 # define number of traits
traits[,4:dim(traits)[2]] <- scale(traits[,4:dim(traits)[2]])

#####################################################################
##################### SPECIFICATIONS ################################
#####################################################################
# Specify directories and tags of each plot size.
setwd("./Raw_data/Biodiversity/Trait_diversity/")
dir = c("./100m_data/", "./400m_data/", "./1000m_data/","./2500m_data/", "./All_plot_data/")
tags = c("_100m", "_400m", "_1000m", "_2500m", "all_plots")
# Choose from: "meanDist", "meanGeod", "nMST", "convHullCont", "hyperVolCont", "eVar", "eDep", "eRDC"
FD_estimators  <- c("meanDist", "nMST", "eDep", "eRDC", "convHullCont", "eVar")

for (i in 1:length(dir)) {
  print(dir[i])
  
  # lat/lon coordinates of "loc" in the Spp_Matrix
  coords <- read.csv(paste0(dir[i],"Coords_FD",tags[i],".csv"))
  Spp_Matrix <- read.csv(paste0(dir[i],"Abun_matrix_FD",tags[i],".csv"))
  
  Spp_Matrix = Spp_Matrix %>%  
    select_all(~gsub("\\s+|\\.", ".", .))
  # Only use indices that have some species
  s1<-rowSums(Spp_Matrix[,2:dim(Spp_Matrix)[2]])
  locind <- which(s1>0)
  
  # Activate cluster for parralel processing if needed
  #cl = createCluster(3, export = ls(globalenv()), 
  # lib = list("plyr", "ape", "hypervolume", "geometry","abind", "rJava", "dplyr"))
  #registerDoSNOW(cl)
  
  results <- aaply(.data=locind,.margins=1,.fun=getIndices,FD_estimators=FD_estimators,.parallel=parallel)
  #stopCluster
  
  colnames(results) <- c("location",FD_estimators)
  
  # Prepare data for fundiversity package
  Trait_mean = read.csv(paste0(dir[i],"Selected_traits_FD",tags[i],".csv"))
 #coords <- read.csv(paste0(dir[i],"Coords_FD",tags[i],".csv"))
  Spp_Matrix <- read.csv(paste0(dir[i],"Abun_matrix_FD",tags[i],".csv"))
  
  coords$loc <- as.numeric(coords$loc)
  rownames(Trait_mean) <- Trait_mean[,1]
  rownames(Spp_Matrix) <- Spp_Matrix[,1]
  Spp_Matrix <-within(Spp_Matrix, rm(loc))
  colnames(Spp_Matrix) = Trait_mean$Species
  Trait_mean = within(Trait_mean, rm(Species))
  Spp_Matrix = as.matrix(Spp_Matrix)
  
  # Functional richness, evenness, divergence
  Functional_divergence = fundiversity::fd_fdiv(traits=Trait_mean, sp_com = Spp_Matrix)
  # Functional_richness = fundiversity::fd_fric(traits=Trait_mean, sp_com = Abun_matrix)
  Functional_evenness = fundiversity::fd_feve(traits=Trait_mean, sp_com = Spp_Matrix)
  Functional_Rao = fundiversity::fd_raoq(traits=Trait_mean, sp_com = Spp_Matrix)
  Functional_Dispersion = fundiversity::fd_fdis(traits=Trait_mean, sp_com = Spp_Matrix)
  
  # Species richness
  sp_rich = vegan::diversity(x = Spp_Matrix, index = "shannon")
  sp_rich = as.data.frame(sp_rich)
  sp_rich$site = as.character(1:length(sp_rich$sp_rich))
  
  # Combine all results
  results_funD <- list(Functional_divergence, Functional_evenness, Functional_Dispersion, Functional_Rao, sp_rich)
  results_funD = results_funD %>% reduce(full_join, by='site')
  
  names(results_funD)[names(results_funD) == 'site'] <- 'loc'
  results_funD$loc <- as.numeric(results_funD$loc)
  x = list(results_funD, coords)
  results_funD = x %>% reduce(full_join, by='loc')
  
  results = as.data.frame(results)
  names(results_funD)[names(results_funD) == 'loc'] <- 'location'
  x = list(results, results_funD)
  results = x %>% reduce(full_join, by='location')
  results = na.omit(results)
  
  save(results,file=paste0(dir[i],"results", tags[i],".Rdata"))
}