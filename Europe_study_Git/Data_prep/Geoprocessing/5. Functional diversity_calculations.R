### CHange number of bootstrapping rounds in nrep: "getIndices <- function(location, FD_estimators, nrep=20)" 
### Change number of observations sampled in nobs (e.g. 300): "getTraitMatrix" and  "m <- getTraitMatrix(location,300,FALSE)"

# Code marked with "####################" requires input priot to running code.


rm(list=ls())
parallel=TRUE
if ("plyr" %in% rownames(installed.packages()) == F) { install.packages("plyr")}
library(plyr)
if ("ape" %in% rownames(installed.packages()) == F) { install.packages("ape")}
library(ape)
if ("hypervolume" %in% rownames(installed.packages()) == F) { install.packages("hypervolume")}
library(hypervolume)
if ("geometry" %in% rownames(installed.packages()) == F) { install.packages("geometry")}
library(geometry)
if ("abind" %in% rownames(installed.packages()) == F) { install.packages("abind")}
library(abind)
if ("rJava" %in% rownames(installed.packages()) == F) { install.packages("rJava")}
library(rJava)
if ("dplyr" %in% rownames(installed.packages()) == F) { install.packages("dplyr")}
library(dplyr)
if ("foreach" %in% rownames(installed.packages()) == F) { install.packages("foreach")}
library(foreach)
if ("doSNOW" %in% rownames(installed.packages()) == F) { install.packages("doSNOW")}
library(doSNOW)
if ("raster" %in% rownames(installed.packages()) == F) { install.packages("raster")}
library(raster)
if ("corpcor" %in% rownames(installed.packages()) == F) { install.packages("corpcor")}
library(corpcor)
if("vegan" %in% rownames(installed.packages()) == F) {install.packages("vegan")}
library(vegan)
# ---------------------- Functions for parralel processing --------------
clusterExport <- local({
  gets <- function(n, v) { assign(n, v, envir = .GlobalEnv); NULL }
  function(cl, list, envir = .GlobalEnv) {
    ## do this with only one clusterCall--loop on slaves?
    for (name in list) {
      clusterCall(cl, gets, name, get(name, envir = envir))
    }
  }
})

createCluster = function(noCores, logfile = "", export = NULL, lib = NULL) {
  require(doSNOW)
  cl <- makeCluster(noCores, type = "SOCK", outfile = logfile)
  if(!is.null(export)) clusterExport(cl, export)
  if(!is.null(lib)) {
    l_ply(lib, function(dum) { 
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoSNOW(cl)
  return(cl)
}

# ------------- Function to generate a trait matrix for a given location --------
# TODO: change sampling mechanism to sample from (log-)normal distribution
getTraitMatrix<-function(location, # location index
                         nobs=200, # Number of observations
                         perspecies=FALSE # Is the number of obs interpreted as per species or total?
) {
  
  cn    <- colnames(Spp_Matrix[,2:dim(Spp_Matrix)[2]])[Spp_Matrix[location,2:dim(Spp_Matrix)[2]]>0]
  nspec <- length(cn)

  m     <- matrix(,ncol=ntraits,nrow=0)
  i     <- 1
  if (perspecies == TRUE) {
    for (i in 1:length(cn)) {
      inds  <- which(traits$Spp==cn[i])
      ichos<- sample(x=inds, size=nobs, replace=TRUE)
      m    <- rbind(m,traits[ichos,4:dim(traits)[2]])
    }
  } else {
    # Calculate how many observations are drawn per species
    nobsperspec <- rep(floor(nobs / nspec),nspec)
    nmore       <- nobs %% nspec
    if (nmore > 0) {
      imore       <- sample(nspec, size=nmore,replace=FALSE)
      nobsperspec[imore] <- nobsperspec[imore] + 1
    }
    for (i in 1:length(cn)) { 
      inds  <- which(traits$Spp==cn[i])
      if (length(inds)>0) {
      ichos<- sample(x=inds, size=nobsperspec[i], replace=TRUE)
      m    <- rbind(m,traits[ichos,4:dim(traits)[2]])
      } else {
        warning(paste0("In location ",location," Species ", cn[i]," does not exist in trait table! Species omitted"))
      }
    }
  }
  return(m)
}

# Function that calculates a list of FD indices in a given location, nrep determines number of bootstrapping rounds
getIndices <- function(location, FD_estimators, nrep=10) {
  results        <- matrix(0,nrow=1,ncol=length(FD_estimators)+1)
  n              <- matrix(0,nrow=1,ncol=length(FD_estimators)+1)
  results[1,1]   <- location
  n[1]           <- 1
  for (irep in 1:nrep) {
  m              <- getTraitMatrix(location,200,FALSE)
  for (i in 1:length(FD_estimators)) {
    expr <- parse(text=paste0("results[1,i+1]<-results[1,i+1] + ",FD_estimators[i],"(m); n[i+1]<-n[i+1]+1")) 
    try(eval(expr))
  }
  }
  results[,-1] <- results[,-1]/n[-1]
  print(location)
  return(results)
}

# source important functions - set to working directory with source code
setwd("\Europe_study_Git\Data_prep\meth")

source('fn_estim_meanDist.r') # Functional distance from mean of each trait to the centroid of occupied space (dissimilarity)
source('fn_estim_meanGeod.R') # Functional geodestic distance THIS DRASTICALLY INCREASES PROCESSING TIME
source('fn_estim_nMST.R') # Functional evenness
source('fn_estim_convHullCont.r') # Functional richness, TAKES FOREVER ON AUGUSTA
#source('fn_estim_hyperVolCont.R') # Hypervolume as described by Blonder et al. 2014 CURRENTLY NOT WORKING
source('fn_estim_entMSTpalencia.r') 
source('fn_estim_eVar.r') # effective variance V_e and SD_e as described by Pe{\~n}a & Rodriguez 2003
source('fn_estim_eDep.r') # effective dependence D_e as described by Pena & Rodriguez 2003:
source('fn_estim_eRDC.r') # effective RDC analogue to the effective dependence
# source('fn_estim_eMIC.r') # causes fatal error due to rjava (reported here: https://github.com/rstudio/rstudio/issues/11076), a patch to resolve is being created
source('fn_makeDepTraits.r')
source('FD_functions.r')

# SELECT DESIRED PLOT SIZE
setwd("\Europe_study_Git\Data_prep\Biodiversity_data\100m_plots_only/")
#setwd("\Europe_study_Git\Data_prep\Biodiversity_data\400m_plots_only/")
# setwd("\Europe_study_Git\Data_prep\Biodiversity_data\1km_plots_only/")      #################################

# complete case trait matrix (individual plant species x trait) - needs to have no missing values!
traits <- read.csv("\Europe_study_Git\Data_prep\Biodiversity_data\Selected_trait_data.csv")
ntraits <- 7 # define number of traits

# only foliar traits
#traits <- read.csv("filledXBackTrans_Spp_leafTraits.csv")
#ntraits <- 6 # define number of traits


# normalize globally
# TODO: Change rescaling to log-normal
traits[,4:dim(traits)[2]] <- scale(traits[,4:dim(traits)[2]])

# lat/lon coordinates of "loc" in the Spp_Matrix
coords <- read.csv("Coords_FD.csv")

# location x individual matrix with location coded as "loc"
# Spp_Matrix <- read.csv ("merged_xy.vs.species_EW_EUFORGEN_AFE_0_1_FINAL.csv"),stringsAsFactors=TRUE,header=TRUE)
# land-cover weighted location x individual matrix (presence/absence, if you want weighted, remove 01 from the end of file name)
# Spp_Matrix <- read.csv ("merged_xy.vs.species_EW_EUFORGEN_correct_PFTcoded_01.csv"),stringsAsFactors=TRUE,header=TRUE)
# only evergreen species
Spp_Matrix <- read.csv(("Abun_matrix_FD.csv"),stringsAsFactors=TRUE,header=TRUE)
Trait_mean = read.csv("./Selected_traits_FD.csv")
colnames(Spp_Matrix)[2:ncol(Spp_Matrix)] = Trait_mean$Species
rm(Trait_mean)

Spp_Matrix = Spp_Matrix %>%  
  select_all(~gsub("\\s+|\\.", ".", .))
# Only use indices that have some species
s1<-rowSums(Spp_Matrix[,2:dim(Spp_Matrix)[2]])
locind <- which(s1>0)


# Select which functional diversity indices you want (e.g. mean Geodetic distance, Convex Hull, Hypervolume)
#FD_estimators  <- c("meanDist", "meanGeod", "nMST", "convHullCont", "hyperVolCont", "eVar", "eDep", "eRDC")
FD_estimators  <- c("meanDist", "nMST", "eDep", "eRDC", "convHullCont", "eVar")
#FD_estimators  <- c("convHullCont", "nMST", "eVar")

# Activate cluster for parralel processing
#cl = createCluster(3, export = ls(globalenv()), 
               # lib = list("plyr", "ape", "hypervolume", "geometry","abind", "rJava", "dplyr"))
#registerDoSNOW(cl)
# Do the actual computation - if you want to test whether the code works, include [1000:1010] between locind and ,. as in the commented out line below
# highly recommended as otherwise this will take a long time to run!
start.time = proc.time()
results <- aaply(.data=locind,.margins=1,.fun=getIndices,FD_estimators=FD_estimators,.parallel=parallel)
stop.time = proc.time()
stop.time - start.time
#stopCluster

#results <- aaply(.data=locind,.margins=1,.fun=getIndices,FD_estimators=FD_estimators,.parallel=parallel)
colnames(results) <- c("location",FD_estimators)

save(results,file="results_100m_plots.Rdata")
# save(results,file="results_400m_plots.Rdata")
# save(results,file="results_1km_plots.Rdata")                                      #################################


# ------------------------- Functional div, rich, eve (FunDiversity package) ---------------------------

rm(list=ls())

options(scipen = 100)
library(fundiversity)
library(dplyr)
library(tidyverse)
library(vegan)

Trait_mean = read.csv("./Selected_traits_FD.csv")
Abun_matrix = read.csv("./Abun_matrix_FD.csv")
Coords = read.csv("./Coords_FD.csv")
Coords$loc <- as.numeric(Coords$loc)
rownames(Trait_mean) <- Trait_mean[,1]
rownames(Abun_matrix) <- Abun_matrix[,1]
Abun_matrix <-within(Abun_matrix, rm(loc))
colnames(Abun_matrix) = Trait_mean$Species
Trait_mean = within(Trait_mean, rm(Species))
Abun_matrix = as.matrix(Abun_matrix)

options(future.globals.maxSize= 891289600) # Increase memory limit for future

# --------------- Functional diversity/species richness calculations -----------------

Functional_divergence = fundiversity::fd_fdiv(traits=Trait_mean, sp_com = Abun_matrix)

# Functional_richness = fundiversity::fd_fric(traits=Trait_mean, sp_com = Abun_matrix)

Functional_evenness = fundiversity::fd_feve(traits=Trait_mean, sp_com = Abun_matrix)

Functional_Rao = fundiversity::fd_raoq(traits=Trait_mean, sp_com = Abun_matrix)

Functional_Dispersion = fundiversity::fd_fdis(traits=Trait_mean, sp_com = Abun_matrix)

sp_rich = vegan::diversity(x = Abun_matrix, index = "shannon")
sp_rich = as.data.frame(sp_rich)
sp_rich$site = as.character(1:length(sp_rich$sp_rich))

# -------------------- Data cleaning -----------------------------
results <- list(Functional_divergence, Functional_evenness, Functional_Dispersion, Functional_Rao, sp_rich)
results = results %>% reduce(full_join, by='site')

names(results)[names(results) == 'site'] <- 'loc'
results$loc <- as.numeric(results$loc)
x = list(results, Coords)
results = x %>% reduce(full_join, by='loc')


write.csv(results, "./results_FunD_100m_plots.csv", row.names = F)              ############################
# write.csv(results, "./results_FunD_400m_plots.csv", row.names = F)
# write.csv(results, "./results_FunD_1km_plots.csv", row.names = F)

# --------------------- Merge all FD variables -------------------- 

results1 = read.csv("\Europe_study_Git\Data_prep\Biodiversity_data\100m_plots_only/results_FunD_100m_plots.csv")
load(file="./results_100m_plots.Rdata")
results2 = as.data.frame(results)
                                                                                   ##############################
#results1 = read.csv("\Europe_study_Git\Data_prep\Biodiversity_data\400m_plots_only/results_FunD_400m_plots.csv")
#load(file="./results_400m_plots.Rdata")
#results2 = as.data.frame(results)

#results1 = read.csv("\Europe_study_Git\Data_prep\Biodiversity_data\1km_plots_only/results_FunD_1km_plots.csv")
#load(file="./results_1km_plots.Rdata")
#results2 = as.data.frame(results)

names(results1)[names(results1) == 'loc'] <- 'location'
x = list(results1, results2)
results = x %>% reduce(full_join, by='location')
results = na.omit(results)

write.csv(results, "./results_Full_100m_plots.csv", row.names = F)
#write.csv(results, "./results_Full_400m_plots.csv", row.names = F)
# write.csv(results, "./results_Full_1km_plots.csv", row.names = F)                 ############################