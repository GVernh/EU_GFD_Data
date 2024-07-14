# This script completes 3 tasks:
# 1. Runs a covaraince-based structural equation models of input data
# 2. Runs a series of goodness of fit tests on a given model
# 3. If sample size is large, an iterator runs chi squared tests of fit on sub-samples of the data

# Author: G.Vernham
# Date: 03/02/2024
options(scipen = 100)
#############################################
################ LIBRARIES ##################
#############################################

libs <- c("tidyverse")

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

dir.create("./Analysis/Final_SEM_models/", 
           showWarnings = T)


#############################################
############# MODEL SELECTION ###############
#############################################
rm(list=ls())
source("./Analysis/Final_SEM_models/SEM_model_100.R")
source("./Analysis/Final_SEM_models/SEM_model_400.R") 
source("./Analysis/Final_SEM_models/SEM_model_1000.R")

# Subset to required data
data_100 = data_100[c("F.rich", "F.div", "F.eve")]
data_400 = data_400[c("F.rich", "F.div", "F.eve")]
data_1000 = data_1000[c("F.rich", "F.div", "F.eve")]

# DATA CLEANING
data_100 = reshape(data_100,timevar = "Dimension", varying = c("F.rich", "F.div", "F.eve"), 
            direction = "long")
data_100$Dimension <- dplyr::recode_factor(data_100$Dimension,
                                    "rich" = "Richness",
                                    "eve" = "Evenness",
                                    "div" = "Divergence")
data_100 = data_100 %>% dplyr::rename("Trait_diversity" = "F")

data_400 = reshape(data_400,timevar = "Dimension", varying = c("F.rich", "F.div", "F.eve"), 
                   direction = "long")
data_400$Dimension <- dplyr::recode_factor(data_400$Dimension,
                                           "rich" = "Richness",
                                           "eve" = "Evenness",
                                           "div" = "Divergence")
data_400 = data_400 %>% dplyr::rename("Trait_diversity" = "F")

data_1000 = reshape(data_1000,timevar = "Dimension", varying = c("F.rich", "F.div", "F.eve"), 
                   direction = "long")
data_1000$Dimension <- dplyr::recode_factor(data_1000$Dimension,
                                           "rich" = "Richness",
                                           "eve" = "Evenness",
                                           "div" = "Divergence")
data_1000 = data_1000 %>% dplyr::rename("Trait_diversity" = "F")


# CREATE BOXPLOTS
data(mtcars)
boxplot(Trait_diversity~Dimension,data=data_100, ylab="Std. value", xlab = "Trait diversity dimension", 
        col = c("#FF1F5B", "#009ADE", "#FFC61E"))
boxplot(Trait_diversity~Dimension,data=data_400, ylab="Std. value", xlab = "Trait diversity dimension", 
        col = c("#FF1F5B", "#009ADE", "#FFC61E"))
boxplot(Trait_diversity~Dimension,data=data_1000, ylab="Std. value", xlab = "Trait diversity dimension", 
        col = c("#FF1F5B", "#009ADE", "#FFC61E"))
        