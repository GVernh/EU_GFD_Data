# DATA TRANSFORMATION AND STANDARDISATION
# This loop fulfills 4 functions:
#   1. Transforms highly skewed data (cube root or log10)
#   2. Checks if transformation has increased normality (if rejected will use non-transformed data)
#   3. Outputs a list of transformed varaibles in console
#   4. Standardizes all variables via Z scoring
# Author: G.Vernham 
# Date: 02/02/2024

#######################################################
################### LIBRARIES #########################
#######################################################
libs <- c("devtools", "Hmisc","tidyverse", "e1071")

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

setwd("./Analysis/Finalised_dataset/")

#######################################################
################# SPECIFICATIONS ######################
#######################################################

# Directory with non-transformed data
dir = list.files(path = "./",
                 pattern = ".csv",
                 full.names = T,
                 recursive = F)
dir = c("./Full_dataset_100m_plots.csv", "./Full_dataset_400m_plots.csv", 
        "./Full_dataset_1000m_plots.csv", "./Full_datasetall_plots_plots.csv")

# Specify variables with negative values
neg_temp = c("BIO1_Annual_Mean_Temperature", "BIO11_Mean_Temperature_Coldest_Quarter",
             "BIO6_Min_Temperature_Coldest_Month","BIO8_Mean_Temperature_Wettest_Quarter",
             "BIO9_Mean_Temperature_Driest_Quarter")
# Specify variables unsuitable for transformation
no_trans = c("Land_system_names", "Land_use", "lon", "lat")

# Create tags for unique file names
tags = c("_100m", "_400m", "_1000m", "all_plots")

# Create directory to store transformed data
dir.create("./Transformed/", showWarnings = FALSE)

#######################################
############ APPLY LOOP ###############
#######################################

for (i in seq(1:length(dir))) {
  print(tags[i])
  data <- read.csv(paste0(dir[i]))
  data <- data %>%
  select(paste0(no_trans),
         paste0(neg_temp), everything())
  trans_data <- data
  
  
  # Cube root variables with no negative values
  for(t in seq(length(no_trans) + length(neg_temp) +1,length(trans_data))){
    x = e1071::skewness(trans_data[[t]])
    if (x > 0.6 | x < -1.25) {c = trans_data[[t]]^(1/3)  # c = log10(as.data.frame(data_5x5[[i]] +1))
    y = abs(e1071::skewness(c))
    b = abs(x)
    if (y < b){ # Reject transformed data that does not improve normality
      s = print(colnames(trans_data[t]))
      trans_data[paste(s)] = c}
    }
  }
  trans_data[c(-1,-2,-3,-4)] <- as.data.frame(scale(trans_data[c(-1,-2,-3,-4)]))
  
  write.csv(x = trans_data, file = paste0("./Transformed/Full_dataset", tags[i],"_trans.csv"), row.names = F)
}

####################################
############ OPTIONAL ##############
####################################

# Add this to loop if you wish to log10 transform variables with negative values
# NOTE: This adds the absolute minimum negative value rounded up to the log transformation to avoid inducing NAs
for (n in seq(length(no_trans) +1, length(no_trans) + length(neg_temp))) {
  x = e1071::skewness(trans_data[[n]])
  if (x > 0.6 | x < -1.25) {c = log10(trans_data[[n]] + abs(min(data[[n]])) + 1)
  y = abs(e1071::skewness(c))
  b = abs(x)
  if (y < b){ # Reject transformed data that does not improve normality
    s = print(colnames(trans_data[n]))
    trans_data[paste(s)] = c} 
  }
}