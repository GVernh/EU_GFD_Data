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

libs <- c("lavaan", "piecewiseSEM", "lavaanPlot","effectsize", "tidyverse")

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

#############################################
############### RUN 100m MODELS #############
#############################################

SEM_100 = sem(model = model_100_Rich, data = data_100)
summary(SEM_100, standardized = T, rsq = T)

lavaan::fitMeasures(SEM_100, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM_100)
head(mi[order(mi$mi, decreasing = T),],30)

SEM_100 = sem(model = model_100_Div, data = data_100)
summary(SEM_100, standardized = T, rsq = T)

lavaan::fitMeasures(SEM_100, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM_100)
head(mi[order(mi$mi, decreasing = T),],30)

SEM_100 = sem(model = model_100_Eve, data = data_100)
summary(SEM_100, standardized = T, rsq = T)

lavaan::fitMeasures(SEM_100, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM_100)
head(mi[order(mi$mi, decreasing = T),],30)

#############################################
############### RUN 400m MODELS #############
#############################################

SEM_400 = sem(model = model_400_Rich, data = data_400)
summary(SEM_400, standardized = T, rsq = T)

lavaan::fitMeasures(SEM_400, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM_400)
head(mi[order(mi$mi, decreasing = T),],30)

SEM_400 = sem(model = model_400_Div, data = data_400)
summary(SEM_400, standardized = T, rsq = T)

lavaan::fitMeasures(SEM_400, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM_400)
head(mi[order(mi$mi, decreasing = T),],30)

SEM_400 = sem(model = model_400_Eve, data = data_400)
summary(SEM_400, standardized = T, rsq = T)

lavaan::fitMeasures(SEM_400, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM_400)
head(mi[order(mi$mi, decreasing = T),],30)

#############################################
############### RUN 1000m MODELS ############
#############################################

SEM_1000 = sem(model = model_1000_Rich, data = data_1000)
summary(SEM_1000, standardized = T, rsq = T)

lavaan::fitMeasures(SEM_1000, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM_1000)
head(mi[order(mi$mi, decreasing = T),],30)

SEM_1000 = sem(model = model_1000_Div, data = data_1000)
summary(SEM_1000, standardized = T, rsq = T)

lavaan::fitMeasures(SEM_1000, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM_1000)
head(mi[order(mi$mi, decreasing = T),],30)

SEM_1000 = sem(model = model_1000_Eve, data = data_1000)
summary(SEM_1000, standardized = T, rsq = T)

lavaan::fitMeasures(SEM_1000, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM_1000)
head(mi[order(mi$mi, decreasing = T),],30)

#############################################
############# Iterator (P value)  ###########
#############################################

#100m
df <- data.frame(matrix(ncol = 6, nrow = 10000))
colnames(df) = c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr")

for (i in seq (1:10000)) {
  tryCatch({
    x = dplyr::slice_sample(data_100, n = 300, replace = F)
    SEM = lavaan::sem(model_100_Div, data = x)
    FM <- lavaan::fitMeasures(SEM, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
    df[i,] = FM
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

p_mean_100 = mean(df$pvalue, na.rm = T)
p_range_100 = range(df$pvalue, na.rm = T)
p_median_100 = median(df$pvalue, na.rm = T)


# 400m
df <- data.frame(matrix(ncol = 6, nrow = 10000))
colnames(df) = c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr")

for (i in seq (1:10000)) {
  tryCatch({
    x = dplyr::slice_sample(data_400, n = 300, replace = F)
    SEM = lavaan::sem(model_400_Div, data = x)
    FM <- lavaan::fitMeasures(SEM, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
    df[i,] = FM
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

p_mean_400 = mean(df$pvalue, na.rm = T)
p_range_400 = range(df$pvalue, na.rm = T)
p_median_400 = median(df$pvalue, na.rm = T)
