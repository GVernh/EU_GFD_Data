rm(list=ls())
options(scipen = 100)
# -------------------- PACAKGES ------------------
if ("lavaan" %in% rownames(installed.packages()) == FALSE) {install.packages("lavaan")}
library(lavaan)
if ("piecewiseSEM" %in% rownames(installed.packages()) == FALSE) {install.packages("piecewiseSEM")}
library(piecewiseSEM)
if ("lavaanPlot" %in% rownames(installed.packages()) == FALSE) {install.packages("lavaanPlot")}
library(lavaanPlot)
if ("effectsize" %in% rownames(installed.packages()) == FALSE) {install.packages("effectsize")}
library(effectsize)

setwd("D:/Europe_study_data/R scripts/Final_SEM_models/")

# ----------------- MODEL SELECTION ----------------

source("SEM_model_100.R")
source("SEM_model_400.R") 
source("SEM_model_1000.R")

# -------------------- MODEL RUN -------------------

SEM = sem(model = model_1000_Eve, data = data_1000)
summary(SEM, standardized = T, rsq = T)

lavaan::fitMeasures(SEM, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM)
head(mi[order(mi$mi, decreasing = T),],30)

# ------------- Iterator (P) --------------------

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
