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
library(stringr)
library(tidyverse)
library(broom)
setwd("D:/Europe_study_data/R scripts/Final_SEM_models/")

# -------------- 100m path diagrams ------------------
source("SEM_model_100.R")

SEM_rich = sem(model = model_100_Rich, data = data_100)
SEM_eve = sem(model = model_100_Eve, data = data_100)
SEM_div = sem(model = model_100_Div, data = data_100)

labels_rich <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
               BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
               pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
               Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.rich = "Trait richness")

labels_eve <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                    BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                    pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                    Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.eve = "Trait evenness")

labels_div <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                    BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                    pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                    Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.div = "Trait divergence")

lavaanPlot(model=SEM_rich, labels = labels_rich, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
lavaanPlot(model=SEM_eve, labels = labels_eve, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
lavaanPlot(model=SEM_div, labels = labels_div, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))



#----------------------------- 400m path diagrams --------------------
source("SEM_model_400.R") 

SEM_rich = sem(model = model_400_Rich, data = data_400)
SEM_eve = sem(model = model_400_Eve, data = data_400)
SEM_div = sem(model = model_400_Div, data = data_400)

labels_rich <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                    BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                    pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                    Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.rich = "Trait richness")

labels_eve <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                   BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                   pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                   Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.eve = "Trait evenness")

labels_div <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                   BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                   pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                   Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.div = "Trait divergence")

lavaanPlot(model=SEM_rich, labels = labels_rich, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
lavaanPlot(model=SEM_eve, labels = labels_eve, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
lavaanPlot(model=SEM_div, labels = labels_div, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))

# ---------------------------- 1000m path diagrams -----------------------
source("SEM_model_1000.R")

SEM_rich = sem(model = model_1000_Rich, data = data_1000)
SEM_eve = sem(model = model_1000_Eve, data = data_1000)
SEM_div = sem(model = model_1000_Div, data = data_1000)

labels_rich <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                    BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                    pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                    Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.rich = "Trait richness")

labels_eve <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                   BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                   pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                   Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.eve = "Trait evenness")

labels_div <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                   BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                   pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                   Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.div = "Trait divergence")

lavaanPlot(model=SEM_rich, labels = labels_rich, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
lavaanPlot(model=SEM_eve, labels = labels_eve, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
lavaanPlot(model=SEM_div, labels = labels_div, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))

