rm(list=ls())
setwd("D:/Europe_study_data/R scripts/Final_SEM_models/")

source("./lavSpatialCorrect.R")
source("./predict_lavaan.R")

# -------- CHOOSE PLOT SIZE -----------------

source("SEM_model_100.R")
source("SEM_model_400.R") 
source("SEM_model_1000.R")

# -------------- 100m plot test -----------------
SEM_Rich = sem(model = model_100_Rich_simp, data = data_100, meanstructure=T)
SEM_Eve = sem(model = model_100_Eve_simp, data = data_100, meanstructure=T)
SEM_Div = sem(model = model_100_Div_simp, data = data_100, meanstructure=T)

Spat_cor_rich = lavSpatialCorrect(SEM_Rich, data_100$lon, data_100$lat)
Spat_cor_rich = as.data.frame(Spat_cor_rich$parameters$F.rich)
Spat_cor_rich = Spat_cor_rich[1:13,]
Spat_cor_rich$model = "Trait richness"

Spat_cor_eve = lavSpatialCorrect(SEM_Eve, data_100$lon, data_100$lat)
Spat_cor_eve = as.data.frame(Spat_cor_eve$parameters$F.eve)
Spat_cor_eve = Spat_cor_eve[1:13,]
Spat_cor_eve$model = "Trait evenness"

Spat_cor_div = lavSpatialCorrect(SEM_Div, data_100$lon, data_100$lat)
Spat_cor_div = as.data.frame(Spat_cor_div$parameters$F.div)
Spat_cor_div = Spat_cor_div[1:13,]
Spat_cor_div$model = "Trait divergence"

Trait_coefs_100 = rbind(Spat_cor_div, Spat_cor_eve, Spat_cor_rich)
New_var = as.data.frame(str_split_fixed(Trait_coefs_100$Parameter, "~", 2))
Trait_coefs_100$"Predicted variable" = New_var$V1
Trait_coefs_100$"Predictor" = New_var$V2

Trait_coefs_100$Predictor = gsub(" ", "", Trait_coefs_100$Predictor, fixed = TRUE)
Trait_coefs_100$`Predicted variable` = gsub(" ", "", Trait_coefs_100$`Predicted variable`, fixed = TRUE)
Trait_coefs_100 <- Trait_coefs_100 %>%
  select(`Predicted variable`, Predictor, everything())
Trait_coefs_100 = subset(Trait_coefs_100, select = -c(Parameter, `Predicted variable`) )
row.names(Trait_coefs_100) <- NULL
Trait_coefs_100 = Trait_coefs_100 %>% 
  rename(
    term = Predictor,
    estimate = Estimate,
    std.error = Std.err,
    p.value = `P(>|z|)`,
    statistic = `Z-value`
  )

Trait_coefs_100[Trait_coefs_100$term == "BIO12_Annual_Precipitation", "term"] = "Annual precipitation"
Trait_coefs_100[Trait_coefs_100$term == "Cec_Rao", "term"] = "Soil Cec het."
Trait_coefs_100[Trait_coefs_100$term == "Clay_Rao", "term"] = "Soil clay het."
Trait_coefs_100[Trait_coefs_100$term == "Soc_Rao", "term"] = "Soil C het."
Trait_coefs_100[Trait_coefs_100$term == "pH_Rao", "term"] = "Soil pH het."
Trait_coefs_100[Trait_coefs_100$term == "Top_Rao", "term"] = "Topographic het."
Trait_coefs_100[Trait_coefs_100$term == "TWI_Rao", "term"] = "TWI het."
Trait_coefs_100[Trait_coefs_100$term == "intensity_ord_N", "term"] = "Land use intensity"
Trait_coefs_100[Trait_coefs_100$term == "BIO1_Annual_Mean_Temperature", "term"] = "Annual temperature"
Trait_coefs_100[Trait_coefs_100$term == "Landform_sha", "term"] = "Landform het."
Trait_coefs_100[Trait_coefs_100$term == "Sand_Rao", "term"] = "Soil sand het."
Trait_coefs_100[Trait_coefs_100$term == "Silt_Rao", "term"] = "Soil silt het."
Trait_coefs_100[Trait_coefs_100$term == "N_Rao", "term"] = "Soil N het."

write.csv(Trait_coefs_100, file = "D:/Europe_study_data/Writeup/Round_2_content/Spatially_corrected_data/FD_coefs_100m_spatially_corrected.csv", 
          row.names = F)

# -------------- 400m plot test -----------------
rm(Trait_coefs_100)

SEM_Rich = sem(model = model_400_Rich_simp, data = data_400, meanstructure=T)
SEM_Eve = sem(model = model_400_Eve_simp, data = data_400, meanstructure=T)
SEM_Div = sem(model = model_400_Div_simp, data = data_400, meanstructure=T)

Spat_cor_rich = lavSpatialCorrect(SEM_Rich, data_400$lon, data_400$lat)
Spat_cor_rich = as.data.frame(Spat_cor_rich$parameters$F.rich)
Spat_cor_rich = Spat_cor_rich[1:13,]
Spat_cor_rich$model = "Trait richness"

Spat_cor_eve = lavSpatialCorrect(SEM_Eve, data_400$lon, data_400$lat)
Spat_cor_eve = as.data.frame(Spat_cor_eve$parameters$F.eve)
Spat_cor_eve = Spat_cor_eve[1:13,]
Spat_cor_eve$model = "Trait evenness"

Spat_cor_div = lavSpatialCorrect(SEM_Div, data_400$lon, data_400$lat)
Spat_cor_div = as.data.frame(Spat_cor_div$parameters$F.div)
Spat_cor_div = Spat_cor_div[1:13,]
Spat_cor_div$model = "Trait divergence"

Trait_coefs_400 = rbind(Spat_cor_div, Spat_cor_eve, Spat_cor_rich)
New_var = as.data.frame(str_split_fixed(Trait_coefs_400$Parameter, "~", 2))
Trait_coefs_400$"Predicted variable" = New_var$V1
Trait_coefs_400$"Predictor" = New_var$V2

Trait_coefs_400$Predictor = gsub(" ", "", Trait_coefs_400$Predictor, fixed = TRUE)
Trait_coefs_400$`Predicted variable` = gsub(" ", "", Trait_coefs_400$`Predicted variable`, fixed = TRUE)
Trait_coefs_400 <- Trait_coefs_400 %>%
  select(`Predicted variable`, Predictor, everything())
Trait_coefs_400 = subset(Trait_coefs_400, select = -c(Parameter, `Predicted variable`) )
row.names(Trait_coefs_400) <- NULL
Trait_coefs_400 = Trait_coefs_400 %>% 
  rename(
    term = Predictor,
    estimate = Estimate,
    std.error = Std.err,
    p.value = `P(>|z|)`,
    statistic = `Z-value`
  )

Trait_coefs_400[Trait_coefs_400$term == "BIO12_Annual_Precipitation", "term"] = "Annual precipitation"
Trait_coefs_400[Trait_coefs_400$term == "Cec_Rao", "term"] = "Soil Cec het."
Trait_coefs_400[Trait_coefs_400$term == "Clay_Rao", "term"] = "Soil clay het."
Trait_coefs_400[Trait_coefs_400$term == "Soc_Rao", "term"] = "Soil C het."
Trait_coefs_400[Trait_coefs_400$term == "pH_Rao", "term"] = "Soil pH het."
Trait_coefs_400[Trait_coefs_400$term == "Top_Rao", "term"] = "Topographic het."
Trait_coefs_400[Trait_coefs_400$term == "TWI_Rao", "term"] = "TWI het."
Trait_coefs_400[Trait_coefs_400$term == "intensity_ord_N", "term"] = "Land use intensity"
Trait_coefs_400[Trait_coefs_400$term == "BIO1_Annual_Mean_Temperature", "term"] = "Annual temperature"
Trait_coefs_400[Trait_coefs_400$term == "Landform_sha", "term"] = "Landform het."
Trait_coefs_400[Trait_coefs_400$term == "Sand_Rao", "term"] = "Soil sand het."
Trait_coefs_400[Trait_coefs_400$term == "Silt_Rao", "term"] = "Soil silt het."
Trait_coefs_400[Trait_coefs_400$term == "N_Rao", "term"] = "Soil N het."

write.csv(Trait_coefs_400, file = "D:/Europe_study_data/Writeup/Round_2_content/Spatially_corrected_data/FD_coefs_400m_spatially_corrected.csv", 
          row.names = F)

# -------------- 1000m plot test -----------------
rm(Trait_coefs_400)

SEM_Rich = sem(model = model_1000_Rich_simp, data = data_1000, meanstructure=T)
SEM_Eve = sem(model = model_1000_Eve_simp, data = data_1000, meanstructure=T)
SEM_Div = sem(model = model_1000_Div_simp, data = data_1000, meanstructure=T)

Spat_cor_rich = lavSpatialCorrect(SEM_Rich, data_1000$lon, data_1000$lat)
Spat_cor_rich = as.data.frame(Spat_cor_rich$parameters$F.rich)
Spat_cor_rich = Spat_cor_rich[1:13,]
Spat_cor_rich$model = "Trait richness"

Spat_cor_eve = lavSpatialCorrect(SEM_Eve, data_1000$lon, data_1000$lat)
Spat_cor_eve = as.data.frame(Spat_cor_eve$parameters$F.eve)
Spat_cor_eve = Spat_cor_eve[1:13,]
Spat_cor_eve$model = "Trait evenness"

Spat_cor_div = lavSpatialCorrect(SEM_Div, data_1000$lon, data_1000$lat)
Spat_cor_div = as.data.frame(Spat_cor_div$parameters$F.div)
Spat_cor_div = Spat_cor_div[1:13,]
Spat_cor_div$model = "Trait divergence"

Trait_coefs_1000 = rbind(Spat_cor_div, Spat_cor_eve, Spat_cor_rich)

New_var = as.data.frame(str_split_fixed(Trait_coefs_1000$Parameter, "~", 2))
Trait_coefs_1000$"Predicted variable" = New_var$V1
Trait_coefs_1000$"Predictor" = New_var$V2

Trait_coefs_1000$Predictor = gsub(" ", "", Trait_coefs_1000$Predictor, fixed = TRUE)
Trait_coefs_1000$`Predicted variable` = gsub(" ", "", Trait_coefs_1000$`Predicted variable`, fixed = TRUE)
Trait_coefs_1000 <- Trait_coefs_1000 %>%
  select(`Predicted variable`, Predictor, everything())
Trait_coefs_1000 = subset(Trait_coefs_1000, select = -c(Parameter, `Predicted variable`) )
row.names(Trait_coefs_1000) <- NULL
Trait_coefs_1000 = Trait_coefs_1000 %>% 
  rename(
    term = Predictor,
    estimate = Estimate,
    std.error = Std.err,
    p.value = `P(>|z|)`,
    statistic = `Z-value`
  )

Trait_coefs_1000[Trait_coefs_1000$term == "BIO12_Annual_Precipitation", "term"] = "Annual precipitation"
Trait_coefs_1000[Trait_coefs_1000$term == "Cec_Rao", "term"] = "Soil Cec het."
Trait_coefs_1000[Trait_coefs_1000$term == "Clay_Rao", "term"] = "Soil clay het."
Trait_coefs_1000[Trait_coefs_1000$term == "Soc_Rao", "term"] = "Soil C het."
Trait_coefs_1000[Trait_coefs_1000$term == "pH_Rao", "term"] = "Soil pH het."
Trait_coefs_1000[Trait_coefs_1000$term == "Top_Rao", "term"] = "Topographic het."
Trait_coefs_1000[Trait_coefs_1000$term == "TWI_Rao", "term"] = "TWI het."
Trait_coefs_1000[Trait_coefs_1000$term == "intensity_ord_N", "term"] = "Land use intensity"
Trait_coefs_1000[Trait_coefs_1000$term == "BIO1_Annual_Mean_Temperature", "term"] = "Annual temperature"
Trait_coefs_1000[Trait_coefs_1000$term == "Landform_sha", "term"] = "Landform het."
Trait_coefs_1000[Trait_coefs_1000$term == "Sand_Rao", "term"] = "Soil sand het."
Trait_coefs_1000[Trait_coefs_1000$term == "Silt_Rao", "term"] = "Soil silt het."
Trait_coefs_1000[Trait_coefs_1000$term == "N_Rao", "term"] = "Soil N het."

write.csv(Trait_coefs_1000, file = "D:/Europe_study_data/Writeup/Round_2_content/Spatially_corrected_data/FD_coefs_1000m_spatially_corrected.csv", 
          row.names = F)