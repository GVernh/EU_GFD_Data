options(scipen = 100)
# Load packages
libs <- c("Hmisc", "tidyverse")

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

# 100m plot histograms

data_100 = read.csv("./Analysis/Finalised_dataset/Transformed/Full_dataset_100m_trans.csv")
data_100 <- data_100 %>%
  select(BIO1_Annual_Mean_Temperature, BIO12_Annual_Precipitation, F.div, F.eve, F.rich,
         TWI_Rao, Landform_sha, Soc_Rao, Silt_Rao, Sand_Rao,
         pH_Rao, N_Rao, Clay_Rao, Cec_Rao, Top_Rao, intensity_ord_N) %>%
  rename(
    `Annual temperature` = BIO1_Annual_Mean_Temperature,
    `Annual precipitation` = BIO12_Annual_Precipitation,
    `Trait divergence` = F.div,
    `Trait evenness` = F.eve,
    `Trait richness` = F.rich,
    `TWI het.` = TWI_Rao,
    `Landform het` = Landform_sha,
    `Soil C het.` = Soc_Rao,
    `Soil silt het.` = Silt_Rao,
    `Soil sand het.` = Sand_Rao,
    `Soil pH het.` = pH_Rao,
    `Soil N het.` = N_Rao,
    `Soil clay het.` = Clay_Rao,
    `Soil Cec het.` = Cec_Rao,
    `Topographic het.` = Top_Rao,
    `Land use int.` = intensity_ord_N
  )

par(mfrow = c(4, 4))
# 1800x900
# NOTE: EXPAND THE BOUNDARY OF THE PLOT WINDOW TO AVOI ERROR (DRAG AND DROP)
Hmisc::hist.data.frame(data_100)

# 400m plot histograms
rm(list=ls())
data_400 = read.csv("./Analysis/Finalised_dataset/Transformed/Full_dataset_400m_trans.csv")
data_400 <- data_400 %>%
  select(BIO1_Annual_Mean_Temperature, BIO12_Annual_Precipitation, F.div, F.eve, F.rich,
         TWI_Rao, Landform_sha, Soc_Rao, Silt_Rao, Sand_Rao,
         pH_Rao, N_Rao, Clay_Rao, Cec_Rao, Top_Rao, intensity_ord_N) %>%
  rename(
    `Annual temperature` = BIO1_Annual_Mean_Temperature,
    `Annual precipitation` = BIO12_Annual_Precipitation,
    `Trait divergence` = F.div,
    `Trait evenness` = F.eve,
    `Trait richness` = F.rich,
    `TWI het.` = TWI_Rao,
    `Landform het` = Landform_sha,
    `Soil C het.` = Soc_Rao,
    `Soil silt het.` = Silt_Rao,
    `Soil sand het.` = Sand_Rao,
    `Soil pH het.` = pH_Rao,
    `Soil N het.` = N_Rao,
    `Soil clay het.` = Clay_Rao,
    `Soil Cec het.` = Cec_Rao,
    `Topographic het.` = Top_Rao,
    `Land use int.` = intensity_ord_N
  )

par(mfrow = c(4, 4))
# 1800x900
Hmisc::hist.data.frame(data_400)


# 1000m plot histograms
rm(list=ls())
data_1000 = read.csv("./Analysis/Finalised_dataset/Transformed/Full_dataset_1000m_trans.csv")

data_1000 <- data_1000 %>%
  select(BIO1_Annual_Mean_Temperature, BIO12_Annual_Precipitation, F.div, F.eve, F.rich,
         TWI_Rao, Landform_sha, Soc_Rao, Silt_Rao, Sand_Rao,
         pH_Rao, N_Rao, Clay_Rao, Cec_Rao, Top_Rao, intensity_ord_N) %>%
  rename(
    `Annual temperature` = BIO1_Annual_Mean_Temperature,
    `Annual precipitation` = BIO12_Annual_Precipitation,
    `Trait divergence` = F.div,
    `Trait evenness` = F.eve,
    `Trait richness` = F.rich,
    `TWI het.` = TWI_Rao,
    `Landform het` = Landform_sha,
    `Soil C het.` = Soc_Rao,
    `Soil silt het.` = Silt_Rao,
    `Soil sand het.` = Sand_Rao,
    `Soil pH het.` = pH_Rao,
    `Soil N het.` = N_Rao,
    `Soil clay het.` = Clay_Rao,
    `Soil Cec het.` = Cec_Rao,
    `Topographic het.` = Top_Rao,
    `Land use int.` = intensity_ord_N
  )

par(mfrow = c(4, 4))
# 1800x900
hist.data.frame(data_1000, nclass=35)
