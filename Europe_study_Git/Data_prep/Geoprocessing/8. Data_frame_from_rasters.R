# -------------------- Libraries -----------------------
if("terra" %in% rownames(installed.packages()) == FALSE) { install.packages("terra")}
library(terra)
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

# -------------- 100m plots ------------------------
rm(list = ls())
setwd("") # Specify where geoprocessed rasters are located

env_ras = list.files(path = "./Finalised_rasters/",
                     pattern = ".tif$",
                     recursive = F,
                     full.names = T)

FD_100_ras = list.files(path = "./Finalised_rasters/Functional_diversity/100m_plot_only/",
                        pattern = "tif$",
                        recursive = F,
                        full.names = T)

stack_100 = terra::rast(c(env_ras, FD_100_ras))

data_100 = as.data.frame(stack_100, na.rm = T) 

data_100 = data_100 %>% 
  rename(
    Top_Rao = DEM_250_Rao,
    Hyd_feat_Sha = hyd_feat_25,
    Cec_Rao = EU_cec_0_5_Rao,
    Clay_Rao = EU_clay_0_5_Rao,
    Land_use = EU_landSystem,
    N_Rao = EU_nitrogen_0_5_Rao,
    pH_Rao = EU_phh2o_0_5_Rao,
    Sand_Rao = EU_sand_0_5_Rao,
    Silt_Rao = EU_silt_0_5_Rao,
    Soc_Rao = EU_soc_0_5_Rao,
    C.hull = convHullCont,
    F.disp = FDis,
    F.div = FDiv,
    Sp.rich = sp_rich,
    F.eve = FEve,
    F.eDep = eDep,
    F.eRDC = eRDC,
    F.rich = eVar,
    F.dist = meanDist,
    F.nMST = nMST,
    F.rao = Q,
    Landform_sha = Landform_Sha_Rao,
    TWI_Rao = TWI_250_Rao
  )

data_100$Land_use <- factor(data_100$Land_use)

write.csv(x = data_100, file = "\Europe_study_Git\Analyses\Finalised_datasets\Full_dataset_100m_plots.csv", row.names = F)

# -------------- 400m plots ------------------------
rm(list = ls())
setwd("D:/Europe_study_data/")

env_ras = list.files(path = "./Finalised_rasters/",
                     pattern = ".tif$",
                     recursive = F,
                     full.names = T)

FD_400_ras = list.files(path = "./Finalised_rasters/Functional_diversity/400m_plot_only/",
                        pattern = "tif$",
                        recursive = F,
                        full.names = T)


stack_400 = terra::rast(c(env_ras, FD_400_ras))

data_400 = as.data.frame(stack_400, na.rm = T) 

data_400 = data_400 %>% 
  rename(
    Top_Rao = DEM_250_Rao,
    Hyd_feat_Sha = hyd_feat_25,
    Cec_Rao = EU_cec_0_5_Rao,
    Clay_Rao = EU_clay_0_5_Rao,
    Land_use = EU_landSystem,
    N_Rao = EU_nitrogen_0_5_Rao,
    pH_Rao = EU_phh2o_0_5_Rao,
    Sand_Rao = EU_sand_0_5_Rao,
    Silt_Rao = EU_silt_0_5_Rao,
    Soc_Rao = EU_soc_0_5_Rao,
    C.hull = convHullCont,
    F.disp = FDis,
    F.div = FDiv,
    Sp.rich = sp_rich,
    F.eve = FEve,
    F.eDep = eDep,
    F.eRDC = eRDC,
    F.rich = eVar,
    F.dist = meanDist,
    F.nMST = nMST,
    F.rao = Q,
    Landform_sha = Landform_Sha_Rao,
    TWI_Rao = TWI_250_Rao
  )

data_400$Land_use <- factor(data_400$Land_use)

write.csv(x = data_400, file = "\Europe_study_Git\Analyses\Finalised_datasets\Full_dataset_400m_plots.csv", row.names = F)


# ----------------------- 1000m plots --------------------------

rm(list = ls())

env_ras = list.files(path = "./Finalised_rasters/",
                     pattern = ".tif$",
                     recursive = F,
                     full.names = T)

FD_1000_ras = list.files(path = "./Finalised_rasters/Functional_diversity/1km_plots_only/",
                         pattern = "tif$",
                         recursive = F,
                         full.names = T)


stack_1000 = terra::rast(c(env_ras, FD_1000_ras))

data_1000 = as.data.frame(stack_1000, na.rm = T) 

data_1000 = data_1000 %>% 
  rename(
    Top_Rao = DEM_250_Rao,
    Hyd_feat_Sha = hyd_feat_25,
    Cec_Rao = EU_cec_0_5_Rao,
    Clay_Rao = EU_clay_0_5_Rao,
    Land_use = EU_landSystem,
    N_Rao = EU_nitrogen_0_5_Rao,
    pH_Rao = EU_phh2o_0_5_Rao,
    Sand_Rao = EU_sand_0_5_Rao,
    Silt_Rao = EU_silt_0_5_Rao,
    Soc_Rao = EU_soc_0_5_Rao,
    C.hull = convHullCont,
    F.disp = FDis,
    F.div = FDiv,
    Sp.rich = sp_rich,
    F.eve = FEve,
    F.eDep = eDep,
    F.eRDC = eRDC,
    F.rich = eVar,
    F.dist = meanDist,
    F.nMST = nMST,
    F.rao = Q,
    Landform_sha = Landform_Sha_Rao,
    TWI_Rao = TWI_250_Rao
  )

data_1000$Land_use <- factor(data_1000$Land_use)

write.csv(x = data_1000, file = "\Europe_study_Git\Analyses\Finalised_datasets\Full_dataset_1000m_plots.csv", row.names = F)