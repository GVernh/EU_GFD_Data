#############################################
############### LIBRARIES ###################
#############################################
libs <- c("terra", "tidyverse")

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

# LOAD RASTERS
env_ras = list.files(path = "./Analysis/Finalised_rasters/",
                     pattern = ".tif$",
                     recursive = F,
                     full.names = T)

# CREATE TAGS & DIRECTORIES

dir = c("./100m_data/", "./400m_data/", "./1000m_data/", "./All_plot_data/")
tags = c("_100m", "_400m", "_1000m", "all_plots")
dir.create("./Analysis/Finalised_dataset/", 
           showWarnings = T)

# LOOP THROUGH RASTERS & CREATE DATA FRAMES
for (i in 1:length(dir)){
  print(dir[i])
  FD_ras = list.files(path = paste0("./Analysis/Finalised_rasters/Functional_diversity", dir[i]),
                           pattern = "tif$",
                           recursive = F,
                           full.names = T)
  
  stack = terra::rast(c(env_ras, FD_ras))
  data = as.data.frame(stack, na.rm = T) 
  
  data = data %>% 
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
  data$Land_use <- factor(data$Land_use)
  
  write.csv(x = data, 
            file = paste0("./Analysis/Finalised_dataset/Full_dataset",tags[i],"_plots.csv"), 
            row.names = F)
}
