options(scipen = 100)
libs <- c("corrplot", "RColorBrewer", "tidyverse")

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

# Load data

data_100 = read.csv("./Analysis/Finalised_dataset/Full_dataset_100m_plots.csv")
data_400 = read.csv("./Analysis/Finalised_dataset/Full_dataset_400m_plots.csv")
data_1000 = read.csv("./Analysis/Finalised_dataset/Full_dataset_1000m_plots.csv")

########################################
# Selected varaibles correlation matrix
########################################

Full_data = rbind(data_100, data_400,data_1000)

Full_data <- Full_data %>%
  dplyr::select(BIO1_Annual_Mean_Temperature, BIO12_Annual_Precipitation, F.div, F.eve, F.rich,
         TWI_Rao, Landform_sha, Soc_Rao, Silt_Rao, Sand_Rao,
         pH_Rao, N_Rao, Clay_Rao, Cec_Rao, Top_Rao, Sp.rich) %>%
  dplyr::rename(
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
    `Species richness` = Sp.rich
  )

cor_sel = cor(Full_data)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  
  jpeg(file= paste0("./Figures_Tables/Correlation_matrix.jpeg")
       ,width = 1200, height = 1144)
  corrplot::corrplot(cor_sel, method="square",col=col(200), type = "upper", tl.col="black",number.cex = 1,tl.cex = 1.3,
           diag = F, addCoef.col = "black")
  dev.off()
  
#############################################
# All considered variables correlation matrix
#############################################
  
Full_data = rbind(data_100, data_400,data_1000)

Full_data = 
  dplyr::Full_data %>% select(-c("lon", "lat", "Land_use", "Land_system_names", "F.nMST", "F.eDep", 
                          "C.hull", "F.eRDC", "F.dist")) %>%
  dplyr::rename(
    `Annual temperature` = BIO1_Annual_Mean_Temperature,
    `Annual precipitation` = BIO12_Annual_Precipitation,
    `Trait divergence` = F.div,
    `Trait evenness` = F.eve,
    `Trait richness` = F.rich,
    `Trait Rao Q` = F.rao,
    `Trait dispersion` = F.disp,
    "Temperature (warmest quarter)" = BIO10_Mean_Temperature_Warmest_Quarter,
    "Temperature (coldest quarter)" = BIO11_Mean_Temperature_Coldest_Quarter,
    "Precipitation (wettest month)" = BIO13_Precipitation_Wettest_Month,
    "Precipitation (driest month)" = BIO14_Precipitation_Driest_Month,
    "Precipitation (seasonality)"= BIO15_Precipitation_Seasonality,
    "Precipitation (wettest quarter)"= BIO16_Precipitation_Wettest_Quarter,
    "Precipitation (driest quarter)"= BIO17_Precipitation_Driest_Quarter,
    "Precipitation (warmest quarter)"= BIO18_Precipitation_Warmest_Quarter,
   "Precipitation (coldest quarter)" = BIO19_Precipitation_Coldest_Quarter,
   "Diurnal range" = BIO2_Mean_Diurnal_Range,
   "Isothermality" = BIO3_Isothermality,
   "Temperature (seasonality)" = BIO4_Temperature_Seasonality,
   "Temperature (warmest month)" = BIO5_Max_Temperature_Warmest_Month,
   "Temperature (coldest month)" =BIO6_Min_Temperature_Coldest_Month,
   "Temperature (annual range)" =BIO7_Temperature_Annual_Range,
   "Temperature (wettest quarter)" =BIO8_Mean_Temperature_Wettest_Quarter,
   "Temperature (driest quarter)" =BIO9_Mean_Temperature_Driest_Quarter,
    `TWI het.` = TWI_Rao,
    `Landform het` = Landform_sha,
    `Soil C het.` = Soc_Rao,
    `Soil silt het.` = Silt_Rao,
    `Soil sand het.` = Sand_Rao,
    `Soil pH het.` = pH_Rao,
    `Soil N het.` = N_Rao,
    `Soil clay het.` = Clay_Rao,
    `Soil Cec het.` = Cec_Rao,
    `Topographic het.` = Top_Rao
  )
cor_sel = cor(Full_data)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

jpeg(file= paste0("./Figures_Tables/Appendices/Correlation_matrix_all.jpeg")
     ,width = 1200, height = 1144)
corrplot::corrplot(cor_sel, method="square",col=col(200), type = "upper", tl.col="black",number.cex = 0.8,tl.cex = 0.9,
         diag = F, addCoef.col = "black")
dev.off()
