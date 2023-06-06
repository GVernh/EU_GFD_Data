# NOTE: these variable shave been checked individually for normallity transformations, some (e.g. F.eve) had worse skew post
# transformation hence they have been left untransformed

# NOTE: any temperature data that appears in the below loops will have negative values and so will need to be log10 transformed above
# and then loops ran again.
# -------------- LIBRARIES ---------------------
rm(list = ls())

if ("devtools" %in% rownames(installed.packages()) == F) {install.packages("devtools")}
library(devtools)
if ("lolcat" %in% rownames(installed.packages()) == F) {install.packages("lolcat")}
library(lolcat)
if ("Hmisc" %in% rownames(installed.packages()) == F) {install.packages("Hmisc")}
library(Hmisc)
if ("tidyverse" %in% rownames(installed.packages()) == F) {install.packages("tidyverse")}
library(tidyverse)

# -------------- 100m PLOT DATA TRANSFORMATION ---------------------

# --------------- DATA -------------------------
setwd("\Europe_study_Git\Analyses\Finalised_datasets\Untransformed_data")


data_100 = read.csv("./Full_dataset_100m_plots.csv")
data_100 <- data_100 %>%
  select("Land_system_names", "Land_use", "lon", "lat", "Landform_sha","F.eve", everything())
trans_data = data_100

for(i in seq(7,length(trans_data))){
  x = skewness.test(trans_data[[i]])
  if (x$statistic > 0.6 | x$statistic < -1.25) {c = trans_data[[i]]^(1/3)  # c = log10(as.data.frame(data_5x5[[i]] +1))
  s = print(colnames(trans_data[i]))
  trans_data[paste(s)] = c
  }
}


trans_data[c(-1,-2,-3,-4)] <- as.data.frame(scale(trans_data[c(-1,-2,-3,-4)])) # standardize by z score



setwd("\Europe_study_Git\Analyses\Finalised_datasets")
write.csv(x = trans_data, file = "./Full_dataset_100m_plots_transformed.csv", row.names = F)

x = c("F.rich","F.div", "F.eve", "Top_Rao", "TWI_Rao","N_Rao","Cec_Rao","Soc_Rao","pH_Rao","BIO1_Annual_Mean_Temperature",
                 "BIO12_Annual_Precipitation","Landform_sha","Clay_Rao","intensity_ord_N","Sand_Rao","Silt_Rao")



test = trans_data[, (colnames(trans_data) %in% x)]

hist.data.frame(trans_data)

# ------------------ 400m DATA TRANSFORMATION -------------------------------
rm(list = ls())
setwd("\Europe_study_Git\Analyses\Finalised_datasets\Untransformed_data")


data_400 = read.csv("./Full_dataset_400m_plots.csv")
data_400 <- data_400 %>%
  select("Land_system_names","Land_use", "lon", "lat","Landform_sha","BIO9_Mean_Temperature_Driest_Quarter", everything())
trans_data = data_400

trans_data$BIO9_Mean_Temperature_Driest_Quarter = log10(data_400$BIO9_Mean_Temperature_Driest_Quarter + 14) # negative values 

for(i in seq(7,length(trans_data))){
  x = skewness.test(trans_data[[i]])
  if (x$statistic > 0.6 | x$statistic < -1.25) {c = trans_data[[i]]^(1/3)  # c = log10(as.data.frame(data_5x5[[i]] +1))
  s = print(colnames(trans_data[i]))
  trans_data[paste(s)] = c
  }
}

# hist.data.frame(trans_data)
#hist.data.frame(data_400)

trans_data[c(-1,-2,-3,-4)] <- as.data.frame(scale(trans_data[c(-1,-2,-3,-4)])) # standardize by z score

#hist.data.frame(trans_data)

setwd("\Europe_study_Git\Analyses\Finalised_datasets")
write.csv(x = trans_data, file = "./Full_dataset_400m_plots_transformed.csv", row.names = F)


# -------------- 1000m plot transformation --------------------

rm(list = ls())
setwd("\Europe_study_Git\Analyses\Finalised_datasets\Untransformed_data")


data_1000 = read.csv("./Full_dataset_1000m_plots.csv")
data_1000 <- data_1000 %>%
  select("Land_system_names","Land_use", "lon", "lat", everything())
trans_data = data_1000

for(i in seq(5,length(trans_data))){
  x = skewness.test(trans_data[[i]])
  if (x$statistic > 0.6 | x$statistic < -1.25) {c = trans_data[[i]]^(1/3)  # c = log10(as.data.frame(data_5x5[[i]] +1))
  s = print(colnames(trans_data[i]))
  trans_data[paste(s)] = c
  }
}

trans_data[c(-1,-2,-3,-4)] <- as.data.frame(scale(trans_data[c(-1,-2,-3,-4)])) # standardize by z score

hist.data.frame(trans_data)

setwd("\Europe_study_Git\Analyses\Finalised_datasets")
write.csv(x = trans_data, file = "./Full_dataset_1000m_plots_transformed.csv", row.names = F)