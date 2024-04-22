# ---------------------- libraries ------------------------------

libs <- c(
  "tidyverse", "stringr", "utf8", "stats", "utils", "devtools")

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

# TO DO:
  # Community weighted variance for each trait within each plot is also needed.

#---------------------- DATA ---------------------------------
load("./Raw_data/Biodiversity/SPlot/sPlotOpen.RData")

Traits = read.csv("./Raw_data/Biodiversity/Trait/Selected_trait_data.csv")

Loc_subset = header.oa[c("PlotObservationID", "Continent", "Releve_area", "is_forest", "Date_of_recording", "Location_uncertainty")]
EU_abun <- merge(DT2.oa, Loc_subset, by = "PlotObservationID")

############################################################
################### CLEANING: RAW DATA #####################
############################################################

EU_abun <- EU_abun %>%
  dplyr::filter(stringr::str_detect(Continent, "Europe")) %>%
  dplyr::select("PlotObservationID", "Species", "Relative_cover", "Releve_area", 
                "Date_of_recording","is_forest", "Location_uncertainty") %>%
  dplyr::mutate(across(Species, gsub, pattern = "_", replacement = " ")) %>%
  dplyr::mutate(Species= utf8::as_utf8(Species)) %>%
  dplyr::filter(is_forest != FALSE) %>% # Remove non-forests
  dplyr::filter(Location_uncertainty < 1001) # Remove plots with high location uncertainty

Traits = Traits[3:10]
Trait_mean = Traits %>%
  dplyr::mutate(across(Spp, str_replace_all, pattern = "[.']", replacement = " ")) %>%
  dplyr::group_by(Spp) %>%
  dplyr::summarise(across(everything(), mean)) %>%
  dplyr::rename(Species = Spp)

###############################################################
############### EXPLORE: FREQUENCY OF PLOT SIZES ##############
###############################################################

x = EU_abun[,c(1,4)]
y = unique(x)
y = na.omit(y)
y = subset(y, Releve_area < 2501)
z = as.data.frame(table(y$Releve_area))
hist(y$Releve_area, breaks = 330, xlab = "Plot size (m\u00B2)", ylab = "Frequency", main = "")

###############################################################
############ SUBSET: DATA TO DESIRED PLOT SIZES ##############
###############################################################

EU_abun_subset_100 <- subset(EU_abun, Releve_area == 100)
EU_abun_subset_400 <- subset(EU_abun, Releve_area == 400)
EU_abun_subset_1000 <- subset(EU_abun, Releve_area > 749 & Releve_area < 1251)
EU_abun_subset_2500 <- subset(EU_abun, Releve_area == 2500)
EU_abun_subset_all <- EU_abun
data_list = list(EU_abun_subset_100, EU_abun_subset_400, EU_abun_subset_1000, 
                    EU_abun_subset_2500, EU_abun_subset_all)

setwd("./Raw_data/Biodiversity/Trait_diversity/")
filenames = c("./100m_data/","./400m_data/", "./1000m_data/", "./2500m_data/", "./All_plot_data/") 
tags = c("_100m", "_400m", "_1000m", "_2500m", "all_plots")

# Create directories
for  (i in 1:length(filenames)){
  dir.create(filenames[i], showWarnings = FALSE)
}

#################################################################
################# CREATE ABUNDANCE & TRAIT MATRIX ###############
#################################################################

for (i in  1:length(data_list)) {
  Abun_data = as.data.frame(data_list[i])
  
  if (i < 5) {
    print(paste0("Computing ", Abun_data$Releve_area[i], "m plots"))}
  else {print("Computing all plots")}
  
  Abun_data = stats::aggregate(Abun_data$Relative_cover,
                                        by=list(Species=Abun_data$Species,
                                                PlotObservationID=Abun_data$PlotObservationID),
                          data=Abun_data,FUN=mean)
  Abun_data = Abun_data %>%
    dplyr::group_by(PlotObservationID) %>%
    dplyr::filter(mean(!Species %in% Trait_mean$Species) < 0.2)%>% 
    as.data.frame(Abun_data) %>%
    ungroup # Remove plots with 20% of species or more missing
  
  Abun_data = Abun_data[(Abun_data$Species %in% Trait_mean$Species),]
  Trait_hom = Trait_mean[(Trait_mean$Species %in% Abun_data$Species),] # Homogenise traits and abun based on species
  
  Trait_hom = Trait_hom[order(Trait_hom$Species), ]
  Abun_data = Abun_data[order(Abun_data$Species), ] # Alphabetical order
  
  Abun_matrix = stats::reshape(Abun_data, 
                               idvar = "PlotObservationID", timevar = "Species", 
                               direction = "wide") # Takes a while
  colnames(Abun_matrix) = sub("x.", "", colnames(Abun_matrix))
  
  Abun_matrix = Abun_matrix[order(Abun_matrix$PlotObservationID),]
  Abun_matrix$loc = 1:length(Abun_matrix[,1])
  Abun_matrix <- Abun_matrix %>%
    dplyr::select("loc", everything())
  Abun_matrix[is.na(Abun_matrix)] = 0 # Change NAs to 0s
  
  EU.oa <- header.oa %>%
    dplyr::filter(str_detect(Continent, "Europe"))
  x = Abun_matrix[,1:2]
  SPlot_Coords = EU.oa[c("PlotObservationID", "Latitude", "Longitude")]
  SPlot_Coords = SPlot_Coords[SPlot_Coords$PlotObservationID %in% Abun_matrix$PlotObservationID,]
  SPlot_Coords = merge(SPlot_Coords, x, by = "PlotObservationID")
  SPlot_Coords <- SPlot_Coords %>%
    dplyr::select("loc", everything())
  
  SPlot_Coords = dplyr::select(SPlot_Coords, -PlotObservationID)
  Abun_matrix <-within(Abun_matrix, rm(PlotObservationID))
  
  write.csv(Abun_matrix, paste0(filenames[i],"Abun_matrix_FD", tags[i],".csv"), row.names=FALSE)
  write.csv(SPlot_Coords, paste0(filenames[i],"Coords_FD", tags[i],".csv"), row.names=FALSE)
  write.csv(Trait_hom, paste0(filenames[i],"Selected_traits_FD", tags[i],".csv"), row.names=FALSE)
}