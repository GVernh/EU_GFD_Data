# ---------------------- libraries ------------------------------
rm(list = ls())
if("tidyverse" %in% rownames(installed.packages()) == FALSE ) { install.packages("tidyverse")}
library(tidyverse)
if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")}
library(sf)
if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")}
library(raster)
if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages("devtools")}
library(devtools)
# devtools::install_github("ropenscilabs/rnaturalearthdata")
# ("rnaturalearthhires",
# repos = "http://packages.ropensci.org",
#  type = "source")
#install.packages("rnaturalearth")
library(rnaturalearth)
if("RefManageR" %in% rownames(installed.packages()) == FALSE) {install.packages("RefManageR")}
library(RefManageR)
if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")}
library(rgeos)
if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal")}
library(rgdal)
if("reshape2" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape2")}
library(reshape2)
if("utf8" %in% rownames(installed.packages()) == FALSE) {install.packages("utf8")}
library(utf8)
if("arsenal" %in% rownames(installed.packages()) == FALSE) {install.packages("arsenal")}
library(arsenal)
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
library(dplyr)

#---------------------- DATA --------------------------------------=----
load("D://Europe_study_data/SPlot/sPlotOpen.RData")

setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/")
Traits = read.csv("./Selected_trait_data.csv")
# --------------------- SPECIES ABUNDANCE ------------------------------
#Subset continent and ID for whole world
Loc_subset = header.oa[c("PlotObservationID", "Continent", "Date_of_recording", "Releve_area")]

#  ABUNDANCE DATA PREP
EU_abun <- merge(DT2.oa, Loc_subset, by = "PlotObservationID")

# Eu_test = EU_abun_subset[1:10, 1:3]
# x = reshape(Eu_test, idvar = "PlotObservationID", timevar = "Species", direction = "wide")


EU_abun <- EU_abun %>%
  filter(str_detect(Continent, "Europe"))

EU_abun_subset = EU_abun[c("PlotObservationID", "Species", "Relative_cover", "Releve_area")]
EU_abun_subset$Species <- gsub("_", " ", EU_abun_subset$Species)
EU_abun_subset %>%
  mutate(Species= as_utf8(Species))->EU_abun_subset


# ----------------- limit to by plot size? -------------------

# Show frequency of plot sizes (m2)
x = EU_abun_subset[,c(1,4)]
y = unique(x)
z = as.data.frame(table(y$Releve_area)) 

EU_abun_subset <- subset(EU_abun_subset, Releve_area == 1)
# EU_abun_subset <- subset(EU_abun_subset, Releve_area == 100)
# EU_abun_subset <- subset(EU_abun_subset, Releve_area == 400)                      ##############################
# EU_abun_subset = subset(EU_abun_subset, Releve_area > 749 & Releve_area < 1251)

#----------------------------- KEW CHECKLIST -----------------------------
sp_checklist = read.table("D:/Europe_study_data/Kew_Checklist/wcvp_names.txt", sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8") 
dist_checklist = read.table("D:/Europe_study_data/Kew_Checklist/wcvp_distribution.txt", sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

continents = dist_checklist[c("plant_name_id", "continent")]
sp_checklist_merge <- merge(sp_checklist, continents, by = "plant_name_id")

EU_checklist_merge_EU <- sp_checklist_merge %>%
  filter(str_detect(continent, "EUROPE"))

EU_abun_subset = EU_abun_subset[(EU_abun_subset$Species %in% EU_checklist_merge_EU$taxon_name),] # Remove non-kew species

# ---------------------------- CREATE ABUNDANCE & TRAIT MATRIX --------------------

EU_abun_subset = aggregate(EU_abun_subset$Relative_cover,
                 by=list(Species=EU_abun_subset$Species,
                         PlotObservationID=EU_abun_subset$PlotObservationID),data=EU_abun_subset,FUN=mean) # Some rows are identical, average them

Traits$Spp<-gsub("[.]", " ",Traits$Spp)
Traits = Traits[3:10]
Trait_mean = stats::aggregate.data.frame(Traits[2:8], 
                                         by= list(Traits$Spp), FUN = mean, na.rm = TRUE)
names(Trait_mean)[1] <- "Species"

EU_abun_subset = EU_abun_subset %>% 
  group_by(PlotObservationID) %>%
  filter(mean(!Species %in% Trait_mean$Species) < 0.2)%>% 
  ungroup # Remove plots with 20% of species or more missing

EU_abun_subset = as.data.frame(EU_abun_subset)

EU_abun_subset = EU_abun_subset[(EU_abun_subset$Species %in% Trait_mean$Species),]
Trait_mean = Trait_mean[(Trait_mean$Species %in% EU_abun_subset$Species),] # Homogenise traits and abun based on species

Trait_mean = Trait_mean[order(Trait_mean$Species), ]
EU_abun_subset = EU_abun_subset[order(EU_abun_subset$Species), ] # Alphabetical order

Abun_matrix = reshape(EU_abun_subset, idvar = "PlotObservationID", timevar = "Species", direction = "wide") # Takes a while

Abun_matrix = Abun_matrix[order(Abun_matrix$PlotObservationID),]
Abun_matrix$loc = 1:length(Abun_matrix[,1])
Abun_matrix <- Abun_matrix %>%
  dplyr::select("loc", everything())
Abun_matrix[is.na(Abun_matrix)] = 0 # Change NAs to 0s

# ------------------------------ ALIGN COORDINATE DATA --------------------------

EU.oa <- header.oa %>%
  filter(str_detect(Continent, "Europe"))
x = Abun_matrix[,1:2]
SPlot_Coords = EU.oa[c("PlotObservationID", "Latitude", "Longitude")]
SPlot_Coords = SPlot_Coords[SPlot_Coords$PlotObservationID %in% Abun_matrix$PlotObservationID,]
SPlot_Coords = merge(SPlot_Coords, x, by = "PlotObservationID")
SPlot_Coords <- SPlot_Coords %>%
  dplyr::select("loc", everything())

# CLEAN DATA FOR FD
SPlot_Coords = dplyr::select(SPlot_Coords, -PlotObservationID)
Abun_matrix <-within(Abun_matrix, rm(PlotObservationID))

setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/1m_plots_only/")
# setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/100m_plots_only/")
# setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/400m_plots_only/")
# setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/1km_plots_only/")     #############################
#setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/")
write.csv(Abun_matrix,"./Abun_matrix_FD.csv", row.names= F)
write.csv(SPlot_Coords,"./Coords_FD.csv", row.names= F)
write.csv(Trait_mean, "./Selected_traits_FD.csv", row.names= F)


#Species list for TRY -----
Sp_list= EU_abun_subset["Species"]
Sp_list= unique(Sp_list)
Sp_list = as.data.frame(Sp_list[(Sp_list$Species %in% EU_checklist_merge_EU$taxon_name),])
colnames(Sp_list) = "Species"
write.csv(Sp_list, "./Sp_list.csv", row.names= F)

# -------------- Show number of species with trait data ------------------
Traits = read.csv("D:/Europe_study_data/R code for spatial biodiv/example_data/filledXBackTrans_Spp.csv")
Traits$Spp<-gsub("[.]", " ",Traits$Spp)
T_spp = Traits["Spp"]
colnames(T_spp) = "Species"
T_spp = unique(T_spp)
How_many_species_traits <- merge(Sp_list, T_spp, by = "Species")
