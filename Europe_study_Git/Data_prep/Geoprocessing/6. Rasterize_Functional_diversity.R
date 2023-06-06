# NOTE: rasters can be created here but are not stored on GITHUB due to storage limitations

# ----------------------- Libraries ------------------
rm(list=ls())
if("terra" %in% rownames(installed.packages()) == FALSE) {install.packages("terra")}
library(terra)

if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")}
library(sf)

if("mapview" %in% rownames(installed.packages()) == FALSE) {install.packages("mapview")}
library(mapview)

if("stars" %in% rownames(installed.packages()) == FALSE) {install.packages("stars")}
library(stars)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

# ----------------------- Woring directory -----------
setwd("\Europe_study_Git\Data_prep\Biodiversity_data/100m_plots_only/")
#setwd("\Europe_study_Git\Data_prep\Biodiversity_data/400m_plots_only/")
#setwd("\Europe_study_Git\Data_prep\Biodiversity_data/1km_plots_only/")          ########################

# ----------------------- Data -----------------------

results = read.csv("./results_Full_100m_plots.csv")
#results = read.csv("./results_Full_400m_plots.csv")
#results = read.csv("./results_Full_1km_plots.csv")                           #########################


results =as.data.frame(results)
results <- results %>%
  dplyr::select(c("Longitude","Latitude"), everything())
# ----------------------- Join data frames -----------

#FD_complete <- merge(results, coords, by = "location")
# FD_complete[FD_complete == "NaN"] = NA
# FD_complete = na.omit(FD_complete)

#  ------------------- CREATE POINT DATAFRAME -----------------------
my.sf.point <- sf::st_as_sf(x = results, 
                        coords = c("Longitude", "Latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

sf_point = sf::st_transform(my.sf.point, crs = "EPSG:3035")

points = terra::vect(sf_point)
#mapview(sf_point)
# ---------------------- RASTERISE ---------------------

Proj_rast = terra::rast("BIO1_Annual_Mean_Temperature.tif")

bio_label = colnames(results[4:14])
for (i in seq_along(1:length(bio_label))) {
  FD_raster = terra::rasterize(x = sf_point, y = Proj_rast, field = bio_label[i], fun = mean)
  names(FD_raster) = bio_label[i]
  path = paste0("\Europe_study_Git\Data_prep\Biodiversity_data\100m_plots_only/", bio_label[i], ".tif") ########################
  terra::writeRaster(FD_raster, filename = path, gdal = "TFW=YES", overwrite = T)
}
