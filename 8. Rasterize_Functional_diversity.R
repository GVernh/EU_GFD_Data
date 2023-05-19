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
setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/1m_plots_only/")
#setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/100m_plots_only/")
#setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/400m_plots_only/")
#setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/1km_plots_only/")          ########################
# setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/FD_results/")

# ----------------------- Data -----------------------
results = read.csv("./results_Full_1m_plots.csv")
#results = read.csv("./results_Full_100m_plots.csv")
#results = read.csv("./results_Full_400m_plots.csv")
#results = read.csv("./results_Full_1km_plots.csv")                           #########################
#results = read.csv("./results_Full.csv")


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

Proj_rast = terra::rast("D:/Europe_study_data/Analysis/Finalised_rasters/BIO1_Annual_Mean_Temperature.tif")

bio_label = colnames(results[4:14])
for (i in seq_along(1:length(bio_label))) {
  FD_raster = terra::rasterize(x = sf_point, y = Proj_rast, field = bio_label[i], fun = mean)
  names(FD_raster) = bio_label[i]
  path = paste0("D:/Europe_study_data/Analysis/Finalised_rasters/Functional_diversity/1m_plot_only/", bio_label[i], ".tif") ########################
  terra::writeRaster(FD_raster, filename = path, gdal = "TFW=YES", overwrite = T)
}


# ---------------- Number of cells with values ----------------------

# NEEDS CORRECTING
# unique(rast)

#FD_raster = raster::raster(FD_raster)
# y = na.omit(getValues(FD_raster)) 

# ------------- create point file for all 100 and 400m plots (map for manuscript) -----------------

setwd("D:/Europe_study_data/SPlot/Complete_biodiversity_data/")
plot_400 = read.csv("./400m_plots_only/results_Full_400m_plots.csv")                      
plot_100 = read.csv("./100m_plots_only/results_Full_100m_plots.csv")
plot_1km =read.csv("./1km_plots_only/results_Full_1km_plots.csv")

boundary = sf::st_read("D:/Europe_study_data/Raw data/Boundary_polygon.shp")
ext = ext(boundary) # Extent of interest
Proj_rast = terra::rast(res = 1000, crs = "EPSG:3035", ext = ext)

plot_100 = subset(plot_100, select = -location)
plot_400 = subset(plot_400, select = -location)
plot_1km = subset(plot_1km, select = -location)

plot_100$area = 100
plot_400$area = 400
plot_1km$area = 1000

plots_100_400 = rbind(plot_400, plot_100)
all_plots = rbind(plot_400, plot_100, plot_1km)

sf.point_100_400 <- sf::st_as_sf(x = plots_100_400, 
                             coords = c("Longitude", "Latitude"),
                             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

sf.point_all <- sf::st_as_sf(x = all_plots, 
                            coords = c("Longitude", "Latitude"),
                            crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

sf.point_100_400 = sf::st_transform(plots_100_400, crs = "EPSG:3035")
sf.point_all = sf::st_transform(sf.point_all, crs = "EPSG:3035")

crop_100_400 = sf::st_crop(sf.point_100_400, ext)
crop_all = sf::st_crop(sf.point_all, ext)

st_write(crop_100_400, "../../Layout maps/100_400_plots.shp" ,driver = "ESRI Shapefile", delete_layer = TRUE)
st_write(crop_all, "../../Layout maps/100_400_1km_plots.shp" ,driver = "ESRI Shapefile", delete_layer = TRUE)
