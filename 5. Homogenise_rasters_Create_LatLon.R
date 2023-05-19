
if("terra" %in% rownames(installed.packages()) == FALSE) {install.packages("terra")}
library(terra)
if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")}
library(sf)
if("foreach" %in% rownames(installed.packages()) == FALSE) {install.packages("foreach")}
library(foreach)
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

# ---------------- Aggregate geodiversity --------------------------
# NOTE: The soil layers were not renamed and so have 0_5 in there raster names but it is actually 0_15 as they were 
  # averaged across depths.
setwd("D:/Europe_study_data/Analysis/250M_5X5_HET/")

geo_rasters = list.files(path = "./",
                        pattern = ".tif$",
                        full.names = T,
                        recursive = F)

foreach(i = seq_along(1:length(geo_rasters)), .packages = "terra") %dopar% {
  rast = terra::rast(geo_rasters[i])
  r1 = terra::aggregate(rast, fact = 4, fun = "mean")
  names(r1) = paste0(names(rast), "_Rao")
  path = paste0("../Finalised_rasters/", names(r1), ".tif")
  terra::writeRaster(x = r1, filename = path, overwrite = T, gdal = "TFW=YES")
}

rm(list=ls())


# ----------------- Project Climate/FD/Landuse --------------------

setwd("D:/Europe_study_data/")

FD_rasters = list.files(path = "./Raw data/Functional_diversity/",
                        pattern =".tif$",
                        full.names = T,
                        recursive = F)

LU = list.files(path= "./Raw data/Land_use/",
                pattern = ".tif$",
                recursive = F,
                full.names = T)

clim_rast = list.files(path = "./Raw data/Climate/",
                          pattern = ".tif$",
                          recursive = F,
                          full.names = T)

clim_LU_FD_rast = c(clim_rast, LU, FD_rasters)
rm(LU, clim_rast, FD_rasters)

Top_rao = rast("./Analysis/Finalised_rasters/DEM_250_Rao.tif")

foreach(i = seq_along(1:length(clim_LU_FD_rast)), .packages = "terra") %dopar% {
  print(i)
  rast = terra::rast(clim_LU_FD_rast[i])
  if(names(rast) == "EU_landSystem") {
      r1 = terra::project(x = rast, y = Top_rao, method = "near", threads = T)
        } else r1 = terra::project(x = rast, y = Top_rao, method = "bilinear", threads = T)
  names(r1) = names(rast)
  path = paste0("./Analysis/Finalised_rasters/", names(r1), ".tif")
  terra::writeRaster(x = r1, filename = path, gdal = "TFW=YES", overwrite = T)
}

# ---------------------- LAND USE raster by names -----------------------

rast = terra::rast("D:/Europe_study_data/Analysis/Finalised_rasters/EU_landSystem.tif")

rast[rast == 0] <- NA

rast = as.factor(rast)
rast

land_classes = levels(rast) [[1]]
names(land_classes)[names(land_classes) == "label"] <- "Land_system_names"

land_classes$Land_system_names = c("water body",
                       "wetland",
                       "glacier",
                       "low-intensity settlement",
                       "medium intensity settlement",
                       "high intensity settlement",
                       "extensive perm-crops",
                       "intensive perm-crops",
                       "low-intensity forest",
                       "medium-intensity forest",
                       "high-intensity forest",
                       "low-intensity grassland",
                       "medium-intensity grassland",
                       "high-intensity grassland",
                       "low-intensity cropland",
                       "medium-intensity cropland",
                       "high-intensity cropland",
                       "forest/shrubs and cropland mosaics",
                       "forest/shrubs and grassland mosaics",
                       "forest/shrubs and bare mosaics",
                       "forest/shrubs and mixed agriculture mosaics",
                       "shrub",
                       "bare and rocks",
                       "low-intensity agricultural mosaics",
                       "medium-intensity agricultural mosaics",
                       "high-intensity agricultural mosaics")

levels(rast) = land_classes

plot(rast)
rast

terra::writeRaster(rast, filename = "./Analysis/Finalised_rasters/EU_Landsystem_by_names.tif", gdal = "TFW=YES", overwrite = T)
rast("D:/Europe_study_data/Analysis/Finalised_rasters/EU_Landsystem_by_names.tif")


# ------------- CREATE LONG/LAT RASTER ------------------

Proj_rast = rast(ext = Top_rao, crs = Top_rao, res = 1000)

rlon<-rlat<-Proj_rast
xy<-terra::xyFromCell(Proj_rast,1:terra::ncell(Proj_rast))
rlon[]<-xy[,1]
rlat[]<-xy[,2]

#par(mfrow=c(1,2))
#image(rlon,main="longitudes")
#image(rlat,main="latitudes")

names(rlon)= "lon"
names(rlat)= "lat"

terra::writeRaster(rlon, filename = "D:/Europe_study_data/Analysis/Finalised_rasters/longitude.tif", gdal = "TFW=YES",
                   overwrite = TRUE)
terra::writeRaster(rlat, filename = "D:/Europe_study_data/Analysis/Finalised_rasters/latitude.tif", gdal = "TFW=YES",
                   overwrite = TRUE)
