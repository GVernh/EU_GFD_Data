# Digital elevation data accessed: https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1.1
# SoilGrids data accessed using rGdal: SoilGrids_rgdal_download.R 

# ----------------- Packages ----------------------------
if("terra" %in% rownames(installed.packages()) == FALSE) {install.packages("terra")}
library(terra)
if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")}
library(sf)
if("foreach" %in% rownames(installed.packages()) == FALSE) {install.packages("foreach")}
library(foreach)

setwd("D:/Europe_study_data/")

# --------- Create a raster to project all data to ---------------
boundary = sf::st_read("\Europe_study_Git\Data_prep\Geoprocessing\Boundary_polygon.shp")
ext = ext(boundary) # Extent of interest
Proj_rast = terra::rast(res = 250, crs = "EPSG:3035", ext = ext)

# ------------- AVERAGE SOIL DATA ------------
soil_0_5 = list.files(path = "./0_5/", # Specify directory of 0-5cm depth soil data downloaded using SoilGrids_rgdal_download.R 
                      pattern = ".tif$",
                      recursive = F,
                      full.name = T)
soil_5_15 = list.files(path = "./5_15/", # Specify directory of 5-15cm depth soil data downloaded using SoilGrids_rgdal_download.R 
                      pattern = ".tif$",
                      recursive = F,
                      full.name = T)

 
foreach(i = seq_along(1:length(soil_0_5)), .packages= "terra") %dopar% {
  print(i)
  soil_stack = terra::rast(soil_0_5[i], soil_5_15[i])
  mean_soil <- terra::app(soil_stack, fun = mean, na.rm = T)
  soil_250m = terra::project(mean_soil, Proj_rast, method = "bilinear", threads = T)
  soil_250m = round(soil_250m, digits = 0)
  x = names(soil_stack[[1]])
  names(soil_250m) = x
  path= paste0("./Projected_rasters/Geodiversity/", substr(x, 1, 6) , "_250m.tif") # Create folder for geodiversity layers in desired directory
  terra::writeRaster(x = soil_250m, filename = path, gdal='TFW=YES', overwrite =T)
}

# Nit and Soc are 4 digit rasters so we need to reduce to 3 for Rao
nit = terra::rast("./Projected_rasters/Geodiversity/EU_nit_250m.tif")
soc = terra::rast("./Projected_rasters/Geodiversity/EU_soc_250m.tif")
nit = nit/10
nit = round(nit, digits = 0)
soc = soc/10
soc = round(soc, digits = 0)
terra::writeRaster(x = soc, filename = "./Projected_rasters/Geodiversity/EU_soc_250m.tif", 
                   gdal='TFW=YES', overwrite =T)
terra::writeRaster(x = nit, filename = "./Projected_rasters/Geodiversity/EU_nit_250m.tif", 
                   gdal='TFW=YES', overwrite =T)

# ----------------- REPROJ DEM -------------------
DEM = terra::rast("./DEM_25.tif") # Specify directory where DEM is located. Downloaded from: https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1.1
DEM_250m = terra::aggregate(DEM, fact = 10, fun  = "mean")
DEM_250m = terra::project(DEM_250m, Proj_rast, method = "bilinear", threads = T)
DEM_250m = DEM_250m/10
DEM_250m = round(DEM_250m, digits = 0)
names(DEM_250m) = "DEM_250"
terra::writeRaster(x = DEM_250m, filename = "./Projected_rasters/Geodiversity/DEM_250m.tif", gdal = "TFW=YES", 
                   overwrite = T)
rm(DEM, DEM_250m)

# NOTE: Below layers created by inputting above DEM into GRASS GIS 
# r.geomorphon for landform
# r.watershed for TWI

# ----------------- CROP LANDFORM -------------
# Data is already in correct projection, res and extent. Just needs to have a column cropped
landform = terra::rast("./Landform_250m.tif")
landform_250m = terra:: crop(landform, Proj_rast)
terra::writeRaster(x = landform_250m, filename = "./Projected_rasters/Geodiversity/Discrete/landform_250m.tif", 
            gdal = "TFW=YES", overwrite = T)
rm(landform, landform_250m)

# ---------------- REPROJ TOPOGRAPHIC WETNESS ------------------

TWI = terra::rast("./TWI_125.tif")
TWI_250m = terra::aggregate(TWI, fact = 2, fun  = "mean")
TWI_250m = terra::project(TWI_250m, Proj_rast, method = "bilinear", threads = T)
TWI_250m = round(TWI_250m, digits = 1)
names(TWI_250m) = "TWI_250"
terra::writeRaster(x = TWI_250m, filename ="./Projected_rasters/Geodiversity/TWI_250m.tif", 
                   gdal = "TFW=YES", overwrite = T)
rm(TWI, TWI_250m)
