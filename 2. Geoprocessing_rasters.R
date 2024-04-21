# Digital elevation data accessed: https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1.1
# SoilGrids data accessed from SoilGrids 2.0 using 1.SoilGrids_rgdal_download.R

# ----------------- LIBRARIES ----------------------------
libs <- c(
  "sf","terra", "foreach")

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


# --------- Create a raster to project all data to ---------------
boundary = sf::st_read("./Raw_data/clipped_EU.shp")
ext = ext(boundary) # Extent of interest
Proj_rast = terra::rast(res = 250, crs = "EPSG:3035", ext = ext)

# ------------- AVERAGE SOIL DATA ------------
soil_0_5 = list.files(path = "./Raw data/Soil/0_5/",
                      pattern = ".tif$",
                      recursive = F,
                      full.name = T)
soil_5_15 = list.files(path = "./Raw data/Soil/5_15/",
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
  path= paste0("./Processed_rasters/Terrain_soil/", substr(x, 1, 6) , "_250m.tif")
  terra::writeRaster(x = soil_250m, filename = path, gdal='TFW=YES', overwrite =T)
}

# Nit and Soc are 4 digit rasters so we need to reduce to 3 to reduce computational requirements of rasterdiv
nit = terra::rast("./Processed_rasters/Terrain_soil/EU_nit_250m.tif")
soc = terra::rast("./Processed_rasters/Terrain_soil/EU_soc_250m.tif")
nit = nit/10
nit = round(nit, digits = 0)
soc = soc/10
soc = round(soc, digits = 0)
terra::writeRaster(x = soc, filename = "./Processed_rasters/Terrain_soil/EU_soc_250m.tif", 
                   gdal='TFW=YES', overwrite =T)
terra::writeRaster(x = nit, filename = "./Processed_rasters/Terrain_soil/EU_nit_250m.tif", 
                   gdal='TFW=YES', overwrite =T)

# ----------------- REPROJ DEM -------------------
DEM = terra::rast("./Raw data/DEM/DEM_25.tif")
# DEM_125m = terra::aggregate(DEM, fact = 5, fun  = "mean") # For TWI at 125m res
DEM_250m = terra::aggregate(DEM, fact = 10, fun  = "mean")
DEM_250m = terra::project(DEM_250m, Proj_rast, method = "bilinear", threads = T)
# DEM_125m = terra::project(DEM_125m, Proj_rast, method = "bilinear", threads = T)
DEM_250m = DEM_250m/10
DEM_250m = round(DEM_250m, digits = 0)
names(DEM_250m) = "DEM_250"
terra::writeRaster(x = DEM_250m, filename = "./Processed_rasters/Terrain_soil/DEM_250m.tif", gdal = "TFW=YES", 
                   overwrite = T)
rm(DEM, DEM_250m)

# ----------------- CROP LANDFORM -------------
# Data is already in correct projection, res and extent. Just needs to have a column cropped
landform = terra::rast("./Raw data/Landform/Landform_250m.tif")
landform_250m = terra:: crop(landform, Proj_rast)
terra::writeRaster(x = landform_250m, filename = "./Processed_rasters/Discrete/landform_250m.tif", 
            gdal = "TFW=YES", overwrite = T)
rm(landform, landform_250m)

# ---------------- REPROJ TOPOGRAPHIC WETNESS ------------------

TWI = terra::rast("./Raw data/TWI/TWI_125.tif")
TWI_250m = terra::aggregate(TWI, fact = 2, fun  = "mean")
TWI_250m = terra::project(TWI_250m, Proj_rast, method = "bilinear", threads = T)
TWI_250m = round(TWI_250m, digits = 1)
names(TWI_250m) = "TWI_250"
terra::writeRaster(x = TWI_250m, filename ="./Processed_rasters/Terrain_soil/TWI_250m.tif", 
                   gdal = "TFW=YES", overwrite = T)
rm(TWI, TWI_250m)