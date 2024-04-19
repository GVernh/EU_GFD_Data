# WARNING: The following script will download SoilGrids rasters to the raw data directory.
# This will require approx. 90GB of hard drive space.

# ---------------------- libraries ------------------------------
#test
libs <- c(
  "XML","sf", "dplyr", "leaflet", "mapview","rgdal", "gdalUtils")

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

# URL & TAGS

sg_url="/vsicurl/https://files.isric.org/soilgrids/latest/data/"
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
WGS= '+proj=longlat +datum=WGS84 +no_defs' 
merc= '+proj=longlat +datum=WGS84 +no_defs' 
CRS("+init=epsg:4326") # used to get CRS codes from EPSG codes

# EU boundary
EU_boundary <- st_read("./Raw_data/clipped_EU.shp")
EUb_transform <- st_transform(EU_boundary, WGS)
EUb_transform
(EUbb <- st_bbox(EUb_transform))
ulx = EUbb$xmin # ul means upper left
uly = EUbb$ymax
lrx= EUbb$xmax # lr means lower right
lry = EUbb$ymin
(EU_coords <- c(ulx, uly, lrx, lry))

# Organic carbon stock
gdal_translate(paste0(sg_url,'ocs/ocs_0-30cm_mean.vrt'), #0-30cm
               "./Raw_data/Soil/EU_ocs1.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Clay content
gdal_translate(paste0(sg_url,'clay/clay_0-5cm_mean.vrt'), #0-5cm
               "./Raw_data/Soil/Clay_content/EU_clay_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'clay/clay_5-15cm_mean.vrt'), #5-15cm
               "./Raw_data/Soil/Clay_content/EU_clay_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'clay/clay_5-15cm_mean.vrt'), #15-30cm
               "./Raw_data/Soil/Clay_content/EU_clay_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Cation exchange capacity
gdal_translate(paste0(sg_url,'cec/cec_0-5cm_mean.vrt'), #0-5cm
               "./Raw_data/Soil/Cation_exchange_capacity/EU_cec_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'cec/cec_5-15cm_mean.vrt'), #5-15cm
               "./Raw_data/Soil/Cation_exchange_capacity/EU_cec_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'cec/cec_15-30cm_mean.vrt'), #15-30cm
               "./Raw_data/Soil/Cation_exchange_capacity/EU_cec_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Soil organic carbon content
gdal_translate(paste0(sg_url,'soc/soc_0-5cm_mean.vrt'), #0-5cm
               "./Raw_data/Soil/Soil_organic_carbon_content/EU_soc_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'soc/soc_5-15cm_mean.vrt'), #5-15cm
               "./Raw_data/Soil/Soil_organic_carbon_content/EU_soc_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'soc/soc_15-30cm_mean.vrt'), #15-30cm
               "./Raw_data/Soil/Soil_organic_carbon_content/EU_soc_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Coarse fragments volumetric
gdal_translate(paste0(sg_url,'cfvo/cfvo_0-5cm_mean.vrt'), #0-5cm
               "./Raw_data/Soil/Coarse_fragments_volumetric/EU_cfvo_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'cfvo/cfvo_5-15cm_mean.vrt'), #5-15cm
               "./Raw_data/Soil/Coarse_fragments_volumetric/EU_cfvo_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'cfvo/cfvo_15-30cm_mean.vrt'), #15-30cm
               "./Raw_data/Soil/Coarse_fragments_volumetric/EU_cfvo_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Nitrogen
gdal_translate(paste0(sg_url,'nitrogen/nitrogen_0-5cm_mean.vrt'), #0-5cm
               "./Raw_data/Soil/Nitrogen_content/EU_nitrogen_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'nitrogen/nitrogen_5-15cm_mean.vrt'), #5-15cm
               "./Raw_data/Soil/Nitrogen_content/EU_nitrogen_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'nitrogen/nitrogen_15-30cm_mean.vrt'), #15-30cm
               "./Raw_data/Soil/Nitrogen_content/EU_nitrogen_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Soil pH in H2O
gdal_translate(paste0(sg_url,'phh2o/phh2o_0-5cm_mean.vrt'), #0-5cm
               "./Raw_data/Soil/Soil_pH_in_H2O/EU_phh2o_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'phh2o/phh2o_5-15cm_mean.vrt'), #5-15cm
               "./Raw_data/Soil/Soil_pH_in_H2O/EU_phh2o_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'phh2o/phh2o_15-30cm_mean.vrt'), #15-30cm
               "./Raw_data/Soil/Soil_pH_in_H2O/EU_phh2o_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Sand content
gdal_translate(paste0(sg_url,'sand/sand_0-5cm_mean.vrt'), #0-5cm
               "./Raw_data/Soil/Sand_content/EU_sand_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'sand/sand_5-15cm_mean.vrt'), #5-15cm
               "./Raw_data/Soil/Sand_content/EU_sand_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'sand/sand_15-30cm_mean.vrt'), #15-30cm
               "./Raw_data/Soil/Sand_content/EU_sand_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Silt content
gdal_translate(paste0(sg_url,'silt/silt_0-5cm_mean.vrt'), #0-5cm
               "./Raw_data/Soil/Silt_content/EU_silt_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'silt/silt_5-15cm_mean.vrt'), #5-15cm
               "./Raw_data/Soil/Silt_content/EU_silt_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'silt/silt_15-30cm_mean.vrt'), #15-30cm
               "./Raw_data/Soil/Silt_content/EU_silt_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Organic carbon densities

gdal_translate(paste0(sg_url,'ocd/ocd_0-5cm_mean.vrt'), #0-5cm
               "./Raw_data/Soil/Organic_carbon_densities/EU_ocd_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'ocd/ocd_5-15cm_mean.vrt'), #5-15cm
               "./Raw_data/Soil/Organic_carbon_densities/EU_ocd_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'ocd/ocd_15-30cm_mean.vrt'), #15-30cm
               "./Raw_data/Soil/Organic_carbon_densities/EU_ocd_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)