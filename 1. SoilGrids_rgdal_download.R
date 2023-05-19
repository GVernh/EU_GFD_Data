#install.packages('XML')
library(XML)
#install.packages('sf')
library(sf)
#install.packages('dplyr')
library(dplyr)
#install.packages("leaflet")
library(leaflet)
#install.packages("mapview")
library(mapview)
#library(devtools)
library(rgdal)
#install.packages("gdalUtils")
library(gdalUtils)

sg_url="/vsicurl/https://files.isric.org/soilgrids/latest/data/"
setwd("D:/Europe_study_data/Soil/Europe/")
bb=c(-337500.000,1242500.000,152500.000,527500.000) # Example bounding box (homolosine) for Ghana
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
WGS= '+proj=longlat +datum=WGS84 +no_defs' 
merc= '+proj=longlat +datum=WGS84 +no_defs' 
CRS("+init=epsg:4326") # used to get CRS codes from EPSG codes


# Test code taken from SOilgrids website for Ghana
# gdal_translate(paste0(sg_url,'ocs/ocs_0-30cm_mean.vrt'),
    #           "./ocs_ghana.tif",
    #           tr=c(250,250),
    #           projwin=bb,
    #           projwin_srs =igh,
    #           verbose=TRUE)


#merc test

merc_boundary <- st_read("../Europe/Merc_bb.shp")
mercb <- st_transform(merc_boundary, merc)
(mercbb <- st_bbox(mercb))
ulx = mercbb$xmin # ul means upper left
uly = mercbb$ymax
lrx= mercbb$xmax # lr means lower right
lry = mercbb$ymin
(Mbb <- c(ulx, uly, lrx, lry))


gdal_translate(paste0(sg_url,'ocs/ocs_0-30cm_mean.vrt'),
               "./EU_ocs2.tif",
               tr=c(250,250),
               projwin_srs=merc,
               projwin = Mbb,
               verbose = T)

# Specify boundary
EU_boundary <- st_read("../Europe/EU_boundary.shp")
EU_igh <- st_transform(EU_boundary, igh)
(EUbbox <- st_bbox(EU_igh))

ulx = EUbbox$xmin # ul means upper left
uly = EUbbox$ymax
lrx= EUbbox$xmax # lr means lower right
lry = EUbbox$ymin
(EUbb <- c(ulx, uly, lrx, lry))



# EU
EU_boundary <- st_read("../Europe/EU_boundary.shp")
EUb_transform <- st_transform(EU_boundary, WGS)
EUb_transform
(EUbb <- st_bbox(EUb_transform))
ulx = EUbb$xmin # ul means upper left
uly = EUbb$ymax
lrx= EUbb$xmax # lr means lower right
lry = EUbb$ymin
(EU_coords <- c(ulx, uly, lrx, lry))

# Organic carbon stock -DONE
gdal_translate(paste0(sg_url,'ocs/ocs_0-30cm_mean.vrt'), #0-30cm
               "./EU_ocs1.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Clay content -DONE
gdal_translate(paste0(sg_url,'clay/clay_0-5cm_mean.vrt'), #0-5cm
               "./Clay_content/EU_clay_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'clay/clay_5-15cm_mean.vrt'), #5-15cm
               "./Clay_content/EU_clay_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'clay/clay_5-15cm_mean.vrt'), #15-30cm
               "./Clay_content/EU_clay_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Cation exchange capacity - DONE

gdal_translate(paste0(sg_url,'cec/cec_0-5cm_mean.vrt'), #0-5cm
               "./Cation_exchange_capacity/EU_cec_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'cec/cec_5-15cm_mean.vrt'), #5-15cm
               "./Cation_exchange_capacity/EU_cec_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'cec/cec_15-30cm_mean.vrt'), #15-30cm
               "./Cation_exchange_capacity/EU_cec_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Soil organic carbon content - DONE

gdal_translate(paste0(sg_url,'soc/soc_0-5cm_mean.vrt'), #0-5cm
               "./Soil_organic_carbon_content/EU_soc_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'soc/soc_5-15cm_mean.vrt'), #5-15cm
               "./Soil_organic_carbon_content/EU_soc_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'soc/soc_15-30cm_mean.vrt'), #15-30cm
               "./Soil_organic_carbon_content/EU_soc_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Coarse fragments volumetric

gdal_translate(paste0(sg_url,'cfvo/cfvo_0-5cm_mean.vrt'), #0-5cm
               "./Coarse_fragments_volumetric/EU_cfvo_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'cfvo/cfvo_5-15cm_mean.vrt'), #5-15cm
               "./Coarse_fragments_volumetric/EU_cfvo_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'cfvo/cfvo_15-30cm_mean.vrt'), #15-30cm
               "./Coarse_fragments_volumetric/EU_cfvo_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Nitrogen
gdal_translate(paste0(sg_url,'nitrogen/nitrogen_0-5cm_mean.vrt'), #0-5cm
               "./Nitrogen_content/EU_nitrogen_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'nitrogen/nitrogen_5-15cm_mean.vrt'), #5-15cm
               "./Nitrogen_content/EU_nitrogen_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'nitrogen/nitrogen_15-30cm_mean.vrt'), #15-30cm
               "./Nitrogen_content/EU_nitrogen_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Soil pH in H2O
gdal_translate(paste0(sg_url,'phh2o/phh2o_0-5cm_mean.vrt'), #0-5cm
               "./Soil_pH_in_H2O/EU_phh2o_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'phh2o/phh2o_5-15cm_mean.vrt'), #5-15cm
               "./Soil_pH_in_H2O/EU_phh2o_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'phh2o/phh2o_15-30cm_mean.vrt'), #15-30cm
               "./Soil_pH_in_H2O/EU_phh2o_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Sand content

gdal_translate(paste0(sg_url,'sand/sand_0-5cm_mean.vrt'), #0-5cm
               "./Sand_content/EU_sand_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'sand/sand_5-15cm_mean.vrt'), #5-15cm
               "./Sand_content/EU_sand_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'sand/sand_15-30cm_mean.vrt'), #15-30cm
               "./Sand_content/EU_sand_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Silt content

gdal_translate(paste0(sg_url,'silt/silt_0-5cm_mean.vrt'), #0-5cm
               "./Silt_content/EU_silt_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'silt/silt_5-15cm_mean.vrt'), #5-15cm
               "./Silt_content/EU_silt_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'silt/silt_15-30cm_mean.vrt'), #15-30cm
               "./Silt_content/EU_silt_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

# Organic carbon densities

gdal_translate(paste0(sg_url,'ocd/ocd_0-5cm_mean.vrt'), #0-5cm
               "./Organic_carbon_densities/EU_ocd_0_5.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'ocd/ocd_5-15cm_mean.vrt'), #5-15cm
               "./Organic_carbon_densities/EU_ocd_5_15.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)

gdal_translate(paste0(sg_url,'ocd/ocd_15-30cm_mean.vrt'), #15-30cm
               "./Organic_carbon_densities/EU_ocd_15_30.tif",
               tr=c(250,250),
               projwin_srs=WGS,
               projwin = EU_coords,
               verbose = T)