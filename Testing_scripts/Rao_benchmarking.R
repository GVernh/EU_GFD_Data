library(raster)
library(rasterdiv)
# install.packages("profmem")
library(profmem)
library(MASS)
#install.packages("tseries")
library(tseries)

setwd("D:/Europe_study_data/Projected_rasters/")
soil_ph = raster::raster("./EU_phh2o_5_15.tif")
temo = raster::raster("./BIO12_Annual_Precipitation.tif")
boundary = rgdal::readOGR(dsn="D:/Europe_study_data/", layer= "TREE_area_of_interest")    
soil_ph_crop = raster::crop(x = soil_ph, y = boundary)
soil_ph_crop = raster::aggregate(soil_ph_crop, fact = 3)


temo
head(temo)
start.time = proc.time()
ph_rao_agg = rasterdiv::paRao(x = mat,area=NULL, field=NULL, window = 3, na.tolerance = 0.7, np=1, #141 sec
                          alpha = 1, simplify = 1,dist_m="euclidean",
                          cluster.type="SOCK", debugging=FALSE, rasterOut=TRUE, method="classic")
stop.time = proc.time()
stop.time - start.time

start.time = proc.time()
ph_rao_agg = rasterdiv::CRE(x = soil_ph_crop, window = 3, na.tolerance = 0.7, np=1, # 3 sec
                              simplify = 3,
                              cluster.type="SOCK", debugging=FALSE, rasterOut=TRUE)
stop.time = proc.time()
stop.time - start.time

plot(ph_rao_agg)

start.time = proc.time()
ph_rao_agg = rasterdiv::Rao(x = soil_ph_crop, window = 3, na.tolerance = 0.7, np=1,  #155 sec
                              simplify = 1,dist_m="euclidean",
                              cluster.type="SOCK", debugging=FALSE, rasterOut=TRUE)
stop.time = proc.time()
stop.time - start.time
