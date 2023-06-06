# Computing heterogeneity from Rao Q is computationally intensive. 
# Parallel processing using high powered computing clusters may be required.


if ("rasterdiv" %in% rownames(installed.packages()) == F) {install.packages("rasterdiv")}
library(rasterdiv)

setwd("/Projected_rasters/Geodiversity/") # Specify directory where downloaded geodiversity layers are located


# -------------- Rao Quadratic Entropy ---------------
# Example provided here for soil pH
soil = raster("./EU_phh_250m.tif")

r1 = rasterdiv::paRao(x = soil,area=NULL, field=NULL, window = 5, na.tolerance = 0.6, np=1, 
                      alpha = 1,simplify = 0, dist_m="euclidean",
                      cluster.type="SOCK", debugging=FALSE, rasterOut=TRUE, method="classic")

names(r1$window.5$alpha.1) = names(soil)

pH_rao = terra::rast(r1$EU_phh2o_0_5$alpha.1)
terra::writeRaster(pH_rao, filename = "../Analysis/250M_5X5_HET/EU_ph_rao.tif", gdal = "TFW=YES", overwrite = T)


# ------------- Landform heterogeneity (Shannons Entropy) -----------------

LF = raster::raster("./landform_250m.tif")

LF_sha <- rasterdiv::Shannon(x = LF, window = 5, na.tolerance = 0.6, np = 1)

raster::writeRaster(LF_sha, filename = "./Analysis/250M_5X5_HET/Landform_Sha.tif", options = c("TFW=YES"), overwrite = T)
