# Computing heterogeneity from Rao Q is computationally intensive.
# The time required is dependent on the number of digits within the raster and the data size (ncol x nrow)
# On a standalone computer you can compute Rao Q for all of Europe at 250m providing the data is only 2 digits
# Any more digits (3-4) will require a large number of processors on HPC
# All data in the "projected_rasters/Geodiversity" folder has been projected with identical dimensions to 
  # 250m resolution and rounded to 0 decimals.

if ("rasterdiv" %in% rownames(installed.packages()) == F) {install.packages("rasterdiv")}
library(rasterdiv)

setwd("D:/Europe_study_data/Projected_rasters/")


# -------------- Rao Quadratic Entropy ---------------
soil = raster("./Geodiversity/EU_soc_250m.tif")

r1 = rasterdiv::paRao(x = soil,area=NULL, field=NULL, window = 5, na.tolerance = 0.6, np=1, 
                      alpha = 1,simplify = 0, dist_m="euclidean",
                      cluster.type="SOCK", debugging=FALSE, rasterOut=TRUE, method="classic")

names(r1$window.5$alpha.1) = names(soil)

pH_rao = terra::rast(r1$EU_phh2o_0_5$alpha.1)
terra::writeRaster(pH_rao, filename = "../Analysis/250M_5X5_HET/EU_ph_rao.tif", gdal = "TFW=YES", overwrite = T)


# ------------- Landform heterogeneity (Shannons Entropy) -----------------

LF = raster::raster("./Projected_rasters/Geodiversity/Discrete/landform_250m.tif")

LF_sha <- rasterdiv::Shannon(x = LF, window = 5, na.tolerance = 0.6, np = 1)

raster::writeRaster(LF_sha, filename = "./Analysis/250M_5X5_HET/Landform_Sha.tif", options = c("TFW=YES"), overwrite = T)
