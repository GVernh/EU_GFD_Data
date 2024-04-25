# Directory for libraries on HPC
.libPaths("./R/linux_lib/4.0")

# Libraries
libs <- c(
  "raster","sf", "dplyr", "rasterdiv")

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

# Load rasters
rasters = list.files(path = "./Processed_rasters/Terrain_soil/",
                     pattern = "tif$",
                     full.names = T,
                     recursive = F)

# Loop through rasters and compute geodiversity

for(i in seq_along(rasters)) {
  print(i)
  rast = raster::raster(rasters[i])
  r1 = rasterdiv::paRao(x = rast_round,area=NULL, field=NULL, window = 5, na.tolerance = 0.6, np=30, 
                        alpha = 1, simplify = 3,dist_m="euclidean",
                        cluster.type="SOCK", debugging=FALSE, rasterOut=TRUE, method="classic")
  names(r1$window.5$alpha.1) = names(rast)
  path= paste0("./Processed_rasters/Geodiversity/", names(r1$window.5$alpha.1), ".tif")
  writeRaster(r1$window.5$alpha.1, filename= path ,options=c('TFW=YES'), overwrite =T)
}

# Landform het.

LF = raster::raster("./Processed_rasters/Discrete/landform_250m.tif")

LF_sha <- rasterdiv::Shannon(x = LF, window = 5, na.tolerance = 0.6, np = 1)

raster::writeRaster(LF_sha, filename = "./Processed_rasters/Geodiversity/Landform_Sha.tif", 
                    options = c("TFW=YES"), overwrite = T)
