#############################################
############### LIBRARIES ###################
#############################################
libs <- c("terra", "sf", "mapview","stars", "tidyverse")

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

#############################################
##### CREATE DIRECTORIES & FILENAME TAGS ####
#############################################
# NOTE: 2500m plots are mostly in Russia, not Europe, thus they have been removed.
dir = c("./100m_data/", "./400m_data/", "./1000m_data/", "./All_plot_data/")
tags = c("_100m", "_400m", "_1000m", "all_plots")
dir.create("./Analysis/Finalised_rasters/Functional_diversity", 
           showWarnings = T)
dir.create("./Figures_Tables/", 
           showWarnings = T)
dir.create("./Figures_Tables/Appendices/", 
           showWarnings = T)

setwd("./Analysis/Finalised_rasters/Functional_diversity/")
for  (i in 1:length(dir)){
dir.create(dir[i], showWarnings = FALSE)
}

#############################################
######### PROJECTION RASTER #################
#############################################
Proj_rast = terra::rast("../../Finalised_rasters/BIO1_Annual_Mean_Temperature.tif")

#############################################
######### RASTERISE NESTED LOOP #############
#############################################

for (i in 1:length(dir)) {
  setwd("../../../Raw_data/Biodiversity/")
  load(file=paste0(dir[i],"results", tags[i],".Rdata"))
  results <- results %>%
    dplyr::select(c("Longitude","Latitude"), everything())
  
  # Create point dataframe
  my.sf.point <- sf::st_as_sf(x = results, 
                              coords = c("Longitude", "Latitude"),
                              crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  sf_point = sf::st_transform(my.sf.point, crs = "EPSG:3035")
  points = terra::vect(sf_point)
  points[["ID_new_v1"]] <- tags[i]
  mapview(points) # Provide a map in viewer for each
  
  setwd("./Analysis/Finalised_rasters/Functional_diversity/")
  terra::writeVector(points, filename = paste0(dir[i],"points.shp"), overwrite=TRUE)
  
  bio_label = colnames(results[4:length(results)])
                       
        for (y in seq_along(1:length(bio_label))) {
          FD_raster = terra::rasterize(x = sf_point, y = Proj_rast, field = bio_label[y], fun = mean)
          names(FD_raster) = bio_label[y]
          path = paste0(dir[i], bio_label[y], ".tif") 
          terra::writeRaster(FD_raster, filename = path, gdal = "TFW=YES", overwrite = T)
        }
  
  # Create histogram of plot frequencies
  sf_point$count = 1
  FD_raster_count = terra::rasterize(x = sf_point, y = Proj_rast, field = "count", fun = sum)
  x  = as.data.frame(FD_raster_count)
  
  jpeg(file= paste0("./Figures_Tables/Appendices",tags[i],".jpeg"),width = 1000, height = 600)
  hist(x$sum, breaks = max(x$sum), xlab = "N plots per 1km cell", ylab = "Frequency", 
       main = "",xlim = c(1,15), cex.lab = 1.4, cex.axis = 1.2)
  dev.off()
}

# ---------------- Number of cells with values ----------------------

# NEEDS CORRECTING
# unique(rast)

#FD_raster = raster::raster(FD_raster)
# y = na.omit(getValues(FD_raster)) 
