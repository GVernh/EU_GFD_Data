# WARNING: The following script will download SoilGrids rasters to the raw data directory.
# This will require approx. 90GB of hard drive space.

# ---------------------- libraries ------------------------------
#test
libs <- c(
  "soilDB","terra")

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

# Europe coundary shape file
EU_boundary <- terra::vect("./Raw_data/clipped_EU.shp")

# Download raster of Soil propserties from SoilGrids 2.0
fetchSoilGrids(
  EU_boundary,
  loc.names = c("id", "lat", "lon"),
  depth_intervals = c("0-5", "5-15", "15-30"),
  variables = c("cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt",
                "soc", "ocd"),
  grid = TRUE,
  filename = "Soil_raster",
  overwrite = TRUE,
  target_resolution = c(250, 250),
  summary_type =  "mean",
  verbose = FALSE,
  progress = TRUE
)