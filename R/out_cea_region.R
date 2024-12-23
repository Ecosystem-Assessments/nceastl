#' Script to extract mean network-scale cea in St. Lawrence System subregions
#'
#' @export

out_cea_region <- function() {
  # Libraries
  library(raster)
  library(sf)

  # Data
  load('./Data/Spatial/egslSub.RData')
  ci <- raster::raster("output/cea_full/ncea.tif")
  # anomalies <- readRDS('./Data/Results/Cumulative_Effects/CumulativeEffects_Anomalies_Normalized.rds')


  # Transform to `sp` object
  egsl <- as(egslSub, 'Spatial')

  # Extract
  region <- extract(ci, egsl, fun = sum, na.rm = TRUE)
  # region2 <- extract(anomalies, egsl, fun = sum, na.rm = TRUE)

  # Area
  areaSub <- sf::st_area(egslSub)
  areaSub <- units::set_units(areaSub, km^2)

  # As data.frame
  regions <- data.frame(
    Ic = region,
    # Anomalies = region2,
    Area = areaSub,
    Ickm2 = region/areaSub,
    # Anomalieskm2 = region2/areaSub,
    Names = egsl$Regions,
    stringsAsFactors = FALSE
  )

  # Export
  out <- here::here("output","cea_region")
  rcea::chk_create(out)
  write.csv(regions, file = here::here(out, "cea_regions.csv"), row.names = FALSE)
}