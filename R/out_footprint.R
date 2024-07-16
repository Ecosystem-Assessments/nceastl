#' Function to export cumulative drivers and species (richness)
#'
#' @export

out_footprint <- function() {
  library(stars)
  cumul <- function(dat) {
    library(stars)
    merge(dat) |>
      stars::st_apply(c(1, 2), sum, na.rm = TRUE)
  }

  # Study area
  load("./Data/Spatial/egslSimple.RData") # Simple EGSL geometry
  aoi <<- egslSimple

  # Cumulative drivers
  load("data/FormatData/driversRaster.RData")
  r <- merge(drivers) |>
    stars::st_apply(c(1, 2), sum, na.rm = TRUE)
  r <- r[aoi]
  out <- here::here("output", "footprint")
  rcea::chk_create(out)
  nm <- glue::glue("cumulative_drivers.tif")
  stars::write_stars(r, dsn = here::here(out, nm), delete_dsn = TRUE, quiet = TRUE)

  # Species richness
  load("data/FormatData/biotic.RData")
  r <- merge(biotic) |>
    stars::st_apply(c(1, 2), sum, na.rm = TRUE)
  r <- r[aoi]
  nm <- glue::glue("species_richness.tif")
  stars::write_stars(r, dsn = here::here(out, nm), delete_dsn = TRUE, quiet = TRUE)
}
