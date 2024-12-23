#' Script to assess species-scale cumulative effects
#'
#' @export

cea <- function() {
  library(stars)
  # Function to export stars objects
  export_stars <- function(dat, out, n) {
    # Create output
    out <- paste0(out, "/")
    rcea::chk_create(out)
    nm <- names(dat)

    # Export
    for (i in 1:n) {
      stars::write_stars(
        adrop(dat[i]),
        paste0(out, "/", nm[i], ".tif")
      )
    }
  }

  # Specify and create output folder
  output <- here::here("output", "cea")
  # output <- "~/scratch/output/cea/"
  rcea::chk_create(output)

  # Load files for analysis
  load("data/FormatData/driversRaster.RData")
  load("data/FormatData/biotic.RData")
  load("data/FormatData/species_sensitivity.RData")
  n <- length(biotic)

  # Cumulative effects assessment
  halpern <- rcea::cea(drivers, biotic, species_sensitivity, "stars")
  export_stars(halpern, output, n = n)
}
