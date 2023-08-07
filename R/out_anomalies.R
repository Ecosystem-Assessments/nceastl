#' Script to extract anomalies between species-scale and network-scale cumualtive effects assessments
#'
#' @export

out_anomalies <- function() {
  # Libraries
  library(stars)
  library(magrittr)

  # output 
  output <- here::here("output","cea_anomalies")
  rcea::chk_create(output)

  # Folders & input files 
  ce <- here::here("output","ncea","net")
  ci <- here::here("output","cea")

  # Load all data 
  loadras <- function(input) {
    input <- dir(input, full.names = TRUE)
    nm <- basename(input) |> stringr::str_replace_all(".tif","")
    lapply(input, function(x) {
      raster::stack(x) |>
      sum()
    }) |>
    raster::stack() |>
    setNames(nm)
  }
  ce <- loadras(ce)
  ci <- loadras(ci)

  # Anomalies 
  anomalies <- list()
  for(i in 1:nlayers(ce)) {
    cn <- round(ce[[i]], 5) + 1
    dn <- round(ci[[i]], 5) + 1
    anomalies[[i]] <- log(cn / dn)
  }
  anomalies <- raster::stack(anomalies) |>
    sum(na.rm = TRUE)

  # Set values outside study region back to NA
  load('./Data/FormatData/idBiotic.RData')
  values(anomalies)[!idBiotic] <- NA

  # Export 
  raster::writeRaster(anomalies, here::here(output, "anomalies.tif"))
}