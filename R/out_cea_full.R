#' Script to extract full cumulative effects from species and network-scale assessments
#'
#' @export

out_cea_full <- function() {
  # Output 
  out <- here::here("output","cea_full")
  rcea::chk_create(out)
  
  # Species-scale cumulative effects assessment
  # cea <- fnc_cumul(here::here("output", "cea"))
  # stars::write_stars(cea, here::here(out, "cea.tif"))
  
  # Network-scale cumulative effects assessment - net effets
  ncea_net <- fnc_cumul(here::here("output", "ncea", "net"))
  stars::write_stars(ncea_net, here::here(out, "ncea.tif"))

  # Network-scale cumulative effects assessment - direct effets
  ncea_direct <- fnc_cumul(here::here("output", "ncea", "direct"))
  stars::write_stars(ncea_direct, here::here(out, "ncea_direct.tif"))

  # Network-scale cumulative effects assessment - indirect effets
  ncea_indirect <- fnc_cumul(here::here("output", "ncea", "indirect"))
  stars::write_stars(ncea_indirect, here::here(out, "ncea_indirect.tif"))
}

fnc_cumul <- function(folder) {
  # Load empty raster for easy masking
  load(here::here("data","FormatData","emptyRaster.RData"))
  r <- r+1
  
  # Load and cumul for each species
  library(stars)
  files <- dir(folder, full.names = TRUE) 
  dat <- lapply(files, function(x) {
    stars::read_stars(x) |>
    stars::st_apply(c("x","y"), sum, na.rm = TRUE) |>
    sf::st_set_crs(sf::st_crs(r))
  })  

  # Cumul for all species together
  dat <- do.call("c", dat) |>
    stars::st_redimension() |>
    stars::st_apply(c(1,2), sum, na.rm = TRUE)     
  
  # Set values outside St. Lawrence to NA
  stars::st_dimensions(r) <- stars::st_dimensions(dat)
  dat <- dat * r
  
  # Return
  dat
}

fnc_cumul2 <- function(folder) {
  # Load empty raster for easy masking
  load(here::here("data","FormatData","idBiotic.RData"))
  
  # Load and cumul for each species
  library(raster)
  files <- dir(folder, full.names = TRUE) 
  dat <- lapply(files, function(x) {
    raster::stack(x) |>
    sum(na.rm = TRUE)
  })  

  # Cumul for all species together
  dat <- raster::stack(dat) |>
    sum(na.rm = TRUE)
  
  # Set values outside St. Lawrence to NA
  values(dat)[!idBiotic] <- NA
  
  # Return
  dat
}

