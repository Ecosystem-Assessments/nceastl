#' Script to extract correlations and anomalies between direct and indirect effects to assess amplification mechanisms
#'
#' @export

out_mechanisms <- function() {
  # Libraries
  library(stars)
  library(magrittr)

  # output 
  output <- here::here("output","cea_mechanisms")
  rcea::chk_create(output)

  # Folders & input files 
  direct <- here::here("output","ncea","direct")
  indirect <- here::here("output","ncea","indirect")

  # Empty raster of study area
  load('./Data/FormatData/idBiotic.RData')
  load('./Data/FormatData/emptyRaster.RData')
  r <- as(r+1, "Raster")
  
  # Areas
  load(here::here("data","FormatData","bt.RData"))
  hr <- data.frame(
    species = colnames(bt),
    area = colSums(bt, na.rm = TRUE)
  )
  rownames(hr) <- NULL
  
  # Load all data 
  loadras <- function(input) {
    input <- dir(input, full.names = TRUE)
    nm <- basename(input) |> stringr::str_replace_all(".tif","")
    lapply(input, function(x) {
      raster::stack(x) |>
      sum(na.rm = TRUE)
    }) |>
    raster::stack() |>
    setNames(nm)
  }
  direct <- loadras(direct)
  indirect <- loadras(indirect)

  # Check names 
  stopifnot(all(hr$species[1:nlayers(direct)] == names(direct)))
  
  # Extractions
  dat <- data.frame(
    species = names(direct),
    anomalies = 0,
    cor = 0,
    r2adj = 0
  )
  
  for(i in 1:nlayers(direct)) {
    drc <- round(direct[[i]], 5) * r
    ind <- round(indirect[[i]], 5) * r
    
    # Spatial differences between direct and indirect effects per area for each species 
    # Assessment of the difference in the distribution of direct vs indirect effects
    dat$anomalies[i] <- sum(values(abs(drc - ind)), na.rm = TRUE) / hr$area[i]
    
    # Correlation between direct and indirect effects 
    # Assessment of the similarity between direct and indirect effects
    tmp <- data.frame(
      dir = values(drc),
      ind = values(ind)
    ) |>
    na.omit() |>
    dplyr::filter(dir != 0 & ind != 0)
    dat$cor[i] <- cor(tmp$dir, tmp$ind, method = "spearman")
    if(!is.na(dat$cor[i])) dat$r2adj[i] <- summary(lm(tmp$dir ~ tmp$ind))$adj.r.squared
  }
  

  for(i in 1:nlayers(direct)) {
    drc <- round(direct[[i]], 5) * r
    ind <- round(indirect[[i]], 5) * r
    xy <- coordinates(drc)
    
    # Correlation between direct and indirect effects 
    # Assessment of the similarity between direct and indirect effects
    tmp <- data.frame(
      dir = values(drc),
      ind = values(ind),
      longitude = xy[,'x'],
      latitude = xy[,'y']
    ) |>
    na.omit() |>
    dplyr::filter(dir != 0 & ind != 0)
    
    
    dat$cor[i] <- cor(tmp$dir, tmp$ind, method = "spearman")
    if(!is.na(dat$cor[i])) dat$r2adj[i] <- summary(lm(tmp$dir ~ tmp$ind))$adj.r.squared
  }



  
  write.csv(dat, here::here(output, "mechanisms.csv"), row.names = FALSE)
}