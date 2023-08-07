#' Figure of the distribution of effects on selected species
#'
#' @export
fig_groups_spatial <- function() {
  # Libraries
  library(raster)
  library(stars)
  library(sf)
  library(graphicsutils)
  library(tidyverse)
  param()
  load('./Data/Spatial/egslSimple.RData') # Simple EGSL geometry
  egslSimple <<- egslSimple

  # Folders & input files 
  ce <- here::here("output","ncea","net")

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

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Plot function
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  plotGroups <- function(x, legend = T, lab = NULL) {
    plotEGSL(layers     = c('egslSimple'),
             cols       = c('#00000000'),
             borders    = c('#00000000'),
             lwds       = c(.75),
             mar        = c(1,1,1,1),
             box        = FALSE,
             axes       = FALSE,
             northArrow = F,
             prj        = prj,
             extent     = st_bbox(egslSimple)+c(-21000,-21000,21000,21000))
    image(x, col = palImpact(100), add = T)
    plot(st_geometry(egslSimple), col = 'transparent', border = focus, lwd = .75, add = T)
    if (legend) {
      legendEGSL(range = c(0, maxValue(x)), pal = palImpact, cexMain = .75, cexSub = .5, n = 5)
    }
    pos <- par("usr")
    text(x = pos[1] + (pos[2]-pos[1])*0.009, y = pos[4] - (pos[4]-pos[3])*0.025, labels = lab, cex = 1, font = 2, adj = c(0,.5))
  }


  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Layout
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  mat <- matrix(0, ncol = 3, nrow = 5)
  mat[1, ] <- 1
  mat[4, ] <- 2
  mat[2,1] <- 3
  mat[5,1] <- 4
  mat[2, 2:3] <- 5:6
  mat[3, 2:3] <- 7:8
  mat[5, 2:3] <- 9:10

  # Graph
  out <- here::here("figures")
  rcea::chk_create(out)
  
  png(here::here(out, 'groups_spatial.png'), res = 300, width = 240, height = 200, units = "mm")
  y <- .75
  layout(mat, heights = c(.2,y,y,.2,y))
  par(family = 'serif')
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Titles
  tit <- c('Invertebrates','Vertebrates')
  for(i in 1:2) {
    par(mar = c(0,0,0,0))
    plot0()
    text(-1, 0, labels = tit[i], cex = 2, font = 2, adj = c(0,.5))
  }

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Main groups
  # Invertebrates
  uid <- which(names(ce) %in% spList$shortname[spList$gr1 == 'Invertebrates'])
  dat <- sum(ce[[uid]], na.rm = TRUE)
  values(dat)[values(dat == 0)] <- NA
  plotGroups(dat)

  # Invertebrates
  uid <- which(names(ce) %in% spList$shortname[spList$gr1 == 'Vertebrates'])
  dat <- sum(ce[[uid]], na.rm = TRUE)
  values(dat)[values(dat == 0)] <- NA
  plotGroups(dat)

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Arthropoda
  uid <- which(names(ce) %in% spList$shortname[spList$gr2 == 'Arthropoda'])
  dat <- sum(ce[[uid]], na.rm = TRUE)
  values(dat)[values(dat == 0)] <- NA
  plotGroups(dat, lab = 'Arthropoda')

  # Cnidaria
  uid <- which(names(ce) %in% spList$shortname[spList$gr2 == 'Cnidaria'])
  dat <- sum(ce[[uid]], na.rm = TRUE)
  values(dat)[values(dat == 0)] <- NA
  plotGroups(dat, lab = 'Cnidaria')

  # Echinodermata
  uid <- which(names(ce) %in% spList$shortname[spList$gr2 == 'Echinodermata'])
  dat <- sum(ce[[uid]], na.rm = TRUE)
  values(dat)[values(dat == 0)] <- NA
  plotGroups(dat, lab = 'Echinodermata')

  # Mollusca
  uid <- which(names(ce) %in% spList$shortname[spList$gr2 == 'Mollusca'])
  dat <- sum(ce[[uid]], na.rm = TRUE)
  values(dat)[values(dat == 0)] <- NA
  plotGroups(dat, lab = 'Mollusca')

  # Actinopterygii
  uid <- which(names(ce) %in% spList$shortname[spList$gr2 == 'Actinopterygii'])
  dat <- sum(ce[[uid]], na.rm = TRUE)
  values(dat)[values(dat == 0)] <- NA
  plotGroups(dat, lab = 'Actinopterygii')

  # Mammalia
  uid <- which(names(ce) %in% spList$shortname[spList$gr2 == 'Mammalia'])
  dat <- sum(ce[[uid]], na.rm = TRUE)
  values(dat)[values(dat == 0)] <- NA
  plotGroups(dat, lab = 'Mammalia')
  dev.off()
}