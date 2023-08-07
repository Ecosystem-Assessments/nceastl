#' Figure of contribution of direct and indirect effects of stressors to taxonomic groups
#'
#' @export
fig_stressors <- function() {
  # Libraries
  library(graphicsutils)
  library(sf)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(latex2exp)
  library(stars)
  library(raster)
  param()
  
  # Data
  load(here::here("Data","FormatData","driversRaster.RData"))
  drivers <- as(drivers, "Raster") # Code already works with raster, save some time
  load('./Data/Spatial/egslSimple.RData') # Simple EGSL geometry
  egslSimple <<- egslSimple
  
  # Plot function
  plotStressor <- function(x, legend = T, lab = NULL) {
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
    image(x, col = palImpact2(100), add = T)
    plot(st_geometry(egslSimple), col = 'transparent', border = focus, lwd = .75, add = T)
    if (legend) {
      legendEGSL(range = c(0, maxValue(x)), pal = palImpact2, cexMain = .75, cexSub = .5, n = 5)
    }
    pos <- par("usr")
    text(x = pos[1] + (pos[2]-pos[1])*0.009, y = pos[4] - (pos[4]-pos[3])*0.025, labels = lab, cex = 1, font = 2, adj = c(0,.5))
  }


  # Layout
  mat <- matrix(0, ncol = 4, nrow = 11)
  mat[1, ] <- 1
  mat[4, ] <- 2
  mat[7, ] <- 3
  mat[10, ] <- 4
  mat[2,1] <- 5
  mat[5,1] <- 6
  mat[8,1] <- 7
  mat[11,1] <- 8
  mat[2, 2:4] <- 9:11
  mat[3, 2:4] <- 12:14
  mat[5, 2:4] <- 15:17
  mat[6, 2:3] <- 18:19
  mat[8, 2:4] <- 20:22
  mat[9, 2:3] <- 23:24
  mat[11, 2:3] <- 25:26

  out <- here::here("figures")
  rcea::chk_create(out)
  png(here::here(out, 'stressors.png'), res = 300, width = 300, height = 450, units = "mm")
  y <- .75
  layout(mat, heights = c(.2,y,y,.2,y,y,.2,y,y,.2,y))

  # Titles
  for(i in 1:nrow(grNames)) {
    par(mar = c(0,0,0,0))
    plot0()
    text(-.95, 0, labels = grNames$name[i], cex = 2, font = 2)
  }

  # Groups
  for(i in 1:nrow(grNames)) {
    uid <- drNames$var[drNames$group %in% grNames$accr[i]]
    dat <- sum(drivers[[uid]])
    plotStressor(dat, lab = 'Cumulative exposure')
  }

  # Stressors
  for(i in 1:nrow(drNames)) {
    plotStressor(drivers[[drNames$var[i]]], lab = drNames$full[i])
  }

  dev.off()
}