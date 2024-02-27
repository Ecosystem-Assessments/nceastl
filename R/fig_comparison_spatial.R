#' Figure of spatial comparison between species-scale and network-scale cumulative effects assessments
#'
#' @export
fig_comparison_spatial <- function() {
  # Libraries
  library(raster)
  library(sf)
  library(graphicsutils)
  library(tidyverse)
  param()
  load("./Data/Spatial/egslSimple.RData") # Simple EGSL geometry
  egslSimple <<- egslSimple

  # Data
  connect <- stars::read_stars("output/cea_full/ncea.tif")
  disconnect <- stars::read_stars("output/cea_full/cea.tif")
  anomalies <- stars::read_stars("output/cea_anomalies/anomalies.tif")

  # Param
  maxVal <- c(
    max(disconnect[[1]], na.rm = TRUE),
    max(connect[[1]], na.rm = TRUE),
    max(anomalies[[1]], na.rm = TRUE)
  )

  minVal <- c(
    min(disconnect[[1]], na.rm = TRUE),
    min(connect[[1]], na.rm = TRUE),
    min(anomalies[[1]], na.rm = TRUE)
  )

  # Figure
  out <- here::here("figures")
  rcea::chk_create(out)

  png(here::here(out, "comparison_spatial.png"), res = 900, width = 300, height = 225, units = "mm")
  par(mfrow = c(2, 2), family = "serif")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot connected
  par(bg = "#ffffff")
  plotEGSL(
    layers = c("egslSimple"),
    cols = c("#00000000"),
    borders = c("#00000000"),
    lwds = c(.75),
    mar = c(2, 2, 2, 2),
    northArrow = F,
    prj = prj,
    extent = extFig
  )

  image(connect, col = palImpact(100), add = T, zlim = c(0, maxVal[2]))
  plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
  legendEGSL(
    range = c(0, maxVal[2]), pal = palImpact, cexMain = .9, cexSub = .65, n = 5,
    mainTitle = "Network-scale cumulative effects"
  )


  # Letter
  pos <- par("usr")
  text(x = pos[1] + (pos[2] - pos[1]) * 0.015, y = pos[4] - (pos[4] - pos[3]) * 0.018, labels = "A", cex = 1.5, font = 2)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot disconnected
  par(bg = "#ffffff")
  plotEGSL(
    layers = c("egslSimple"),
    cols = c("#00000000"),
    borders = c("#00000000"),
    lwds = c(.75),
    mar = c(2, 2, 2, 2),
    northArrow = F,
    prj = prj,
    extent = extFig
  )

  image(disconnect, col = palImpact(100), add = T, zlim = c(0, maxVal[1]))
  plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
  legendEGSL(
    range = c(0, maxVal[1]), pal = palImpact, cexMain = .9, cexSub = .65, n = 5,
    mainTitle = "Species-scale cumulative effects"
  )

  # Letter
  pos <- par("usr")
  text(x = pos[1] + (pos[2] - pos[1]) * 0.009, y = pos[4] - (pos[4] - pos[3]) * 0.018, labels = "B", cex = 1.5, font = 2)



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot anomalies
  par(bg = "#ffffff")
  plotEGSL(
    layers = c("egslSimple"),
    cols = c("#00000000"),
    borders = c("#00000000"),
    lwds = c(.75),
    mar = c(2, 2, 2, 2),
    northArrow = F,
    prj = prj,
    extent = extFig
  )

  image(anomalies, col = palDist(100), add = T, zlim = c(0, maxVal[3]))
  plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
  legendEGSL(
    range = c(0, maxVal[3]), pal = palDist, cexMain = .9, cexSub = .65, n = 5,
    mainTitle = "Transgressive properties", subTitle = "log(Network-scale / Species-scale)"
  )

  # Letter
  pos <- par("usr")
  text(x = pos[1] + (pos[2] - pos[1]) * 0.015, y = pos[4] - (pos[4] - pos[3]) * 0.018, labels = "C", cex = 1.5, font = 2)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot biplot
  par(mar = c(4.5, 4.5, 2, 2))
  plot0(x = c(0, 1))
  axis(1, at = seq(0, 1, length.out = 5), labels = seq(0, ceiling(maxVal[1]), length.out = 5))
  axis(2, at = seq(0, 1, length.out = 5), labels = seq(0, ceiling(maxVal[2]), length.out = 5))
  mtext(side = 1, text = "Species-scale cumulative effect", line = 2.5, font = 2)
  mtext(side = 2, text = "Network-scale cumulative effects", line = 2.5, font = 2)
  points(x = disconnect[[1]] / maxVal[1], y = connect[[1]] / maxVal[2], pch = 20, col = "#00000077", cex = .15)
  lines(x = c(0, 1), y = c(0, 1), col = "#EEB956", lwd = 3)

  # Letter
  mtext(side = 2, at = 1.02, text = "D", cex = 1.3, font = 2, las = 2, line = 4)
  dev.off()
}
