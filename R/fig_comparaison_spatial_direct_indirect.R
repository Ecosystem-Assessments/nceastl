#' Figure of spatial comparison between species-scale and network-scale cumulative effects assessments
#'
#' @export
fig_comparison_spatial_direct_indirect <- function() {
  # Libraries
  library(stars)
  library(raster)
  library(sf)
  library(graphicsutils)
  library(tidyverse)
  param()
  load("./Data/Spatial/egslSimple.RData") # Simple EGSL geometry
  egslSimple <<- egslSimple

  # Species & Drivers
  species <- stars::read_stars("output/footprint/species_richness.tif")
  drivers <- stars::read_stars("output/footprint/cumulative_drivers.tif")

  # Cumulative effects
  direct <- stars::read_stars("output/cea_full/ncea_direct.tif")
  indirect <- stars::read_stars("output/cea_full/ncea_indirect.tif")
  diff <- direct - indirect

  # Param
  maxVal <- c(
    max(direct[[1]], na.rm = TRUE),
    max(indirect[[1]], na.rm = TRUE)
  )

  # Figure
  out <- here::here("figures")
  rcea::chk_create(out)

  png(here::here(out, "comparison_spatial_direct_indirect.png"), res = 900, width = 300, height = 300, units = "mm")
  par(mfrow = c(3, 2), family = "serif")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot cumulative drivers
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

  image(drivers, col = viridis::magma(100), add = T, zlim = c(0, max(drivers[[1]], na.rm = TRUE)))
  plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
  legendEGSL(
    range = c(0, max(drivers[[1]], na.rm = TRUE)), pal = viridis::magma, cexMain = .9, cexSub = .65, n = 5,
    mainTitle = "Cumulative stressors"
  )


  # Letter
  pos <- par("usr")
  text(x = pos[1] + (pos[2] - pos[1]) * 0.015, y = pos[4] - (pos[4] - pos[3]) * 0.018, labels = "A", cex = 1.5, font = 2)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot species richness
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

  image(species, col = viridis::viridis(100), add = T, zlim = c(0, max(species[[1]], na.rm = TRUE)))
  plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
  legendEGSL(
    range = c(0, max(species[[1]], na.rm = TRUE)), pal = viridis::viridis, cexMain = .9, cexSub = .65, n = 5,
    mainTitle = "Species richness"
  )


  # Letter
  pos <- par("usr")
  text(x = pos[1] + (pos[2] - pos[1]) * 0.015, y = pos[4] - (pos[4] - pos[3]) * 0.018, labels = "B", cex = 1.5, font = 2)



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot direct
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

  image(direct, col = palImpact(100), add = T, zlim = c(0, maxVal[1]))
  plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
  legendEGSL(
    range = c(0, maxVal[1]), pal = palImpact, cexMain = .9, cexSub = .65, n = 5,
    mainTitle = "Direct cumulative effects"
  )


  # Letter
  pos <- par("usr")
  text(x = pos[1] + (pos[2] - pos[1]) * 0.015, y = pos[4] - (pos[4] - pos[3]) * 0.018, labels = "C", cex = 1.5, font = 2)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot indirect
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

  image(indirect, col = palImpact(100), add = T, zlim = c(0, maxVal[2]))
  plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
  legendEGSL(
    range = c(0, maxVal[2]), pal = palImpact, cexMain = .9, cexSub = .65, n = 5,
    mainTitle = "Indirect cumulative effects"
  )

  # Letter
  pos <- par("usr")
  text(x = pos[1] + (pos[2] - pos[1]) * 0.009, y = pos[4] - (pos[4] - pos[3]) * 0.018, labels = "D", cex = 1.5, font = 2)



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot differences
  red <- "#744242"
  blue <- "#036e95"
  pal1 <- colorRampPalette(c(graphicsutils::lighten(red, 80), graphicsutils::darken(red, 50)))
  pal2 <- colorRampPalette(c(graphicsutils::darken(blue, 50), graphicsutils::lighten(blue, 80)))
  r <- c(floor(min(diff[[1]], na.rm = TRUE)), ceiling(max(diff[[1]], na.rm = TRUE)))
  rMax <- max(abs(r))

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
  R <- c(min(diff[[1]], na.rm = TRUE), max(diff[[1]], na.rm = TRUE))
  image(diff, col = pal1(100), zlim = c(0, rMax), add = TRUE)
  image(diff, col = pal2(100), zlim = c(-rMax, 0), add = TRUE)
  plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)

  # Legend
  plot_legend_dual(
    rMax = rMax,
    pal1 = pal1,
    pal2 = pal2,
    colText = "#000000",
    mainTitle = "Spatial differences",
    subTitle = "Direct - indirect effects"
  )

  # Letter
  pos <- par("usr")
  text(x = pos[1] + (pos[2] - pos[1]) * 0.015, y = pos[4] - (pos[4] - pos[3]) * 0.018, labels = "E", cex = 1.5, font = 2)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot biplot
  par(mar = c(4.5, 4.5, 2, 2))
  plot0(x = c(0, maxVal[1]), y = c(0, maxVal[2]))
  axis(1, at = seq(0, maxVal[1], length.out = 5), labels = seq(0, ceiling(maxVal[1]), length.out = 5))
  axis(2, at = seq(0, maxVal[2], length.out = 5), labels = seq(0, ceiling(maxVal[2]), length.out = 5))
  mtext(side = 1, text = "Direct cumulative effect", line = 2.5, font = 2)
  mtext(side = 2, text = "Indirect cumulative effects", line = 2.5, font = 2)
  points(x = direct[[1]], y = indirect[[1]], pch = 20, col = "#00000077", cex = .05)
  lines(x = c(0, maxVal[1]), y = c(0, maxVal[1]), col = "#EEB956", lwd = 3)

  # Letter
  mtext(side = 2, at = maxVal[2], text = "F", cex = 1.3, font = 2, las = 2, line = 2.5)
  dev.off()
}

# =================================================================
plot_legend_dual <- function(
    rMax,
    pal1 = NULL,
    pal2 = NULL,
    cexMain = .75,
    cexSub = .5,
    minUp = .08,
    mainTitle = NULL,
    subTitle = NULL,
    n = 5,
    colText = "#dedede") {
  # Legends
  # Palette
  if (class(pal1) == "character") {
    pal1 <- colorRampPalette(pal1)
    pal2 <- colorRampPalette(pal2)
  }

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .015 * xR # minimum left side to write
  yinit <- ymax - minUp * yR # minimum upper side to write
  ygap <- .04 * yR
  xgap <- .014 * xR
  ybarUp <- yinit - ygap / 2 - .0041 * yR
  ybarDn <- yinit - ygap - ygap / 2 + .0041 * yR

  # Divide in two
  wdPal <- .17 * xR
  xmid <- xinit + (wdPal) / 2
  xend <- xinit + wdPal

  # Palette 2 (negative)
  x <- seq(from = xinit, to = xmid, by = .0003 * xR)
  z <- data.frame(
    y1 = ybarUp,
    y2 = ybarDn,
    x1 = x[1:length(x) - 1],
    x2 = x[2:length(x)],
    col = pal2(length(x) - 1),
    stringsAsFactors = F
  )
  for (k in 1:nrow(z)) {
    polygon(
      x = c(z$x1[k], z$x2[k], z$x2[k], z$x1[k], z$x1[k]),
      y = c(z$y1[k], z$y1[k], z$y2[k], z$y2[k], z$y1[k]),
      col = z$col[k],
      border = z$col[k]
    )
  }

  # Palette 1 (positive)
  x <- seq(from = xmid, to = xend, by = .0003 * xR)
  z <- data.frame(
    y1 = ybarUp,
    y2 = ybarDn,
    x1 = x[1:length(x) - 1],
    x2 = x[2:length(x)],
    col = pal1(length(x) - 1),
    stringsAsFactors = F
  )
  for (k in 1:nrow(z)) {
    polygon(
      x = c(z$x1[k], z$x2[k], z$x2[k], z$x1[k], z$x1[k]),
      y = c(z$y1[k], z$y1[k], z$y2[k], z$y2[k], z$y1[k]),
      col = z$col[k],
      border = z$col[k]
    )
  }

  # Add axis
  x <- seq(from = xinit, to = xinit + wdPal, length.out = n)
  lines(x = c(xinit, xinit + wdPal), y = rep(z$y2[1], 2), col = colText)
  for (i in 1:n) lines(x = rep(x[i], 2), y = c(z$y2[1], z$y2[1] - .003 * yR), col = colText)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Labels
  lab <- round(c(-rMax, -rMax / 2, 0, rMax / 2, rMax))
  text(
    x = x,
    y = rep(z$y2[1] - .01 * yR, n),
    labels = lab,
    cex = cexSub * .75,
    adj = c(1, 1),
    srt = 45,
    col = colText
  )

  # Add titles
  yText <- ybarUp + .025 * yR

  # Add sub text
  if (!is.null(subTitle)) {
    text(
      x = xinit,
      y = yText,
      labels = latex2exp::TeX(subTitle, italic = TRUE),
      cex = cexSub,
      adj = c(0, 1),
      col = colText
    )
    yText <- yText + .035 * yR
  }

  # Add main title
  if (!is.null(mainTitle)) {
    text(
      x = xinit,
      y = yText,
      labels = mainTitle,
      cex = cexMain,
      font = 2,
      adj = c(0, 1),
      col = colText
    )
  }
}
