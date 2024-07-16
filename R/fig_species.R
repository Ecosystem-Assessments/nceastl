#' Figure of the distribution of effects on selected species
#'
#' @export
fig_species_spatial <- function() {
  # Libraries
  library(raster)
  library(stars)
  library(sf)
  library(graphicsutils)
  library(tidyverse)
  param()
  load("./Data/Spatial/egslSimple.RData") # Simple EGSL geometry
  egslSimple <<- egslSimple


  # Species for which to get the data
  ground <- c(
    "Gadus.morhua",
    "Hippoglossus.hippoglossus",
    "Hippoglossoides.platessoides",
    "Glyptocephalus.cynoglossus",
    "Sebastes.sp.",
    "Reinhardtius.hippoglossoides"
  )
  other <- c("Lophius.americanus", "Delphinapterus.leucas")

  # Folders & input files
  inp_cea <- here::here("output", "cea", glue::glue("{c(ground,other)}.tif"))
  inp_ncea <- here::here("output", "ncea", "net", glue::glue("{c(ground,other)}.tif"))

  # Load all data
  loadras <- function(input) {
    lapply(input, function(x) {
      raster::stack(x) |>
        sum()
    }) |>
      raster::stack() |>
      setNames(c(ground, other))
  }
  di <- loadras(inp_cea)
  ce <- loadras(inp_ncea)

  # Create groudfish layer
  di$Groundfish <- sum(di[[ground]])
  ce$Groundfish <- sum(ce[[ground]])

  # Names
  # sp <- c('Groundfish','Lophius.americanus','Delphinapterus.leucas')
  sp <- c("Groundfish", "Delphinapterus.leucas")
  spNames <- gsub("\\.", " ", sp)

  # sp <- spNames <- c('Malacoraja.senta','Hemitripterus.americanus','Microgadus.tomcod','Bathyraja.spinicauda','Leptoclinus.maculatus')
  # sp <- 'Homarus.americanus'
  # mat <- matrix(1:3, nrow = 3)
  # par(mfrow = c(2,2))

  # # 0 to NA
  # for(i in 1:length(sp)) {
  #   values(ce[[sp[i]]])[values(ce[[sp[i]]]) == 0] <- NA
  #   values(di[[sp[i]]])[values(di[[sp[i]]]) == 0] <- NA
  # }

  # Graph
  out <- here::here("figures")
  rcea::chk_create(out)

  # Layout
  # mat <- matrix(1:9, nrow = 3)
  mat <- matrix(1:6, nrow = 2)

  png(here::here(out, "species_spatial.png"), res = 300, width = 300, height = 200, units = "mm")
  layout(mat)

  # Plot network-scale CEA
  for (i in 1:length(sp)) {
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
    maxVal <- maxValue(ce[[sp[i]]])
    image(ce[[sp[i]]], col = palImpact2(100), add = T, zlim = c(0, maxVal))
    plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
    legendEGSL(
      range = c(0, maxVal[1]), pal = palImpact, cexMain = .9, cexSub = .65, n = 5,
      mainTitle = spNames[i], subTitle = "Network-scale"
    )
    # Letter
    pos <- par("usr")
    text(x = pos[1] + (pos[2] - pos[1]) * 0.015, y = pos[4] - (pos[4] - pos[3]) * 0.022, labels = toupper(letters[i]), cex = 1.2, font = 2)
  }

  # Plot species-scale CEA
  for (i in 1:length(sp)) {
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
    maxVal <- maxValue(di[[sp[i]]])
    image(di[[sp[i]]], col = palImpact2(100), add = T, zlim = c(0, maxVal))
    plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
    legendEGSL(
      range = c(0, maxVal), pal = palImpact, cexMain = .9, cexSub = .65, n = 5,
      mainTitle = spNames[i], subTitle = "Species-scale"
    )
    # # Letter
    # pos <- par("usr")
    # text(x = pos[1] + (pos[2]-pos[1])*0.009, y = pos[4] - (pos[4]-pos[3])*0.015, labels = 'a', cex = 1.5, font = 2)
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Plot biplot
  for (i in 1:length(sp)) {
    dat1 <- values(di[[sp[i]]])
    dat2 <- values(ce[[sp[i]]])
    maxVal <- c(maxValue(di[[sp[i]]]), maxValue(ce[[sp[i]]]))

    par(mar = c(4.5, 4.5, 2, 2))
    # plot0(x = c(0, 1))
    plot0(x = c(0, maxVal[1]), y = c(0, maxVal[2]))
    # axis(1, at = seq(0, 1, length.out = 5), labels = seq(0, ceiling(maxVal[1]), length.out = 5))
    # axis(2, at = seq(0, 1, length.out = 5), labels = seq(0, ceiling(maxVal[2]), length.out = 5))
    axis(1, at = seq(0, maxVal[1], length.out = 5), labels = seq(0, ceiling(maxVal[1]), length.out = 5))
    axis(2, at = seq(0, maxVal[2], length.out = 5), labels = seq(0, ceiling(maxVal[2]), length.out = 5))
    mtext(side = 1, text = "Species-scale cumulative effects", line = 2.5, font = 1, cex = .7)
    mtext(side = 2, text = "Network-scale cumulative effects", line = 2.5, font = 1, cex = .7)
    # points(x = dat1 / maxVal[1], y = dat2 / maxVal[2], pch = 20, col = "#00000077", cex = .15)
    points(x = dat1, y = dat2, pch = 20, col = "#00000077", cex = .15)
    # lines(x = c(0, 1), y = c(0, 1), col = "#EEB956", lwd = 3)
    lines(x = c(0, maxVal[1]), y = c(0, maxVal[1]), col = "#EEB956", lwd = 3)

    # Letter
    # mtext(side = 2, at = 1.02, text = 'b', cex = 1.3, font = 2, las = 2, line = 4)
  }
  dev.off()
}
