#' Script to generate the figure of network-scale cumulative effects
#'
#' @export

fig_ncea <- function() {
  # source('./Code/Figures/F1-CumulativeEffects.R')
  # Libraries
  library(sf)
  library(dplyr)
  library(tidyr)
  library(latex2exp)
  library(raster)
  library(stringr)
  param()

  # Data
  ci <- raster::raster("output/cea_full/ncea.tif")
  region <- read.csv("output/cea_region/cea_regions.csv")
  load("./Data/Spatial/egslSimple.RData") # Simple EGSL geometry
  load("./Data/Spatial/egslSub.RData") # EGSL subregions
  egslSimple <<- egslSimple

  # Format region data
  # Calculate relative scores
  region <- region %>%
    mutate(rel1 = Ickm2 / max(Ickm2)) # ,
  # rel2 = Anomalieskm2/max(Anomalieskm2))

  # Join with sf object
  region <- dplyr::left_join(egslSub, region, by = c("Regions" = "Names"))

  # Regional evaluations
  pts <- st_centroid(region)
  # pts <- pts[!pts$Regions %in% c('S','EAV','EAM'), ] # Remove Saguenay and upstream saguaney
  pts <- pts[!pts$Regions %in% c("S"), ] # Remove Saguenay and upstream saguaney
  pts$rel1 <- units::set_units(pts$rel1, NULL)
  # pts$rel2 <- units::set_units(pts$rel2, NULL)

  # load('./Data/Biotic/Biotic.RData')
  # load('./Data/FormatData/idBiotic.RData')
  # rich <- sum(biotic, na.rm = TRUE)
  # values(rich)[!idBiotic] <- NA
  #
  # load('./Data/Drivers/Drivers.RData')
  # cume <- sum(drivers)
  # values(cume)[!idBiotic] <- NA


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Figure
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # # Output
  # out <- here::here("figures")
  # rcea::chk_create(out)
  #
  # png(
  #   here::here(out, "ncea.png"),
  #   res = 300,
  #   width = 300,
  #   height = 225,
  #   units = "mm"
  # )

  # png('./Figures/cumulative_effects.png', width = 300, height = 225, res = 300, units = 'mm')
  # png('./Figures/cumulative_effects.png', width = 425, height = 225, res = 300, units = 'mm')
  # layout(matrix(c(1,2,1,3), ncol = 2, byrow = TRUE), widths = c(.7, .3))
  # Figure cumualtive effects
  par(bg = "#ffffff", family = "serif")
  plotEGSL(
    layers = c("egslSimple"),
    cols = c("#00000000"),
    borders = c("#00000000"),
    lwds = c(.75),
    mar = c(4, 0, 4, 0),
    box = FALSE,
    axes = FALSE,
    northArrow = F,
    prj = prj,
    extent = st_bbox(egslSimple) + c(-21000, -21000, 21000, 21000)
  )
  image(ci, col = palImpact(100), add = T)
  # image(ci, col = viridis::viridis(100), add = T)
  plot(st_geometry(egslSimple), col = "transparent", border = focus, lwd = .75, add = T)
  # Add regions
  plot(st_geometry(egslSub),
    col = "transparent",
    border = paste0("#ffffff", "44"),
    lwd = 2,
    add = TRUE
  )


  # Modify geometries
  st_geometry(pts)[3] <- st_geometry(pts)[3] + c(-7500, 0)
  st_geometry(pts)[4] <- st_geometry(pts)[4] + c(0, 30000)
  st_geometry(pts)[6] <- st_geometry(pts)[6] + c(0, 10000)
  st_geometry(pts)[8] <- st_geometry(pts)[8] + c(40000, -10000)
  st_geometry(pts)[9] <- st_geometry(pts)[9] + c(90000, -17000)
  st_geometry(pts)[11] <- st_geometry(pts)[11] + c(30000, 0)
  st_geometry(pts)[15] <- st_geometry(pts)[15] + c(0, 15000)
  st_geometry(pts)[21] <- st_geometry(pts)[21] + c(5000, 10000)
  # Plot lines

  # Locate manually
  # plot(egslOutline)
  # manloc <- locator()
  # extPt <- data.frame(X = manloc[[1]], Y = manloc[[2]])


  # Plot geometries individually
  x <- st_coordinates(pts)
  x <- as.data.frame(x)
  lineDF <- data.frame(
    X1 = x$X,
    Y1 = x$Y,
    X2 = numeric(nrow(x)),
    Y2 = numeric(nrow(x)),
    Names = character(nrow(x)),
    nameX = numeric(nrow(x)),
    nameY = numeric(nrow(x)),
    ptX = numeric(nrow(x)),
    ptX2 = numeric(nrow(x)),
    ptY = numeric(nrow(x)),
    rel1 = pts$rel1,
    # rel2 = pts$rel2,
    col1 = palReg(101)[(pts$rel1 * 100) + 1],
    # col2 = palReg2(101)[(pts$rel2*100)+1],
    stringsAsFactors = FALSE
  )

  # Names
  lineDF$Names <- c(
    "Lower North \nShore",
    "Western \nNewfoundland",
    "Esquiman \nChannel",
    "Southwestern \nNewfoundland",
    "Middle North \nShore",
    "Upper North \nShore",
    "Jacques Cartier \nStrait",
    "Northern Anticosti \nIsland",
    "Southern Anticosti \nIsland",
    "Lower Laurentian \nChannel",
    "Magdalen \nIslands",
    "Honguedo \nStrait",
    "Upper Laurentian \nChannel",
    "Southern Gaspé \nWaters",
    "Northern Gaspé \nWaters",
    "Prince Edward \nIsland",
    "Downstream Middle \nEstuary",
    "Upstream Middle \nEstuary",
    "Downstream Lower \nEstuary",
    "Southern Lower \nEstuary",
    "Northern Lower \nEstuary"
  )

  # Lines
  lineDF[1, c("X2", "Y2")] <- c(lineDF$X1[1] - 20000, lineDF$Y1[1] + 70000)
  lineDF[2, c("X2", "Y2")] <- c(lineDF$X1[2] + 90000, lineDF$Y1[2] + 100000)
  lineDF[3, c("X2", "Y2")] <- c(lineDF$X1[3] + 180000, lineDF$Y1[3] + 180000)
  lineDF[4, c("X2", "Y2")] <- c(lineDF$X1[4] + 90000, lineDF$Y1[4] + 110000)
  lineDF[9, c("X2", "Y2")] <- c(lineDF$X1[9] + 240000, lineDF$Y1[9] - 8000)
  lineDF[10, c("X2", "Y2")] <- c(lineDF$X1[10] + 120000, lineDF$Y1[10] - 30000)
  lineDF[11, c("X2", "Y2")] <- c(lineDF$X1[11] + 120000, lineDF$Y1[11] - 30000)
  lineDF[5, c("X2", "Y2")] <- c(lineDF$X1[5] - 90000, lineDF$Y1[5] + 100000)
  lineDF[6, c("X2", "Y2")] <- c(lineDF$X1[6] - 70000, lineDF$Y1[6] + 40000)
  lineDF[16, c("X2", "Y2")] <- c(lineDF$X1[16] + 200000, lineDF$Y1[16] - 40000)
  lineDF[17, c("X2", "Y2")] <- c(lineDF$X1[17], lineDF$Y1[17] - 70000)
  lineDF[18, c("X2", "Y2")] <- c(lineDF$X1[18], lineDF$Y1[18] - 90000)
  lineDF[19, c("X2", "Y2")] <- c(lineDF$X1[19], lineDF$Y1[19] - 50000)
  lineDF[20, c("X2", "Y2")] <- c(lineDF$X1[20], lineDF$Y1[20] - 60000)
  lineDF[15, c("X2", "Y2")] <- c(lineDF$X1[15] - 35000, lineDF$Y1[15] - 25000)
  lineDF[13, c("X2", "Y2")] <- c(lineDF$X1[13], lineDF$Y1[13] + 100000)
  lineDF[21, c("X2", "Y2")] <- c(lineDF$X1[21] - 70000, lineDF$Y1[21] + 40000)
  lineDF[7, c("X2", "Y2")] <- c(lineDF$X1[7] + 40000, lineDF$Y1[7] + 150000)
  lineDF[8, c("X2", "Y2")] <- c(lineDF$X1[8] - 90000, lineDF$Y1[8] + 120000)
  lineDF[12, c("X2", "Y2")] <- c(lineDF$X1[12] - 250000, lineDF$Y1[12] - 200000)
  lineDF[14, c("X2", "Y2")] <- c(lineDF$X1[14] - 170000, lineDF$Y1[14] - 200000)

  # Name positions
  v <- 20000
  v2 <- 50000
  h <- 5000
  lineDF[1, c("nameX", "nameY")] <- c(lineDF$X2[1], lineDF$Y2[1] + v2 - 5000)
  lineDF[2, c("nameX", "nameY")] <- c(lineDF$X2[2] + h, lineDF$Y2[2])
  lineDF[3, c("nameX", "nameY")] <- c(lineDF$X2[3] + h, lineDF$Y2[3])
  lineDF[4, c("nameX", "nameY")] <- c(lineDF$X2[4] + h, lineDF$Y2[4])
  lineDF[5, c("nameX", "nameY")] <- c(lineDF$X2[5], lineDF$Y2[5] + v2)
  lineDF[6, c("nameX", "nameY")] <- c(lineDF$X2[6], lineDF$Y2[6] + v2)
  lineDF[7, c("nameX", "nameY")] <- c(lineDF$X2[7], lineDF$Y2[7] + v2)
  lineDF[8, c("nameX", "nameY")] <- c(lineDF$X2[8], lineDF$Y2[8] + v2)
  lineDF[9, c("nameX", "nameY")] <- c(lineDF$X2[9] + h, lineDF$Y2[9])
  lineDF[10, c("nameX", "nameY")] <- c(lineDF$X2[10] + h, lineDF$Y2[10])
  lineDF[11, c("nameX", "nameY")] <- c(lineDF$X2[11] + h, lineDF$Y2[11])
  lineDF[12, c("nameX", "nameY")] <- c(lineDF$X2[12], lineDF$Y2[12] - v)
  lineDF[13, c("nameX", "nameY")] <- c(lineDF$X2[13], lineDF$Y2[13] + v2)
  lineDF[14, c("nameX", "nameY")] <- c(lineDF$X2[14], lineDF$Y2[14] - v)
  lineDF[15, c("nameX", "nameY")] <- c(lineDF$X2[15], lineDF$Y2[15] - v)
  lineDF[16, c("nameX", "nameY")] <- c(lineDF$X2[16] + h, lineDF$Y2[16])
  lineDF[17, c("nameX", "nameY")] <- c(lineDF$X2[17], lineDF$Y2[17] - v)
  lineDF[18, c("nameX", "nameY")] <- c(lineDF$X2[18], lineDF$Y2[18] - v)
  lineDF[19, c("nameX", "nameY")] <- c(lineDF$X2[19], lineDF$Y2[19] - v)
  lineDF[20, c("nameX", "nameY")] <- c(lineDF$X2[20], lineDF$Y2[20] - v)
  lineDF[21, c("nameX", "nameY")] <- c(lineDF$X2[21], lineDF$Y2[21] + v2)

  # Points X position
  h <- lineDF$rel1 * 20000
  lineDF$ptX <- lineDF$nameX + h
  h2 <- lineDF$rel1 * 20000
  lineDF$ptX2 <- lineDF$nameX + 2 * h + h2 + 10000
  lineDF$ptX <- rowSums(data.frame(x = lineDF$nameX, y = h))
  v <- apply(data.frame(h, h2), 1, max)
  lineDF$ptY <- lineDF$nameY - v - 20000

  # plotEGSL()
  # plot(st_geometry(pts), add = T, col = '#84faf899', border = 'transparent', pch = 20, cex = 1)
  # text(x = x[,1], y = x[,2], labels = 1:nrow(x), cex = 1.5)
  # Param
  for (i in 1:nrow(lineDF)) {
    lines(x = c(lineDF$X1[i], lineDF$X2[i]), y = c(lineDF$Y1[i], lineDF$Y2[i]), lwd = 2, col = colLines2)
    text(x = lineDF$nameX[i], y = lineDF$nameY[i], labels = lineDF$Names[i], cex = .85, adj = c(0, .5), font = 3)
    points(x = lineDF$ptX[i], y = lineDF$ptY[i], cex = (lineDF$rel1[i] * 7), adj = 1, pch = 20, col = lineDF$col1[i])
    # points(x = lineDF$ptX2[i], y = lineDF$ptY[i], cex = (lineDF$rel2[i] * 7), adj = 1, pch = 20, col = lineDF$col2[i])
  }


  # Plot points
  plot(st_geometry(pts), add = T, col = "#ffffff33", border = "transparent", pch = 20, cex = 1.5)
  plot(st_geometry(pts), add = T, col = "#ffffff99", border = "transparent", pch = 20, cex = 1)



  # ----------------------------------------------#
  # --------------      Legends     --------------#
  # ----------------------------------------------#

  # Legends
  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]

  # Letters
  # text(x = xmin+(xmin*-.12), y = ymax*.965, labels = 'a', cex = 1.5, font = 2)
  # text(x = xmax-(xmin*-.05), y = ymax*.965, labels = 'b', cex = 1.5, font = 2)
  # text(x = xmax-(xmin*-.05), y = ymax*.525, labels = 'c', cex = 1.5, font = 2)


  # Rest
  xinit <- xmin + 60000 # minimum left side to write
  yinit <- ymax - 60000 # minimum upper side to write
  ygap <- 20000
  xgap <- 20000
  xl1 <- 180000
  xl2 <- 250000
  text(x = xinit, y = yinit, bquote(bold("Cumulative" ~ "effects")), cex = .9, font = 2, adj = c(0, .5))
  text(x = xinit + xl1, y = yinit, bquote(bold("Cumulative" ~ "effects" ~ "/" ~ km^2)), cex = .9, adj = c(0, .3))
  # text(x = xinit+xl1+xl2, y = yinit, bquote(bold("Anomalies"~"/"~km^2)), cex = .9, adj = c(0,.3))

  # text(x = xinit, y = yinit-ygap, TeX('$I_{c,x} = \\sum_{i=1}^{n} \\sum_{j=1}^{m} D_{i,x} * H_{j,x} * V_{i,j}$'), cex = .75, adj = c(0,.5))
  # text(x = xinit+xl1, y = yinit-ygap, TeX('$I_b * 1000^{-1}= \\sum_{x=1}^{o} I_{c,x}$'), cex = .75, adj = c(0,.5))
  # text(x = xinit+xl1+xl2, y = yinit-ygap, TeX('$I_{ba} = A_b^{-1} * \\sum_{x=1}^{o} I_{c,x}$'), cex = .75, adj = c(0,.5))

  # Cumulative impacts legend
  x <- seq(from = xinit, to = xinit + 130000, by = 1000)
  z <- data.frame(
    y1 = yinit - ygap / 8 - ygap / 2 - 1000,
    y2 = yinit - ygap / 8 - ygap - ygap / 2 + 1000,
    x1 = x[1:length(x) - 1],
    x2 = x[2:length(x)],
    col = palImpact(length(x) - 1),
    stringsAsFactors = FALSE
  )
  for (k in 1:nrow(z)) {
    polygon(
      x = c(z$x1[k], z$x2[k], z$x2[k], z$x1[k], z$x1[k]),
      y = c(z$y1[k], z$y1[k], z$y2[k], z$y2[k], z$y1[k]),
      col = z$col[k],
      border = "transparent"
    )
  }

  # Regional impact legends
  cex <- seq(0.1, 7, length.out = 5)
  rel <- cex / 7

  z <- data.frame(
    y1 = rep(yinit - 1.25 * ygap, 5),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      cex = seq(0.5, 7, length.out = 5),
      rel = cex / 7,
      cex2 = cumsum(cex),
      col1 = palReg(101)[(rel * 100) + 1],
      col2 = palReg2(101)[(rel * 100) + 1],
      x1 = xl1 + xinit + (cex2 * 10000) / 1.3,
      x2 = xl1 + xl2 + xinit + (cex2 * 10000) / 1.3
    )
  z$x2[1] <- z$x2[1] - 3000
  z$x1[1] <- z$x1[1] - 3000
  for (k in 1:5) points(z$x1[k], z$y[k], cex = z$cex[k], col = z$col1[k], adj = .5, pch = 20)
  # for(k in 1:5) points(z$x2[k], z$y[k], cex = z$cex[k], col = z$col2[k], adj = .5, pch = 20)

  # Add axes
  # Cumulative impacts
  n <- 5
  x <- seq(from = xinit, to = xinit + 130000, length.out = n)
  y <- yinit - ygap / 4 - ygap - ygap / 2 + 1000
  lines(x = c(xinit, xinit + 130000), y = rep(y, 2))
  for (i in 1:n) lines(x = rep(x[i], 2), y = c(y, y - 2500))
  text(
    x = x,
    y = rep(y - 5000, n),
    labels = round(
      seq(
        from = floor(min(values(ci), na.rm = TRUE)),
        to = ceiling(max(values(ci), na.rm = TRUE)),
        length.out = n
      ),
      0
    ),
    cex = .6,
    adj = c(.5, 1)
  )

  # Regional assessment
  # text(x = z$x2,
  #      y = z$y-12000-(z$cex2*1500)/1.3,
  #      labels = round(seq(from = 0, to = max(as.numeric(pts$Anomalieskm2)), length.out = 5), 2),
  #      cex = .6, adj = c(.5,.5))


  text(
    x = z$x1,
    y = z$y - 12000 - (z$cex2 * 1500) / 1.3,
    labels = round(seq(from = 0, to = max(as.numeric(pts$Ickm2)), length.out = 5), 0),
    cex = .6, adj = c(.5, .5)
  )


  # #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # # Figure cumulative exposure
  # par(bg = '#ffffff')
  # plotEGSL(layers     = c('egslSimple'),
  #          cols       = c('#00000000'),
  #          borders    = c('#00000000'),
  #          lwds       = c(.75),
  #          mar        = c(2,0,2,0),
  #          box        = FALSE,
  #          axes       = FALSE,
  #          northArrow = F,
  #          prj        = prj,
  #          extent     = st_bbox(egslSimple)+c(-21000,-21000,21000,21000))
  # image(cume, col = palImpact(100), add = T)
  # plot(st_geometry(egslSimple), col = 'transparent', border = focus, lwd = .75, add = T)
  # legendEGSL(range = c(0, maxValue(cume)), pal = palImpact, cexMain = .9, cexSub = .65, n = 5,
  #            mainTitle = 'Cumulative exposure', subTitle = 'Sum of standardized stressor intensity')
  #
  #
  # #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # # Figure taxa richness
  # par(bg = '#ffffff')
  # plotEGSL(layers     = c('egslSimple'),
  #          cols       = c('#00000000'),
  #          borders    = c('#00000000'),
  #          lwds       = c(.75),
  #          mar        = c(2,0,2,0),
  #          box        = FALSE,
  #          axes       = FALSE,
  #          northArrow = F,
  #          prj        = prj,
  #          extent     = st_bbox(egslSimple)+c(-21000,-21000,21000,21000))
  # image(rich, col = palDist2(100), add = T)
  # plot(st_geometry(egslSimple), col = 'transparent', border = focus, lwd = .75, add = T)
  # legendEGSL(range = c(0, maxValue(rich)), pal = palDist2, cexMain = .9, cexSub = .65, n = 5,
  #            mainTitle = 'Taxa richness')


  # Letter
  pos <- par("usr")
  text(x = pos[1] + (pos[2] - pos[1]) * 0.035, y = pos[4] - (pos[4] - pos[3]) * 0.015, labels = "A", cex = 1.5, font = 2)

  # dev.off()
}