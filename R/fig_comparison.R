#' Comparison of results per km2 between ncea and ncea
#'
#' @export

fig_comparison <- function() {
  # Function
  summ <- function(dat) {
    tmp <- dplyr::select(dat, -species) |>
      rowSums()
    data.frame(
      species = dat$species,
      total = tmp
    )
  }

  # Data
  fold <- here::here("output", "cea_km2")
  cea_km <- read.csv(here::here(fold, "cea_km2.csv")) |>
    summ() |>
    dplyr::rename(cea = total)
  ncea_km <- read.csv(here::here(fold, "ncea_km2.csv")) |>
    summ() |>
    dplyr::rename(ncea = total)
  cekm <- dplyr::left_join(cea_km, ncea_km, by = "species") |>
    dplyr::rename(Taxa = species) |>
    dplyr::filter(!Taxa %in% c("Zooplankton", "Phytoplankton"))
  param()

  # # Trophic sensitivity (whoops!)
  # cekm$trophic_sensitivity <- .4

  # # Species distributions
  # load(here::here("data","FormatData","bt.RData"))
  # hr <- data.frame(
  #   species = colnames(bt),
  #   area = colSums(bt, na.rm = TRUE)
  # ) |>
  # dplyr::mutate(area2 = log((area / max(area)) + 1))
  # rownames(hr) <- NULL
  # cekm <- dplyr::left_join(cekm, hr, by = c("Taxa" = "species"))
  #
  # # Mechanisms
  # mec <- read.csv(here::here("output","cea_mechanisms","mechanisms.csv"))
  # mec$cor <- abs(mec$cor - 1)
  # cekm <- dplyr::left_join(cekm, mec, by = c("Taxa" = "species"))

  # Number of interactions per species
  load("Data/FormatData/metaweb.RData")
  degree <- data.frame(
    species = colnames(metaweb),
    degree = rowSums(metaweb) + colSums(metaweb)
  )
  rownames(degree) <- NULL
  cekm <- dplyr::left_join(cekm, degree, by = c("Taxa" = "species")) |>
    # dplyr::mutate(cex = log(((degree + 1) / max(degree)) + 1))
    dplyr::mutate(cex = (degree * 0.02) + .5)

  # # Distance to identity line (residuals)
  # dist2d <- function(x,y) {
  #  pt <- c(x,y)
  #  ori <- c(0,0)
  #  end <- c(max(cekm$cea),max(cekm$ncea))
  #  v1 <- ori - end
  #  v2 <- pt - ori
  #  m <- cbind(v1,v2)
  #  abs(det(m))/sqrt(sum(v1*v1))
  # }
  # cekm <- cekm |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(res = dist2d(cea,ncea))
  # cor(cekm$degree, cekm$res, method = "spearman")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Figure
  out <- here::here("figures")
  rcea::chk_create(out)

  png(here::here(out, "comparison.png"), res = 300, width = 300, height = 225, units = "mm")
  par(family = "serif")

  # Plot taxa
  # Add color to data frame
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n]
  }

  cols <- c("#000000", gg_color_hue(2))
  cols[2] <- darken(cols[2], 20)
  cols[3] <- darken(cols[3], 20)
  cols <- paste0(cols, "77")
  cekm$col <- cols[1]
  cekm$col[cekm$Taxa %in% fish] <- cols[3]

  # Plot
  par(mar = c(4.5, 4.5, 2, 2))
  plot0(x = c(0, max(cekm$cea, na.rm = TRUE)), y = c(0, max(cekm$ncea, na.rm = TRUE)))
  axis(1)
  axis(2)
  mtext(side = 1, text = bquote(bold("Species-scale" ~ "cumulative" ~ "effects" ~ "/" ~ km^2)), line = 2.5, font = 2)
  mtext(side = 2, text = bquote(bold("Network-scale" ~ "cumulative" ~ "effects" ~ "/" ~ km^2)), line = 2.5, font = 2)

  # WARNING: 1:1 line, to review this does not make any sense to me anymore
  uid <- cekm$cea == max(cekm$cea, na.rm = TRUE)
  lines(x = c(0, cekm$cea[uid]), y = c(0, cekm$ncea[uid]), lty = 2, col = "#EEB95699", lwd = 3)
  lines(x = c(0, max(cekm$cea)), y = c(0, max(cekm$cea)), lwd = 3, lty = 3, col = "#00000066")
  # lines(x = c(0, max(cekm$cea)), y = c(0, max(cekm$ncea)), lwd = 3, lty = 3, col = "#00000066")
  # text(x = 2.45, y = 3.13, labels = "Relative 1:1 line", adj = c(0, .5), srt = 35, col = "#000000CC")
  text(x = 2.45, y = 2.35, labels = "1:1 line", adj = c(0, .5), srt = 30, col = "#000000CC")
  text(x = 2.7, y = 1.25, labels = "Direct effects baseline", adj = c(1, .5), srt = 15, col = "#EEB956")

  # Weight to make points bigger
  amp <- 2

  # Regular points
  uid <- !cekm$Taxa %in% fish | cekm$Taxa %in% endangered | cekm$Taxa %in% threatened | cekm$Taxa %in% concern
  points(x = cekm$cea[uid], y = cekm$ncea[uid], pch = 20, cex = cekm$cex[uid] * amp, col = cekm$col[uid])

  # Fisheries
  uid <- cekm$Taxa %in% fish
  points(x = cekm$cea[uid], y = cekm$ncea[uid], pch = 20, cex = cekm$cex[uid] * amp, col = cekm$col[uid])
  uid <- cekm$Taxa == "Mallotus.villosus"

  # Species at risk
  # Concern
  uid <- cekm$Taxa %in% concern
  points(x = cekm$cea[uid], y = cekm$ncea[uid], pch = 20, cex = cekm$cex[uid] * amp, col = paste0(colCon, "77"))

  # Threatened
  uid <- cekm$Taxa %in% threatened
  points(x = cekm$cea[uid], y = cekm$ncea[uid], pch = 20, cex = cekm$cex[uid] * amp, col = paste0(colThr, "BB"))

  # Endangered
  uid <- cekm$Taxa %in% endangered
  points(x = cekm$cea[uid], y = cekm$ncea[uid], pch = 20, cex = cekm$cex[uid] * amp, col = paste0(colEnd, "99"))


  # Legend
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  z <- data.frame(
    y = rep(ymax * .95, 5),
    cex = seq(0.3, 4, length.out = 5)
  ) %>%
    mutate(
      cex2 = cumsum(cex),
      rel = round(cex / 6, 1),
      col = "#00000066",
      x = (xmin + .023) + (cex2 / 160)
    )
  z$x[2] <- z$x[2] + .002
  z$x[3] <- z$x[3] + .001
  z$x[4] <- z$x[4] - .002
  z$x[5] <- z$x[5] - .008
  # for(k in 1:nrow(z)) points(z$x[k], z$y[k], cex = z$cex[k], col = z$col[k], adj = .5, pch = 20)
  # text(x = z$x, y = (z$y-.02-(z$cex2)/300) - c(.0005,.0005,.005,.0075,.005), labels = z$rel, cex = .6, adj = c(.5,.5))
  deg <- seq(0, max(cekm$degree), length.out = 5)
  cex <- ((deg * 0.02) + .5) * amp
  x <- c(.025, .12, .26, .45, .7)
  points(x = (xmin + x) * .5, y = rep(ymax * .94, 5), cex = cex, col = cols[1], pch = 20)
  text(x = (xmin + .025) * .5, y = ymax * .98, labels = "Number of interactions", cex = 1, font = 1, adj = c(0, .5))
  y <- c(.925, .92, .915, .91, .905)
  text(x = (xmin + x) * .5, y = ymax * y, labels = deg, cex = .8, font = 1, adj = .5, col = "#000000")

  points(x = (xmin + .025) * .5, y = ymax * .87, pch = 20, cex = 2, col = cols[3])
  points(x = (xmin + .025) * .5, y = ymax * .83, pch = 20, cex = 2, col = paste0(colEnd, "77"))
  points(x = (xmin + .025) * .5, y = ymax * .79, pch = 20, cex = 2, col = paste0(colThr, "77"))
  points(x = (xmin + .025) * .5, y = ymax * .75, pch = 20, cex = 2, col = paste0(colCon, "77"))
  text(x = (xmin + .04) * .05, y = ymax * .87, labels = "Commercially exploited", cex = 1, font = 1, adj = c(0, .5))
  text(x = (xmin + .04) * .05, y = ymax * .83, labels = "Endangered", cex = 1, font = 1, adj = c(0, .5))
  text(x = (xmin + .04) * .05, y = ymax * .79, labels = "Threatened", cex = 1, font = 1, adj = c(0, .5))
  text(x = (xmin + .04) * .05, y = ymax * .75, labels = "Concern", cex = 1, font = 1, adj = c(0, .5))


  # Names to put on the graph (manual and ugly)
  # text(x = cekm$cea[uid], y = cekm$ncea[uid], labels = cekm$Taxa[uid], adj = c(0,.5), cex = .75, col = '#858585')

  dat <- data.frame(
    Taxa = c(
      "Mallotus.villosus", "Chionoecetes.opilio", "Hippoglossus.hippoglossus", "Lophius.americanus",
      "Gadus.morhua", "Chlamys.islandica", "Strongylocentrotus.sp.", "Balaenoptera.physalus",
      "Delphinapterus.leucas", "Anarhichas.minor", "Cucumaria.frondosa", "Mytilus.sp.",
      "Psolus.fabricii", "Actinauge.sp.", "Astarte.sp.", "Psolus.phantapus",
      "Gorgonocephalus.sp.", "Cyanea.capillata"
    ),
    stringsAsFactors = FALSE
  )
  dat$Names <- gsub("\\.", " ", dat$Taxa)
  dat$Names[dat$Taxa == "Strongylocentrotus.sp."] <- "Strongylocentrotus sp"
  dat <- left_join(dat, cekm[, c("Taxa", "cea", "ncea")], by = "Taxa")

  # Adjust position
  dat$x <- dat$cea
  dat$y <- dat$ncea
  # -----
  dat[1, 3] <- (dat[1, 3] + .005) * 1.2
  dat[1, 4] <- (dat[1, 4] - .05) * .95
  dat[2, 3] <- (dat[2, 3] + .005) * 1.1
  dat[2, 4] <- (dat[2, 4]) * 1.06
  dat[3, 3] <- (dat[3, 3] + .005) * 1.05
  dat[3, 4] <- (dat[3, 4] - .045) * .85

  dat[4, 3] <- (dat[4, 3] + .004) * 1.03
  dat[4, 4] <- (dat[4, 4] + .05)

  dat[5, 3] <- (dat[5, 3] + .005) * 1.14
  dat[6, 3] <- (dat[6, 3] + .007) * 1.03
  dat[6, 4] <- (dat[6, 4] - .05) * 1
  dat[7, 3] <- (dat[7, 3] + .007) * 1.03

  dat[8, 3] <- (dat[8, 3] + .01) * 1.1
  dat[8, 4] <- (dat[8, 4] + .09) * .91

  dat[9, 4] <- (dat[9, 4] + .07) * 1.06

  dat[10, 3] <- (dat[10, 3] + .012) * 1.9
  dat[10, 4] <- (dat[10, 4] - .01) * .9

  dat[11, 3] <- (dat[11, 3] + .007) * 1.03
  dat[12, 3] <- (dat[12, 3] + .007) * 1.03
  dat[13, 3] <- (dat[13, 3] + .007) * 1.03
  dat[14, 3] <- (dat[14, 3] + .007) * 1.03
  dat[15, 3] <- (dat[15, 3] + .007) * 1.03
  dat[16, 3] <- (dat[16, 3] + .007) * 1.03
  dat[17, 3] <- (dat[17, 3] + .007) * 1.03
  dat[18, 3] <- (dat[18, 3] + .007) * 1.03

  # dat <- dat[1,]
  for (i in 1:nrow(dat)) {
    coords <- matrix(data = dat[i, c(3:6)], nrow = 2, byrow = TRUE)
    lines(x = coords[, 1], y = coords[, 2], col = "#EEB956", lwd = .75)
  }
  text(x = dat$cea + .002, y = dat$ncea, labels = dat$Names, adj = c(0, .5), cex = .75, col = "#404040")


  dev.off()
}
