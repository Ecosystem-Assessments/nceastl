#' Figure of contribution of species to indirect effects of other species
#'
#' @export
fig_species_contribution <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Libraries
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  library(raster)
  library(tidyverse)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  dat <- read.csv("output/cea_species_contribution/species_contribution_network.csv")
  rownames(dat) <- colnames(dat)
  # contributing_to <- colSums(dat)/sum(dat)
  # contributing_to <- contributing_to / max(contributing_to)
  contributing_to <- colSums(dat)
  # affected_by <- rowSums(dat)/sum(dat)
  # affected_by <- affected_by / max(affected_by)
  affected_by <- rowSums(dat)
  contr <- data.frame(
    species = names(contributing_to),
    contribution = contributing_to,
    affected = affected_by
  )
  dat <- apply(dat, 2, function(x) x / sum(x)) # Percent
  # dat <- dat / max(dat) # Full relative

  # Get colors by species group
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n] |>
      lapply(graphicsutils::darken, percentage = 10) |>
      unlist()
  }

  param()
  # Species list
  colInv <- "#226c61"
  colVer <- "#6a5024"
  spGroup <- dplyr::select(spList, gr1, gr2) |>
    dplyr::distinct() |>
    dplyr::arrange(gr1, gr2) |>
    # dplyr::mutate(col = ifelse(gr1 == "Invertebrates", colInv, colVer)) |>
    dplyr::mutate(col = gg_color_hue(dplyr::n())) |>
    dplyr::select(-gr1)

  sp <- dplyr::left_join(spList, spGroup, by = "gr2") |>
    dplyr::select(shortname, gr1, gr2, col) |>
    dplyr::left_join(contr, by = c("shortname" = "species")) |>
    dplyr::arrange(gr1, gr2, contribution)

  # Reorder data
  uid <- lapply(sp$shortname, function(x) which(colnames(dat) == x)) |> unlist()
  dat <- dat[uid, uid]
  nSp <- ncol(dat)

  # Groups
  gr1 <- sp[, "gr1", drop = FALSE] |>
    dplyr::mutate(id = 1:dplyr::n()) |>
    dplyr::group_by(gr1) |>
    dplyr::summarize(min = min(id), max = max(id)) |>
    as.data.frame(stringsAsFactors = FALSE)

  gr2 <- sp[, "gr2", drop = FALSE] |>
    dplyr::mutate(id = 1:dplyr::n()) |>
    dplyr::group_by(gr2) |>
    dplyr::summarize(min = min(id), max = max(id)) |>
    as.data.frame(stringsAsFactors = FALSE)
  gr2$gr2 <- gsub("Others2", "Others", gr2$gr2)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Point graph
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  out <- here::here("figures")
  rcea::chk_create(out)

  bg <- "#ffffff"
  png(
    here::here(out, glue::glue("species_contribution.png")),
    res = 300,
    width = 300,
    height = 300,
    units = "mm"
  )


  par(family = "serif")
  par(mar = c(1, 1, 1, 1))
  xR <- c(-25, nrow(dat))
  yR <- c(-25, nrow(dat))
  graphicsutils::plot0(x = xR, y = yR)
  text(y = nrow(dat) + 5, x = nrow(dat) / 2, labels = "Contribution to indirect effects", font = 2)
  text(x = nrow(dat) + 5, y = nrow(dat) / 2, labels = "Indirect effects", font = 2, srt = -90)
  # for(i in 1:nrow(dat)) lines(x = c(i,i), y = c(0.5,yR[2]+3), lty = 2, lwd = 1.5, col = "#909090")
  # for(i in 1:nrow(dat)) lines(x = c(0.75,xR[2]+.25), y = c(i,i), lty = 2, lwd = 1.5, col = "#90909066")

  for (j in 1:nSp) {
    for (i in 1:nSp) {
      points(
        x = j,
        y = i,
        cex = log(dat[i, j] + 1) * 10,
        col = sp$col[i],
        bg = glue::glue("{sp$col[i]}99"),
        pch = 21
      )
    }
  }

  # Affected by
  points(x = 1:nSp, y = rep(-3, nSp), cex = log(sp$contribution + 1), col = "#7a9f6a", bg = "#7a9f6a99", pch = 21)

  # Contributing to
  points(x = rep((-3), nSp), y = 1:nSp, cex = sp$affected, col = "#7a9f6a", bg = "#7a9f6a99", pch = 21)

  # Text
  text(x = 1:nSp, y = rep(-6, nSp), labels = sp$shortname, srt = 90, adj = c(1, .5), cex = .3)
  text(x = -6, y = 1:nSp, labels = sp$shortname, srt = 0, adj = c(1, .5), cex = .3)
  text(x = -6, y = -3, labels = "Contribution to indirect effects", font = 2, srt = 0, adj = c(1, .5), cex = .3)
  text(x = -3, y = -6, labels = "Total indirect effects", font = 2, srt = 90, adj = c(1, .5), cex = .3)

  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Taxonomic groups side = 2
  # 1st group
  xMin <- -28
  for (i in 1:nrow(gr1)) {
    y <- as.numeric(gr1[i, c("min", "max")])
    lines(x = rep(xMin + .1, 2), y = y)
    lines(y = rep(xMin + .1, 2), x = y)
    text(x = xMin - 3, y = mean(y), adj = .5, font = 2, labels = gr1$gr1[i], srt = 90)
    text(y = xMin - 3, x = mean(y), adj = .5, font = 2, labels = gr1$gr1[i])
  }

  # 2nd group
  for (i in 1:nrow(gr2)) {
    y <- as.numeric(gr2[i, c("min", "max")])
    lines(x = rep(xMin + 6, 2), y = y)
    lines(y = rep(xMin + 6, 2), x = y)
    text(x = xMin + 3, y = mean(y), adj = .5, font = 1, labels = gr2$gr2[i], cex = .8, srt = 90)
    text(y = xMin + 3, x = mean(y), adj = .5, font = 1, labels = gr2$gr2[i], cex = .8)
  }

  dev.off()
}