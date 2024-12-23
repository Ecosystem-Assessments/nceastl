#' Figure of contribution of direct and indirect effects of stressors and of taxonomic groups to indirect effects of stressors to taxonomic groups
#'
#' @export
fig_contribution <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Libraries
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  library(raster)
  library(tidyverse)
  param()

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors for stressors
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n] |>
      lapply(graphicsutils::darken, percentage = 10) |>
      unlist()
  }
  add_alpha <- function(alpha) {
    as.hexmode(as.integer(alpha))
  }

  # Colors for species
  cols_species <- function(n) {
    viridis::cividis(n) |>
      substr(1, 7)
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Stressors & species
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Driver groups
  drGroup <- grNames |>
    dplyr::mutate(cols = gg_color_hue(dplyr::n())) |>
    dplyr::rename(group = name)

  # Drivers list
  drList <- drNames |>
    dplyr::rename(drivers = var) |>
    dplyr::left_join(drGroup, by = c("group" = "accr")) |>
    dplyr::select(-group, group = group.y)

  # Number of drivers
  nDrGroup <- nrow(drGroup)

  # Species list
  spGroup <- dplyr::select(spList, shortname, gr1, gr2) |>
    dplyr::distinct() |>
    dplyr::group_by(gr1, gr2) |>
    dplyr::arrange(gr1, gr2, shortname) |>
    dplyr::mutate(grid = cur_group_id()) |>
    dplyr::mutate(cols = cols_species(8)[grid]) |>
    dplyr::select(gr1, gr2, shortname, cols) |>
    dplyr::ungroup()

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Stressor contribution
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Impacts per km2 for stressors
  # cekm <- readRDS('./Data2/Results/Cumulative_Effects/CumulativeEffects_Area.rds')
  cekm_imp <- function(f, type) {
    tmp <- read.csv(f) |>
      tidyr::pivot_longer(cols = -c("species"), names_to = "drivers", values_to = "effect") |>
      dplyr::left_join(drList, by = "drivers") |>
      dplyr::left_join(spList, by = c("species" = "shortname")) |>
      dplyr::select(species, drivers, effect, group, cols, gr1, gr2) |>
      dplyr::group_by(group, gr1, gr2, cols) |>
      dplyr::summarize(effect = sum(effect)) |>
      dplyr::ungroup() |>
      dplyr::select(group, gr1, gr2, cols, effect) |>
      dplyr::mutate(type = type)
  }

  cekm <- dplyr::bind_rows(
    cekm_imp("output/cea_km2/ncea_direct_km2.csv", "b_direct"), # b_ and _a are for order on graph
    cekm_imp("output/cea_km2/ncea_indirect_km2.csv", "a_indirect") # quickfix
  )

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Species contribution
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  spc <- read.csv("output/cea_species_contribution/species_contribution.csv") |>
    dplyr::filter(!vc_from %in% c("Zooplankton", "Phytoplankton")) |>
    dplyr::mutate(effect = rowSums(dplyr::across(dplyr::where(is.numeric)))) |>
    dplyr::select(vc, vc_from, effect) |>
    dplyr::left_join(spGroup, by = c("vc_from" = "shortname")) |>
    dplyr::select(vc, vc_from, effect, gr2_from = gr2, gr1_from = gr1, cols) |>
    dplyr::left_join(spGroup[c("gr1", "gr2", "shortname")], by = c("vc" = "shortname")) |>
    dplyr::select(gr1, gr2, gr1_from, gr2_from, effect, cols) |>
    dplyr::group_by(gr1, gr2, gr1_from, gr2_from, cols) |>
    dplyr::summarize(effect = sum(effect)) |>
    dplyr::ungroup()

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Prepare data for graph
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Stressor contribution
  xGap <- c(.20, -.20, .20, -.20)
  yGap <- c(.20, .20, -.20, -.20)
  cekm <- cekm |>
    dplyr::filter(!gr2 %in% c("Others", "Others2")) |>
    dplyr::group_by(gr1, gr2) |>
    dplyr::mutate(x = cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::group_by(type) |>
    dplyr::mutate(y = cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      x = x + xGap[cur_group_id()],
      y = y + yGap[cur_group_id()] + 2
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(cex = 6 * (effect / max(effect)))


  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Species contribution
  spc <- dplyr::mutate(spc, cex = 6 * (effect / max(effect)))
  inv <- dplyr::filter(spc, gr1_from == "Invertebrates") |> dplyr::select(-gr1_from)
  ver <- dplyr::filter(spc, gr1_from == "Vertebrates") |> dplyr::select(-gr1_from)

  # Invertebrates
  xGap <- c(.20, -.20, .20, -.20)
  yGap <- c(.20, .20, -.20, -.20)
  inv <- inv |>
    dplyr::filter(
      !gr2 %in% c("Others", "Others2") &
        !gr2_from %in% c("Others", "Others2")
    ) |>
    dplyr::group_by(gr1, gr2) |>
    dplyr::mutate(x = cur_group_id(), cex = cex, cols = cols) |>
    dplyr::ungroup() |>
    dplyr::group_by(gr2_from) |>
    dplyr::mutate(y = cur_group_id(), cex = cex, cols = cols) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      x = x + xGap[y],
      y = yGap[y] + 2
    )

  # Vertebrates
  xGap <- c(.20, -.20)
  yGap <- c(0, 0)
  ver <- ver |>
    dplyr::filter(
      !gr2 %in% c("Others", "Others2") &
        !gr2_from %in% c("Others", "Others2")
    ) |>
    dplyr::group_by(gr1, gr2) |>
    dplyr::mutate(x = cur_group_id(), cex = cex, cols = cols) |>
    dplyr::ungroup() |>
    dplyr::group_by(gr2_from) |>
    dplyr::mutate(y = cur_group_id(), cex = cex, cols = cols) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      x = x + xGap[y],
      y = yGap[y] + 1
    )


  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Overall contributions per groups
  cekmtot <- cekm |>
    dplyr::group_by(gr2, type) |>
    dplyr::summarise(effect = sum(effect)) |>
    dplyr::ungroup() |>
    dplyr::mutate(effect = (effect / max(effect)) * 150) |>
    dplyr::mutate(alpha = add_alpha(effect)) |>
    dplyr::left_join(unique(cekm[, c("gr1", "gr2")]), by = "gr2") |>
    dplyr::mutate(
      type = stringr::str_replace(type, "a_", ""),
      type = stringr::str_replace(type, "b_", "")
    ) |>
    dplyr::arrange(type, gr1, gr2) |>
    dplyr::select(type, gr2, alpha)

  # Invertebrates
  invtot <- inv |>
    dplyr::group_by(gr2) |>
    dplyr::summarise(effect = sum(effect)) |>
    dplyr::ungroup() |>
    dplyr::mutate(type = "Invertebrates")

  # Vertebrates
  vertot <- ver |>
    dplyr::group_by(gr2) |>
    dplyr::summarise(effect = sum(effect)) |>
    dplyr::ungroup() |>
    dplyr::mutate(type = "Vertebrates")

  # All taxonomic groups
  spctot <- dplyr::bind_rows(invtot, vertot) |>
    dplyr::mutate(effect = (effect / max(effect)) * 150) |>
    dplyr::mutate(alpha = add_alpha(effect)) |>
    dplyr::left_join(unique(inv[, c("gr1", "gr2")]), by = "gr2") |>
    dplyr::arrange(type, gr1, gr2) |>
    dplyr::select(type, gr2, alpha)

  # All data
  grtot <- rbind(cekmtot, spctot) |>
    dplyr::mutate(
      alpha = as.character(alpha),
      nchar = nchar(alpha),
      alpha = ifelse(nchar == 1, paste0("0", alpha), alpha)
    ) |>
    dplyr::select(-nchar) |>
    tidyr::pivot_wider(names_from = gr2, values_from = alpha) |>
    dplyr::select(-type)
  grtot <- grtot[4:1, ]

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Graph
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Output
  out <- here::here("figures")
  rcea::chk_create(out)

  bg <- "#ffffff"
  grDevices::png(
    here::here(out, glue::glue("contribution.png")),
    res = 300,
    width = 275,
    height = 300,
    units = "mm"
  )
  # layout(matrix(1:2, nrow = 2), heights = c(.88,.12))
  par(family = "serif", bg = bg)
  par(mar = c(0, 0, 0, 0))
  xR <- c(-1, 9.5)
  yR <- c(-5.5, 5)
  graphicsutils::plot0(x = xR, y = yR)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Graph elements
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # # Polygons
  yL <- 4.7
  x <- c(.55, 4.45)
  # polygon(
  #   x = c(x[1],x[1],x[2],x[2]),
  #   y = c(-1.2,rep(yL-.1, 2),-1.2),
  #   col = '#f5f4f4',
  #   border = '#00000000'
  # )

  # Lines left
  lines(x = c(-.1, 6.45), y = c(3.5, 3.5), lty = 2)
  lines(x = c(-.75, 6.45), y = c(2.5, 2.5))
  lines(x = c(-.1, 6.45), y = c(1.5, 1.5), lty = 2)

  # Text left
  cT <- .95
  text(x = 0.15, y = 4, labels = "Direct\neffects", srt = 90, adj = .5, font = 1, cex = cT)
  text(x = 0.15, y = 3, labels = "Indirect\neffects", srt = 90, adj = .5, font = 1, cex = cT)
  text(x = 0.15, y = 2, labels = "Invertebrates", srt = 90, adj = .5, font = 1, cex = cT)
  text(x = 0.15, y = 1, labels = "Vertebrates", srt = 90, adj = .5, font = 1, cex = cT)
  text(x = -.5, y = 3.5, labels = "Stressor\ncontribution", srt = 90, adj = .5, font = 2)
  text(x = -.5, y = 1.5, labels = "Taxa contribution\nto indirect effects", srt = 90, adj = .5, font = 2)

  # Taxonomic groups
  lines(y = c(yL, yL), x = x, lwd = 1.5)
  text(y = yL + .2, x = mean(x), adj = .5, font = 2, labels = "Invertebrates")
  x <- c(4.55, 6.45)
  lines(y = c(yL, yL), x = x, lwd = 1.5)
  text(y = yL + .2, x = mean(x), adj = .5, font = 2, labels = "Vertebrates")

  # Group names
  gr <- dplyr::select(spGroup, gr1, gr2) |>
    dplyr::distinct() |>
    dplyr::arrange(gr1, gr2) |>
    dplyr::filter(!gr2 %in% c("Others", "Others2")) |>
    dplyr::mutate(x = 1:dplyr::n())
  text(x = gr$x, y = rep(-.2, nrow(gr)), labels = gr$gr2, adj = c(1, .5), srt = 90, cex = 1, font = 2)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Encircle groups
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  xG <- .35
  yG <- .35
  for (y in 1:4) {
    for (x in 1:6) {
      coords <- data.frame(
        x = c(x - xG, x + xG, x + xG, x - xG),
        y = c(y - yG, y - yG, y + yG, y + yG)
      )
      graphicsutils::encircle(
        x = coords$x,
        y = coords$y,
        off.set = .55,
        border = "#00000000",
        col = glue::glue("#000000{grtot[y,x]}")
      )
    }
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Contributions
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Stressors
  points(
    x = cekm$x,
    y = cekm$y,
    pch = 21,
    col = cekm$cols,
    bg = glue::glue("{cekm$cols}99"),
    cex = cekm$cex
  )

  # Invertebrates
  points(
    x = inv$x,
    y = inv$y,
    pch = 21,
    col = inv$cols,
    bg = glue::glue("{inv$cols}99"),
    cex = inv$cex
  )

  # Invertebrates
  points(
    x = ver$x,
    y = ver$y,
    pch = 21,
    col = ver$cols,
    bg = glue::glue("{ver$cols}99"),
    cex = ver$cex
  )


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Icons
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  colimg <- "#000000BB"
  l <- .13
  # Humpback whale - Megaptera novaeangliae
  # by Chris huh
  # http://phylopic.org/image/ce70490a-79a5-47fc-afb9-834e45803ab4/
  # License https://creativecommons.org/licenses/by-sa/3.0/
  img <- png::readPNG("./img/PhyloPic.ce70490a.Chris-huh.Balaenoptera-novaeangliae.png", native = TRUE)
  pchImage(x = 6.2, y = .2 - l, obj = img, cex.x = .5, cex.y = .3, col = colimg)

  # Beluga whale - Delphinapterus leucas
  # by Xavier Giroux-Bougard
  # http://phylopic.org/image/f1367ab1-40cf-4e9a-a84b-37508f11a7c7/
  img <- png::readPNG("./img/PhyloPic.f1367ab1.Xavier-Giroux-Bougard.Delphinapterus_Delphinapterus-leucas_Monodontidae.png", native = TRUE)
  pchImage(x = 5.85, y = .4 - l, obj = img, cex.x = .5, cex.y = .25, col = colimg)

  # Atlantic cod - Gadus morhua
  # Milton Tan
  # http://phylopic.org/image/bba1800a-dd86-451d-a79b-c5944cfe5231/
  img <- png::readPNG("./img/PhyloPic.bba1800a.Milton-Tan.Gadariae_Gadidae_Gadiformes_Gadinae_Gadus_Gadus-morhua_Zeiogadaria.png", native = TRUE)
  pchImage(x = 5.2, y = .2 - l, obj = img, cex.x = .5, cex.y = .25, col = colimg)

  # Hippoglossoides
  # https://www.phylopic.org/images/b1d9363d-7ba2-4eda-9998-f45cd6d703f1/hippoglossoides-platessoides
  # Author: Nathan Hermann
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("./img/Phylopic_hippoglossoides.png", native = TRUE)
  pchImage(x = 4.85, y = .4 - l, obj = img, cex.x = .5, cex.y = .25, col = colimg)

  # Shrimp
  # by Christoph Schomburg
  # https://www.phylopic.org/images/aed2513d-2386-4218-b913-384838c0107b/penaeus
  img <- png::readPNG("img/Phylopic_shrimp.png", native = TRUE)
  pchImage(x = 1, y = .1 - l, obj = img, cex.x = .38, cex.y = .17, col = colimg)

  # Snow crab
  # Phylopic: http://phylopic.org/image/ce70234a-527a-415d-bc1a-e878fccf0e4e/
  # Author: Harold N Eyster
  # License: https://creativecommons.org/licenses/by/3.0/
  # No changes were made
  img <- png::readPNG("./img/PhyloPic.ce70234a.Harold-N-Eyster.Epialtidae_Epialtinae_Majoidea_Pugettia_Pugettia-producta.png", native = TRUE)
  pchImage(x = 0.8, y = .35 - l, obj = img, cex.x = .5, cex.y = .3, col = colimg)

  # Lobster
  # https://www.phylopic.org/images/4256e839-f525-47a2-8bb8-fd56f2623cc3/homarus-americanus
  # Author: Andy Wilson
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_lobster.png", native = TRUE)
  pchImage(x = 1.25, y = .45 - l, obj = img, cex.x = .5, cex.y = .3, col = colimg)

  # Actinauge
  # https://www.phylopic.org/images/63135016-5a17-4d54-9098-02760e5e4499/actinia-equina
  # Author: Guillaume Dera
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_actinauge.png", native = TRUE)
  pchImage(x = 2, y = .11 - l, obj = img, cex.x = .25, cex.y = .2, col = colimg)

  # Pennatula
  # https://www.phylopic.org/images/825e7087-0dcb-48f6-b626-74ea1067df25/pteroeides-spinosum
  # Author: Guillaume Dera
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_pennatula.png", native = TRUE)
  pchImage(x = 1.85, y = .4 - l, obj = img, cex.x = .1, cex.y = .32, col = colimg)

  # Cyanea capillata
  # https://www.phylopic.org/images/f2fb5773-f8ff-45b0-9c6c-cc5aef3c93d5/cyanea-capillata
  # Author: Guillaume Dera
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_cyanea_capillata.png", native = TRUE)
  pchImage(x = 2.25, y = .35 - l, obj = img, cex.x = .25, cex.y = .35, col = colimg)

  # Holothuria
  # https://www.phylopic.org/images/a62a4f3e-ca19-4ae1-aeb8-1baf1ce93be7/holothuria
  # Author: Lauren Sumner-Rooney
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_holothuria.png", native = TRUE)
  pchImage(x = 3, y = .5 - l, obj = img, cex.x = .3, cex.y = .15, col = colimg)

  # Asteroidea
  # https://www.phylopic.org/images/c3b6f519-544c-4869-afca-3c5775df73ce/asteroidea
  # Author: Fernando Carezzano
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_asteroidea.png", native = TRUE)
  pchImage(x = 2.9, y = .15 - l, obj = img, cex.x = .2, cex.y = .3, col = colimg)

  # Strongylocentrotus
  # https://www.phylopic.org/images/21f01ed3-d1ce-419c-8339-06be3c353466/strongylocentrotus-purpuratus
  # Author: Christoph Schomburg
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_strongylocentrotus.png", native = TRUE)
  pchImage(x = 3.2, y = .22 - l, obj = img, cex.x = .32, cex.y = .32, col = colimg)

  # Scallop
  # https://www.phylopic.org/images/3f29d01d-6e0a-427f-94ff-d49bf529f04f/pecten-maximus
  # Author: Guillaume Dera
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_scallop.png", native = TRUE)
  pchImage(x = 4.2, y = .18 - l, obj = img, cex.x = .25, cex.y = .28, col = colimg)

  # Buccinidae
  # https://www.phylopic.org/images/4097f50f-5076-4250-bd0f-95d150fea234/kelletia-kelletii
  # Author: Felix Vaux
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_buccinidae.png", native = TRUE)
  pchImage(x = 3.9, y = .25 - l, obj = img, cex.x = .22, cex.y = .33, col = colimg)

  # Astarte
  # https://www.phylopic.org/images/02c24f6f-4402-4047-abec-8ac82c95b39b/astarte-sulcata
  # Author: Katie S. Collins
  # License: https://creativecommons.org/publicdomain/zero/1.0/
  # No changes were made
  img <- png::readPNG("img/Phylopic_astarte.png", native = TRUE)
  pchImage(x = 4.08, y = .45 - l, obj = img, cex.x = .25, cex.y = .22, col = colimg)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Legend
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Stressors
  xS <- c(6.65, 9.35, 6.65, 9.35)
  text(y = 4, x = xS[1], labels = "Stressors (n)", adj = c(0, .5), font = 2)
  graphicsutils::encircle(
    x = xS,
    y = c(3.25, 3.25, 3.75, 3.75),
    off.set = .55,
    border = "#00000044",
    col = "#00000000"
  )

  x <- c(8.15, 7.85, 8.15, 7.85) - .15
  y <- c(3.65, 3.65, 3.35, 3.35) + .05
  xG <- .15
  points(
    x = x,
    y = y,
    pch = 21,
    col = drGroup$cols,
    bg = glue::glue("{drGroup$cols}99"),
    cex = 1.5
  )
  text(x = x[1] + xG, y = y[1], adj = c(0, .5), labels = "Climate (6)", cex = .9)
  text(x = x[2] - xG, y = y[2], adj = c(1, .5), labels = "Coastal (5)", cex = .9)
  text(x = x[3] + xG, y = y[3], adj = c(0, .5), labels = "Fisheries (5)", cex = .9)
  text(x = x[4] - xG - .22, y = y[4], adj = c(1, .5), labels = "Marine\ntraffic", cex = .9)
  text(x = x[4] - xG, y = y[4], adj = c(1, .5), labels = "(2)", cex = .9)

  # Invertebrates
  text(y = 2.5, x = xS[1], labels = "Invertebrates (n)", adj = c(0, .5), font = 2)
  graphicsutils::encircle(
    x = xS,
    y = c(1.75, 1.75, 2.25, 2.25),
    off.set = .55,
    border = "#00000044",
    col = "#00000000"
  )

  y <- c(2.15, 2.15, 1.85, 1.85)
  xG <- .15
  points(
    x = x,
    y = y,
    pch = 21,
    col = inv$cols,
    bg = glue::glue("{inv$cols}99"),
    cex = 1.5
  )
  text(x = x[1] + xG, y = y[1], adj = c(0, .5), labels = "Arthropoda (30)", cex = .9)
  text(x = x[2] - xG, y = y[2], adj = c(1, .5), labels = "Cnidaria (20)", cex = .9)
  text(x = x[3] + xG, y = y[3], adj = c(0, .5), labels = "Echinodermata (21)", cex = .9)
  text(x = x[4] - xG, y = y[4], adj = c(1, .5), labels = "Mollusca (19)", cex = .9)

  # Vertebrates
  text(y = 1.4, x = xS[1], labels = "Vertebrates (n)", adj = c(0, .5), font = 2)
  graphicsutils::encircle(
    x = xS,
    y = c(0.85, 0.85, 1.15, 1.15),
    off.set = .55,
    border = "#00000044",
    col = "#00000000"
  )

  y <- c(1, 1)
  xG <- .15
  points(
    x = x[1:2],
    y = y,
    pch = 21,
    col = ver$cols,
    bg = glue::glue("{ver$cols}99"),
    cex = 1.5
  )
  text(x = x[1] + xG, y = y[1], adj = c(0, .5), labels = "Actinopterygii (62)", cex = .9)
  text(x = x[2] - xG, y = y[2], adj = c(1, .5), labels = "Mammalia (24)", cex = .9)


  # # Titles
  # x <- seq(from = -2, by = (diff(xR)/4), length.out = 4)[2:4]
  # yT <- -1.75
  # lab <- c("Stressors","Invertebrates","Vertebrates")
  # text(y = rep(yT, 3), x = x, labels = lab, adj = c(0,.5), font = 2)
  #
  # # Subtitles
  # yG <- .25
  # y <- seq(from = yT-yG, by = -yG, length.out = 4)
  #
  # # Drivers
  # points(
  #   x = rep(x[1],4)+.1,
  #   y = y,
  #   pch = 21,
  #   col = drGroup$cols,
  #   bg = glue::glue("{drGroup$cols}99"),
  #   cex = 1.5
  # )
  # text(x = rep(x[1]+.3,4), y = y, labels = drGroup$group, adj = c(0,.5))
  #
  # # Invertebrates
  # grSp <- spGroup |>
  #   dplyr::select(gr1, gr2, cols) |>
  #   distinct()
  # inv <- dplyr::filter(
  #   grSp,
  #   gr1 == "Invertebrates" &
  #   gr2 != "Others"
  # )
  # points(
  #   x = rep(x[2],4)+.1,
  #   y = y,
  #   pch = 21,
  #   col = inv$cols,
  #   bg = glue::glue("{inv$cols}99"),
  #   cex = 1.5
  # )
  # text(x = rep(x[2]+.3,4), y = y, labels = inv$gr2, adj = c(0,.5))
  #
  # # Vertebrates
  # ver <- dplyr::filter(
  #   grSp,
  #   gr1 == "Vertebrates" &
  #   gr2 != "Others2"
  # )
  # points(
  #   x = rep(x[3],2)+.1,
  #   y = y[1:2],
  #   pch = 21,
  #   col = ver$cols,
  #   bg = glue::glue("{ver$cols}99"),
  #   cex = 1.5
  # )
  # text(x = rep(x[3]+.3,4), y = y[1:2], labels = ver$gr2, adj = c(0,.5))
  dev.off()

  # Crop with image magick
  path <- here::here(out, "contribution.png")
  magick::image_read(path) |>
    magick::image_crop("+100-1350") |>
    magick::image_write(path = path, format = "png")
}
