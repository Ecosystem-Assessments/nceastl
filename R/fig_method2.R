#' Conceptual figure for methodology
#'
#' @describeIn fig_method metanetwork figure
#' @export

fig_method2 <- function() {
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Libraries and parameters 
  library(stars) |> suppressPackageStartupMessages()
  library(colorspace) |> suppressPackageStartupMessages()
  # library(scales) |> suppressPackageStartupMessages()
  fH <- 200
  bg <- "#ffffff00"
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors 
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    col <- hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n] |> 
           lapply(graphicsutils::darken, percentage = 30) |>
           unlist() 
  }
  colDr <- gg_color_hue(4)
  pal <- function(col) colorRampPalette(c(graphicsutils::lighten(col, percentage = 80), col))
  climate <- pal(colDr[1])
  coastal <- pal(colDr[2])
  fisheries <- pal(colDr[3])
  shipping <- pal(colDr[4])
  # colDr <- rep("#000000BB", 4)

  # Colors for species 
  colInv <- "#226c61"
  colInv2 <- graphicsutils::lighten(colInv, percentage = 30)
  colVer <- "#6a5024"
  colVer2 <- graphicsutils::lighten(colVer, percentage = 30)
  palInv <- pal(colInv)
  palVer <- pal(colVer)
  palVer <- palInv 
  colVer <- colInv  
  colVer2 <- colInv2
  colInt <- "#32506A"
  colVer <- colInv <- "#000000BB"
  colVer2 <- colInv2 <- graphicsutils::lighten(colInv, percentage = 30)
  
  # Palette for vulnerability
  vuln <- pal(colDr[4])
  vuln <- pal(colInt)
  
  # Palette for effects
  colsPalette <- c("#c7cbce", "#96a3a3", "#687677", "#222d3d", "#25364a", "#c77f20","#e69831", "#e3af16", "#e4be29", "#f2ea8b")
  palImpact <- colorRampPalette(colsPalette)
  colBox <- colsPalette[8]
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Species

  # Humpback whale - Megaptera novaeangliae
  # by Chris huh
  # http://phylopic.org/image/ce70490a-79a5-47fc-afb9-834e45803ab4/
  # License https://creativecommons.org/licenses/by-sa/3.0/
  hump <- png::readPNG('./img/PhyloPic.ce70490a.Chris-huh.Balaenoptera-novaeangliae.png', native = TRUE)

  # Beluga whale - Delphinapterus leucas
  # by Xavier Giroux-Bougard
  # http://phylopic.org/image/f1367ab1-40cf-4e9a-a84b-37508f11a7c7/
  bew <- png::readPNG('./img/PhyloPic.f1367ab1.Xavier-Giroux-Bougard.Delphinapterus_Delphinapterus-leucas_Monodontidae.png', native = TRUE)

  # Atlantic cod - Gadus morhua
  # Milton Tan
  # http://phylopic.org/image/bba1800a-dd86-451d-a79b-c5944cfe5231/
  cod <- png::readPNG('./img/PhyloPic.bba1800a.Milton-Tan.Gadariae_Gadidae_Gadiformes_Gadinae_Gadus_Gadus-morhua_Zeiogadaria.png', native = TRUE)

  # Capelin - Mallotus villosus
  # by xgirouxb
  # http://phylopic.org/image/f1f91d08-b850-4600-ad64-622ce87f0199/
  cap <- png::readPNG('./img/PhyloPic.f1f91d08.xgirouxb.Osmeridae_Osmeriformes_Osmerinae_Osmerini_Osmeroidea_Osmeroidei_Thaleichthys_Thaleichthys-pacificus.png', native = TRUE)

  # Krill - Meganyctiphanes norvegica
  # by Steven Haddock • Jellywatch.org
  # http://phylopic.org/image/44a3628d-aafd-45cc-97a6-1cb74bd43dec/
  kri <- png::readPNG('./img/PhyloPic.44a3628d.Steven-Haddock-Jellywatch-org.Copepoda-Malacostraca_Eucarida_Eumalacostraca_Euphausiacea_Euphausiidae_Malacostraca.png', native = TRUE)

  # Copepoda
  # by Joanna Wolfe
  # http://phylopic.org/image/c5dbd85a-c4be-4990-a369-c830ad23cb22/
  cop <- png::readPNG('./img/PhyloPic.c5dbd85a.Joanna-Wolfe.Calanoida_Copepoda_Epacteriscidae_Erebonectes_Gymnoplea_Neocopepoda.png', native = TRUE)
  cop[cop == 16777216] <-0

  # Pennatula
  # https://www.phylopic.org/images/825e7087-0dcb-48f6-b626-74ea1067df25/pteroeides-spinosum
  # Author: Guillaume Dera
  # License: https://creativecommons.org/publicdomain/zero/1.0/ 
  # No changes were made
  pen <- png::readPNG('img/Phylopic_pennatula.png', native = TRUE)

  # Asteroidea
  # https://www.phylopic.org/images/c3b6f519-544c-4869-afca-3c5775df73ce/asteroidea
  # Author: Fernando Carezzano
  # License: https://creativecommons.org/publicdomain/zero/1.0/ 
  # No changes were made
  ast <- png::readPNG('img/Phylopic_asteroidea.png', native = TRUE)

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Drivers
  SST <- png::readPNG('./img/SST.png', native = TRUE) # modified from https://fontawesome.com/icons/thermometer-full?style=solid
  Shipping <- png::readPNG('./img/ship-solid.png', native = TRUE) # https://fontawesome.com/icons/ship?style=solid
  DD <- png::readPNG('./img/trawl.png', native = TRUE)
  Acid <- png::readPNG('./img/Acid.png', native = TRUE)
  DNH <- png::readPNG('./img/DNH.png', native = TRUE)


  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Get data for graphs, use Edzer Pebesma code from Spatial Data Science
  # Adapted from (C) 2019, Edzer Pebesma, CC-BY-SA
  set.seed(2)
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  r <- read_stars(tif)
  pickdat <- function(nrow = 5, ncol = 8, cont = TRUE) {
    beg <- sample(1:300, 1)
    m <- r[[1]][beg:(beg+(nrow-1)),beg:(beg+(ncol-1)),1]
    if (!cont) {
      mM <- median(m)
      m[m <= mM] <- 0
      m[m > mM] <- 1      
    } else {
      m <- m / max(m)
    }
    dim(m) <- c(x = nrow, y = ncol) # named dim
    s <- st_as_stars(m)

    # Return
    s
  }

  # Drivers 
  nDr <- 5
  drivers <- purrr::map(1:nDr, \(i) pickdat()) |>
             setNames(glue::glue("dr{1:nDr}"))
  drivers <- do.call("c", drivers) 
  vals <- c(0.9,0,1,0,0.8)
  for(i in c(1,3,5)) drivers[[i]][3,7] <- max(drivers[[i]])
  for(i in c(2,4)) drivers[[i]][3,7] <- 0#min(drivers[[i]])
  # for(i in c(1:5)) drivers[[i]][3,7] <- vals[i]
  
  # for plots
  dr <- drivers
  attr(dr, "dimensions")[[1]]$delta = 3
  attr(dr, "dimensions")[[2]]$delta = -.25
  attr(attr(dr, "dimensions"), "raster")$affine = c(-1.2, 0.0)    
  
  # Species 
  nSp <- 8
  species <- purrr::map(1:nSp, \(i) pickdat(cont = FALSE)) |>
             setNames(glue::glue("sp{1:nSp}"))
  species <- do.call("c", species) 
  for(i in c(3,4,6,7,8)) species[[i]][3,7] <- 1
  for(i in c(1,2,5)) species[[i]][3,7] <- 0
  
  # for plots
  sp <- species
  attr(sp, "dimensions")[[1]]$delta = 3
  attr(sp, "dimensions")[[2]]$delta = -.25
  attr(attr(sp, "dimensions"), "raster")$affine = c(-1.2, 0.0)    

  # Create data for species specific sensitivities & metaweb
  spnm <- c(
    "Delphinapterus leucas","Megaptera novaeangliae",
    "Gadus morhua","Mallotus villosus",
    "Pennatula aculeata","Ctenodiscus crispatus"
  )
  stnm <- c("PositiveSST","Acidification","Shipping","FisheriesDNH","FisheriesDD")
  load("Data/Vulnerability/Vulnerability.RData")
  vulnerability <- vulnerability[spnm, stnm]
  Krill <- c(0,1,0,0,0)
  Copepod <- c(1,0.6,0,0,0)
  vulnerability["Megaptera novaeangliae","Shipping"] <- 0.8
  vulnerability["Gadus morhua","Shipping"] <- 0
  vulnerability["Pennatula aculeata","FisheriesDD"] <- 0.7
  vulnerability["Ctenodiscus crispatus","FisheriesDD"] <- 0.5
  vulnerability["Pennatula aculeata","FisheriesDNH"] <- 0.6
  vulnerability["Ctenodiscus crispatus","FisheriesDNH"] <- 0.4
  vulnerability <- rbind(vulnerability, Krill, Copepod)
  
  # Change order 
  spnm <- c(
    "Ctenodiscus crispatus", "Pennatula aculeata", "Copepod",
    "Krill", "Megaptera novaeangliae", "Delphinapterus leucas",
    "Gadus morhua","Mallotus villosus"
  )
  vulnerability <- vulnerability[spnm, ]

  # Create raster
  v <- vulnerability
  colnames(v) <- rownames(v) <- NULL
  dim(v) <- c(x = 8, y = 5) # named dim
  v <- st_as_stars(t(v))
  attr(v, "dimensions")[[1]]$delta = 4
  attr(v, "dimensions")[[2]]$delta = 1
  # attr(attr(s, "dimensions"), "raster")$affine = c(-1.2, 0.0)    

  # Metaweb
  meta <- matrix(data = 0, nrow = 8, ncol = 8, dimnames = list(spnm, spnm))
  meta["Copepod",] <- c(0,0,0,0,0,0,0,1)
  meta["Krill", ] <- c(0,0,0,0,0,0,0,1)
  meta["Mallotus villosus", ] <- c(0,0,0,0,0,1,1,0)
  meta["Gadus morhua", ] <- c(0,0,0,0,0,1,0,0)
  meta["Ctenodiscus crispatus", ] <- c(0,0,0,0,1,0,0,0)
  meta["Pennatula aculeata", ] <- c(0,0,0,0,1,0,0,0)
  
  # Cumulative effects
  colnames(vulnerability) <- glue::glue("dr{1:nDr}")
  rownames(vulnerability) <- glue::glue("sp{1:nSp}")
  rownames(meta) <- colnames(meta) <- glue::glue("sp{1:nSp}")
  trophic_sensitivity <- rcea::trophic_sensitivity
  ncea <- rcea::ncea(drivers, species, vulnerability, meta, trophic_sensitivity)
  nceasp <- rcea::cea_extract(ncea$net, cumul_fun = "vc") 
  nceafull <- rcea::cea_extract(ncea$net, cumul_fun = "full") 
  # plot(merge(nceasp), breaks = "equal", col = palImpact)
  
  # flip em...
  nceasp <- nceasp |>
    as.data.frame() |>
    dplyr::mutate(x = rev(x), y = rev(y)) |>
    stars::st_as_stars()
  nceafull <- nceafull |>
    as.data.frame() |>
    dplyr::mutate(x = rev(x), y = rev(y)) |>
    stars::st_as_stars()
  attr(nceafull, "dimensions")[[1]]$delta = 3.5
  attr(nceafull, "dimensions")[[2]]$delta = .85

  # Raster for plots 
  nceasp_ras <- nceasp
  attr(nceasp_ras, "dimensions")[[1]]$delta = 3
  attr(nceasp_ras, "dimensions")[[2]]$delta = -.25
  attr(attr(nceasp_ras, "dimensions"), "raster")$affine = c(-1.2, 0.0)    

  # Extract values from cell of interest
  nceasp_val <- list()
  for(i in 1:8) nceasp_val[[i]] <- nceasp[[i]][33]
  nceasp_val <- nceasp_val[c(3,4,8,7,6)]
  nceasp_val <- unlist(nceasp_val)  
  nceasp_col <- palImpact(101)[(nceasp_val / max(nceasp_val))*100]
  
  # Cumulative effects motif summary
  motifs <- rcea::triads(meta, trophic_sensitivity) # 3-species motifs for full metaweb
  direct_effect <- rcea::cea(drivers, species, vulnerability) |> rcea::make_array() # Direct effects
  direct_pathways <- rcea::cea_pathways(direct_effect, species) # Pathways of direct effect  
  indirect_pathways <- rcea::ncea_pathways_(direct_pathways, motifs) # Pathways of indirect effect
  motif_summary <- rcea::ncea_motifs(direct_effect, indirect_pathways) # Motif summary
  motif_effects <- rcea::ncea_effects(motif_summary, w_d=.5, w_i=.25) # Effects on each motif

  # Extract only cell of interest 
  # 1: Ctenodiscus crispatus
  # 2: Pennatula aculeata
  # 3: Copepod
  # 4: Krill
  # 5: Megaptera novaeangliae
  # 6: Delphinapterus leucas
  # 7: Gadus morhua
  # 8: Mallotus villosus
  cell <- dplyr::filter(motif_effects, id_cell == 33 & vc_id %in% c(3,4,6,7,8)) |>
    dplyr::mutate(ncea = dr1+dr2+dr3+dr4+dr5) |>
    dplyr::mutate(ncea = (ncea/max(ncea))*.75) |>
    dplyr::select(vc_id, interaction, direct, ncea) |>
    dplyr::mutate(id = rep(seq(1,dplyr::n()/3), each = 3))
  
  # Create function to generate bars for graphs
  bars <- function(focus, inter, x, y, yG, xG = .05) {
    dat <- dplyr::group_by(cell, id) |>
      dplyr::filter(
        vc_id == focus & 
        all(interaction %in% inter) 
      ) |>
      dplyr::group_by(direct) |>
      dplyr::summarise(ncea = sum(ncea)) |>
      dplyr::ungroup()
    d <- dat$ncea[dat$direct]
    i <- dat$ncea[!dat$direct]
    
    # Bars
    rect(x-xG, y+1-yG, x+xG, y+1-yG+d, col = "#00000088", border = "#000000BB")
    # lines(x = c(x,x), y = c(y+1-yG,y+1-yG+d), lwd = 4, col = colDr[1])
    rect(x-xG, y-1+yG, x+xG, y-1+yG-i, col = "#00000088", border = "#000000BB")
    # lines(x = c(x,x), y = c(y-1+yG,y-1+yG-i), lwd = 4, col = colDr[1])    
  }

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Plotting functions
  # Adapted from (C) 2019, Edzer Pebesma, CC-BY-SA
  plt <- function(x, yoffset = 0, add, pal, cell = TRUE) {
      attr(x, "dimensions")[[2]]$offset = attr(x, "dimensions")[[2]]$offset + yoffset
      l <- st_as_sf(x, as_points = FALSE)
      if (! add)
          plot(l, axes = FALSE, breaks = "equal", pal = pal, reset = FALSE, border = grey(.75), key.pos = NULL, main = NULL, xlab = "time")
      else
          plot(l, axes = TRUE, breaks = "equal", pal = pal, add = TRUE, border = grey(.75))
    
      if (cell) plot(sf::st_geometry(l)[33], col = "#00000000", add = TRUE, border = "#ffffff", lwd = 1.5)  
      u <- st_union(l)
      # print(u)
      plot(st_geometry(u), add = TRUE, col = NA, border = 'black', lwd = 2)
  }

  pldr <- function(s, x, y, nrow = 5, ncol = 8, add = TRUE, randomize = FALSE) {
    tif <- system.file("tif/L7_ETMs.tif", package = "stars")
    r <- read_stars(tif)
    attr(s, "dimensions")[[1]]$offset = x
    attr(s, "dimensions")[[2]]$offset = y
    # m <- r[[1]][y + 1:nrow,x + 1:ncol,1]
    # if (randomize)
    #   m <- m[sample(y + 1:nrow),x + 1:ncol]
    # dim(m) = c(x = nrow, y = ncol) # named dim
    # s[[1]] = m
    plt(s[1], 0, add, pal = climate)
    plt(s[2], 1, TRUE, pal = climate, cell = FALSE)
    plt(s[3], 2, TRUE, pal = shipping)
    plt(s[4], 3, TRUE, pal = fisheries, cell = FALSE)
    plt(s[5], 4, TRUE, pal = fisheries)
  }
  
  plsp <- function(s, x, y, nrow = 5, ncol = 8, add = TRUE, randomize = FALSE) {
    tif <- system.file("tif/L7_ETMs.tif", package = "stars")
    r <- read_stars(tif)
    attr(s, "dimensions")[[1]]$offset = x
    attr(s, "dimensions")[[2]]$offset = y
    # m <- r[[1]][y + 1:nrow,x + 1:ncol,1]
    # if (randomize)
    #   m <- m[sample(y + 1:nrow),x + 1:ncol]
    # dim(m) = c(x = nrow, y = ncol) # named dim
    # s[[1]] = m
    plt(s[1], 0, add, pal = palInv, cell = FALSE)
    plt(s[2], 1, TRUE, pal = palInv, cell = FALSE)
    plt(s[3], 2, TRUE, pal = palInv)
    plt(s[4], 3, TRUE, pal = palInv)
    plt(s[5], 4, TRUE, pal = palVer, cell = FALSE)
    plt(s[6], 5, TRUE, pal = palVer)
    plt(s[7], 6, TRUE, pal = palVer)
    plt(s[8], 7, TRUE, pal = palVer)
  }
  
  plnceasp <- function(s, x, y, nrow = 5, ncol = 8, add = TRUE, randomize = FALSE, pal = palImpact) {
    attr(s, "dimensions")[[1]]$offset = x
    attr(s, "dimensions")[[2]]$offset = y
    plt(s[1], 0, add, pal = pal, cell = FALSE)
    plt(s[2], 1, TRUE, pal = pal, cell = FALSE)
    plt(s[3], 2, TRUE, pal = pal)
    plt(s[4], 3, TRUE, pal = pal)
    plt(s[5], 4, TRUE, pal = pal, cell = FALSE)
    plt(s[6], 5, TRUE, pal = pal)
    plt(s[7], 6, TRUE, pal = pal)
    plt(s[8], 7, TRUE, pal = pal)
  }

  plv <- function(s, x, y, yoffset = 0, pal = vuln) {
    attr(s, "dimensions")[[1]]$offset = x
    attr(s, "dimensions")[[2]]$offset = y + yoffset
    l <- st_as_sf(s, as_points = FALSE)
    plot(l, axes = TRUE, breaks = "equal", pal = pal, add = TRUE, border = grey(.75))
    # plot(sf::st_geometry(l)[33], col = "#00000000", add = TRUE, border = "#ffffff", lwd = 1.5)  
    u <- st_union(l)
    plot(st_geometry(u), add = TRUE, col = NA, border = 'black', lwd = 2)
  }

  # Food web
  species <- function(sp = NULL, fadeSp = NULL, stress = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1,0,1)
    ysp <- .6
    ydr <- 1.4
    
    # Species
    cxy <- .6
    sh <- .75
    # graphicsutils::pchImage(x = -.51, .5, obj = hump, cex.x = 2.2*sh, cex.y = cxy+.1, col = colVer)
    graphicsutils::pchImage(x = .01, 1, obj = bew, cex.x = 2*sh, cex.y = cxy, col = colVer)
    graphicsutils::pchImage(x = .5, .66, obj = cod, cex.x = 1.55*sh, cex.y = cxy, col = colVer)
    graphicsutils::pchImage(x = 0, .04, obj = cap, cex.x = 1.25*sh, cex.y = cxy, col = colVer)
    graphicsutils::pchImage(x = -.3, -.66, obj = kri, cex.x = .85*sh, cex.y = cxy-.1, col = colInv)
    graphicsutils::pchImage(x = .3, -.66, obj = cop, cex.x = .7*sh, cex.y = cxy, col = colInv)
  }

  interactions <- function(int = NULL, fadeInt = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1,0,1)
    ysp <- .6
    ydr <- 1.4
    
    # Interactions
    lines(x = c(0, 0), y = c(.15,.85), col = colInt, lwd = 1) # bel to cap
    # lines(x = c(-.1, -.4), y = c(.1,.4), col = colInt, lwd = 1) # hump to cap
    lines(x = c(.1, .4), y = c(.1,.52), col = colInt, lwd = 1) # cod to cap
    lines(x = c(.1, .4), y = c(.9325,.7275), col = colInt, lwd = 1) # bel to cod
    lines(x = c(.045, .2375), y = c(-.1,-.52), col = colInt, lwd = 1) # cap to cop
    lines(x = c(-.045, -.2375), y = c(-.1,-.52), col = colInt, lwd = 1) # cap to kri
    # lines(x = c(-.4725, -.325), y = c(.36,-.52), col = colInt, lwd = 1) # hump to kri
  }

  drivers <- function(fadeDr = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1,0,1)
    ysp <- .6
    ydr <- 1.4
    
    # Drivers
    graphicsutils::pchImage(x = -.4, 1.45, obj = SST, cex.x = .7, cex.y = .7, col = colDr[1])
    graphicsutils::pchImage(x = 0, 1.45, obj = Shipping, cex.x = .7, cex.y = .65, col = colDr[4])
    graphicsutils::pchImage(x = .4, 1.45, obj = DD, cex.x = .7, cex.y = .65, col = colDr[3])
  }

  pathways <- function(pathRem = NULL, fadePath = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1,0,1)
    ysp <- .6
    ydr <- 1.4
    
    # Pathways of effect
    lines(x = c(-.4, .25), y = c(1.3,-.52), lty = 4, col = colDr[1], lwd = 1) # SST to cop
    lines(x = c(0, 0), y = c(1.3,1.1), lty = 4, col = colDr[4], lwd = 1) # SHP to bew
    # lines(x = c(0, -.41), y = c(1.3,.6), lty = 4, col = colDr[4], lwd = 1) # SHP to hump
    lines(x = c(.4, .485), y = c(1.3,.75), lty = 4, col = colDr[3], lwd = 1) # FISH to cod
    lines(x = c(.4, .03), y = c(1.3,.1), lty = 4, col = colDr[3], lwd = 1) # FISH to cap
  }

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Figure 1: drivers stack
  # Output
  out <- here::here("figures")
  rcea::chk_create(out)
  
  png(
    here::here(out, "raster_stack.png"), 
    res = 300, 
    width = 50, 
    height = 125,
    units = "mm"
  )
  
  par(mar = c(rep(.5,4)), family = 'serif', bg = bg)
  graphicsutils::plot0(x = c(-15,15), y = c(-5,20))
  
  # Rasters 
  pldr(dr, 0, 16)
  plsp(sp, 0, 6)
  plv(v, -8, -6)
  
  # Lines + text 
  y <- c(13.5,20)
  xL <- -13.9
  lines(x = c(xL,xL), y = y, lwd = 1.5, col = "#000000")  
  text(x = xL-.75, y = mean(y), adj = c(.5,0), srt = 90, labels = "Stressors", cex = .7, font = 2)
  y <- c(3.5,13)
  lines(x = c(xL,xL), y = y, lwd = 1.5, col = "#000000")
  text(x = xL-.75, y = mean(y), adj = c(.5,0), srt = 90, labels = "Species", cex = .7, font = 2)
  y <- c(-6,3)
  lines(x = c(xL,xL), y = y, lwd = 1.5, col = "#000000")
  text(x = xL-.75, y = mean(y), adj = c(.5,0), srt = 90, labels = "Species sensitivity", cex = .7, font = 2)
  
  lines(x = rep(-.6,2), y = c(21,3.65), lwd = 1.5, lty = 2)
  lines(x = c(-.6,15.6), y = c(3.65,3.65), lwd = 1.5, lty = 2)
  polygon(x = c(15,15,15.8), y = c(3.55,3.75,3.65), col = "#000000", lwd = 2)
  
  
  
  # Icons
  xI <- -11.7
  sY <- .7
  
  # Stressors
  graphicsutils::pchImage(x = xI+.3, 14, obj = SST, cex.x = .75, cex.y = .4, col = colDr[1])
  graphicsutils::pchImage(x = xI, 15, obj = Acid, cex.x = .75, cex.y = .25, col = colDr[1])
  graphicsutils::pchImage(x = xI, 16, obj = Shipping, cex.x = .75, cex.y = .3, col = colDr[4])
  graphicsutils::pchImage(x = xI, 17, obj = DNH, cex.x = .75, cex.y = .3, col = colDr[3])
  graphicsutils::pchImage(x = xI, 18, obj = DD, cex.x = .75, cex.y = .35, col = colDr[3])
  
  # Species 
  graphicsutils::pchImage(x = xI, 11, obj = cap, cex.x = .9, cex.y = .3*sY, col = colVer)
  graphicsutils::pchImage(x = xI, 10, obj = cod, cex.x = 1, cex.y = .3*sY, col = colVer)
  graphicsutils::pchImage(x = xI, 9, obj = bew, cex.x = 1.1, cex.y = .3*sY, col = colVer)
  graphicsutils::pchImage(x = xI, 8, obj = hump, cex.x = 1.1, cex.y = .3*sY, col = colVer)
  graphicsutils::pchImage(x = xI, 7, obj = kri, cex.x = .9, cex.y = .3*sY, col = colInv)
  graphicsutils::pchImage(x = xI, 6, obj = cop, cex.x = .9, cex.y = .35, col = colInv)
  graphicsutils::pchImage(x = xI, 5, obj = pen, cex.x = .2, cex.y = .38*sY, col = colInv)
  graphicsutils::pchImage(x = xI, 4, obj = ast, cex.x = .5, cex.y = .35*sY, col = colInv)
  
  # Species-specific sensitivity
  mD <- -9.5
  graphicsutils::pchImage(x = xI, 11+mD, obj = cap, cex.x = .9, cex.y = .3*sY, col = colVer)
  graphicsutils::pchImage(x = xI, 10+mD, obj = cod, cex.x = 1, cex.y = .3*sY, col = colVer)
  graphicsutils::pchImage(x = xI, 9+mD, obj = bew, cex.x = 1.1, cex.y = .3*sY, col = colVer)
  graphicsutils::pchImage(x = xI, 8+mD, obj = hump, cex.x = 1.1, cex.y = .3*sY, col = colVer)
  graphicsutils::pchImage(x = xI, 7+mD, obj = kri, cex.x = .9, cex.y = .3*sY, col = colInv)
  graphicsutils::pchImage(x = xI, 6+mD, obj = cop, cex.x = .9, cex.y = .35, col = colInv)
  graphicsutils::pchImage(x = xI, 5+mD, obj = pen, cex.x = .2, cex.y = .38*sY, col = colInv)
  graphicsutils::pchImage(x = xI, 4+mD, obj = ast, cex.x = .5, cex.y = .35*sY, col = colInv)
  
  yD <- 2.75
  xI <- seq(-6, by = 4, length.out = 5)
  graphicsutils::pchImage(x = xI[1], yD, obj = SST, cex.x = .75, cex.y = .4, col = colDr[1])
  graphicsutils::pchImage(x = xI[2], yD, obj = Acid, cex.x = .75, cex.y = .25, col = colDr[1])
  graphicsutils::pchImage(x = xI[3], yD, obj = Shipping, cex.x = .75, cex.y = .3, col = colDr[4])
  graphicsutils::pchImage(x = xI[4], yD, obj = DNH, cex.x = .75, cex.y = .3, col = colDr[3])
  graphicsutils::pchImage(x = xI[5], yD, obj = DD, cex.x = .75, cex.y = .35, col = colDr[3])
  
  dev.off()
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Figure 2: Food web
  # Output
  png(
    here::here(out, "foodweb.png"), 
    res = 300, 
    width = 75, 
    height = 75,
    units = "mm"
  )
  
  par(mar = c(0, 0, 0, 0), family = 'serif', bg = bg)
  graphicsutils::plot0(x = c(-.55, .6), y = c(-.8, 1.7))
  interactions()
  pathways()
  species(sp = c(1:4,6))
  drivers()
  
  dev.off()
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Figure 3: Effects
  # Output
  png(
    here::here(out, "effects.png"), 
    res = 300, 
    width = 150, 
    height = 200,
    units = "mm"
  )
  
  y <- seq(2, by = 3, length.out = 5)
  par(mar = c(0, 0, 0, 0), family = 'serif', bg = bg)
  graphicsutils::plot0(x = c(0, 5), y = c(0, 16))
  graphicsutils::pchImage(x = 1, y[5], obj = bew, cex.x = 1.4, cex.y = .6, col = colVer)
  graphicsutils::pchImage(x = 1, y[4], obj = cod, cex.x = 1.2, cex.y = .6, col = colVer)
  graphicsutils::pchImage(x = 1, y[3], obj = cap, cex.x = 1.1, cex.y = .6, col = colVer)
  graphicsutils::pchImage(x = 1, y[2], obj = kri, cex.x = 1.1, cex.y = .6, col = colInv)
  graphicsutils::pchImage(x = 1, y[1], obj = cop, cex.x = .8, cex.y = .65, col = colInv)
  
  
  # Bar plots & points
  x <- 1.7
  xM <- seq(x+.2, by = .3, length.out = 6)
  
  yG <- .4
  nM <- c(3,3,6,3,3)
  for(i in y) {
    lines(x = rep(x,2), y = c(i-yG,i+yG), lwd = 1.5) # Motif
    lines(x = rep(x,2), y = c(1+i-yG,1+i+yG-.15), lwd = 1.5) # Direct
    lines(x = rep(x,2), y = c(i-yG-1+.15,i+yG-1), lwd = 1.5) # Indirect
  
    # ugly
    if (i == 8) {
      lines(x = c(x+.1, xM[6]), y = rep(1+i-yG,2), lwd = 1, lty = 2)# x axis Direct
      lines(x = c(x+.1, xM[6]), y = rep(i+yG-1,2), lwd = 1, lty = 2)# x axis Indirect
    } else {
      lines(x = c(x+.1, xM[3]), y = rep(1+i-yG,2), lwd = 1, lty = 2)# x axis Direct
      lines(x = c(x+.1, xM[3]), y = rep(i+yG-1,2), lwd = 1, lty = 2)# x axis Indirect      
    }
  
    text(x = x-.1, y = i, adj = .5, labels = "M", cex = 1, font = 2)
    text(x = x-.1, y = i+1-.075, adj = .5, labels = "D", cex = 1, font = 2)
    text(x = x-.1, y = i-1+.075, adj = .5, labels = "I", cex = 1, font = 2)
  }
  
  # Full assessment 
  points(x = xM[c(3,3,6,3,3)]+.3, y = y, pch = 21, col = "#000000", bg = nceasp_col, cex = nceasp_val*.75)
  
  # Motifs & bars
  # Copepod
  ptcex <- 1.1
  ptlwd <- 2
  plotMotifs("tt","x", x = xM[1], y = y[1], scalingY = .3, cex = ptcex, posCol=colInv2, lwd = ptlwd, path = c("m_y","m_z"))  
  plotMotifs("tt","x", x = xM[2], y = y[1], scalingY = .3, cex = ptcex, posCol=colInv2, lwd = ptlwd, path = c("m_y","m_z"))
  plotMotifs("ap","x", x = xM[3], y = y[1], scalingY = .3, , scalingX = .2, cex = ptcex, posCol=colInv2, lwd = ptlwd, path = c("r_x","m_z"))
  bars(3, c(3,6,8), xM[1], y[1], yG)
  bars(3, c(3,7,8), xM[2], y[1], yG)
  bars(3, c(3,4,8), xM[3], y[1], yG)
  
  # Krill
  plotMotifs("tt","x", x = xM[1], y = y[2], scalingY = .3, cex = ptcex, posCol=colInv2, lwd = ptlwd, path = c("m_y","m_z"))
  plotMotifs("tt","x", x = xM[2], y = y[2], scalingY = .3, cex = ptcex, posCol=colInv2, lwd = ptlwd, path = c("m_y","m_z"))
  plotMotifs("ap","x", x = xM[3], y = y[2], scalingY = .3, , scalingX = .2, cex = ptcex, posCol=colInv2, lwd = ptlwd, path = c("r_y","m_z"))
  bars(4, c(4,6,8), xM[1], y[2], yG)
  bars(4, c(4,7,8), xM[2], y[2], yG)
  bars(4, c(3,4,8), xM[3], y[2], yG)
  
  # Capelin
  plotMotifs("om","x", x = xM[1], y = y[3], scalingY = .3, , scalingX = .2, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("r_x","m_y","m_z"))
  plotMotifs("ap","z", x = xM[2], y = y[3], scalingY = .3, , scalingX = .2, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("r_y","m_z"))
  plotMotifs("tt","y", x = xM[3], y = y[3], scalingY = .3, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("r_x","m_y","m_z"))
  plotMotifs("tt","y", x = xM[4], y = y[3], scalingY = .3, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("r_x","m_y","m_z"))
  plotMotifs("tt","y", x = xM[5], y = y[3], scalingY = .3, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("m_y","m_z"))
  plotMotifs("tt","y", x = xM[6], y = y[3], scalingY = .3, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("m_y","m_z"))
  bars(8, c(6,7,8), xM[1], y[3], yG)
  bars(8, c(3,4,8), xM[2], y[3], yG)
  bars(8, c(3,6,8), xM[3], y[3], yG)
  bars(8, c(3,7,8), xM[4], y[3], yG)
  bars(8, c(4,6,8), xM[5], y[3], yG)
  bars(8, c(4,7,8), xM[6], y[3], yG)
  
  # Cod 
  plotMotifs("om","y", x = xM[1], y = y[4], scalingY = .3, , scalingX = .2, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("r_x","m_y","m_z"))
  plotMotifs("tt","z", x = xM[2], y = y[4], scalingY = .3, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("r_x","m_y","m_z"))
  plotMotifs("tt","z", x = xM[3], y = y[4], scalingY = .3, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("m_y","m_z"))
  bars(7, c(6,7,8), xM[1], y[4], yG)
  bars(7, c(3,7,8), xM[2], y[4], yG)
  bars(7, c(4,7,8), xM[3], y[4], yG)
  
  # Beluga 
  plotMotifs("om","z", x = xM[1], y = y[5], scalingY = .3, , scalingX = .2, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("r_x","m_y","m_z"))
  plotMotifs("tt","z", x = xM[2], y = y[5], scalingY = .3, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("r_x","m_y","m_z"))
  plotMotifs("tt","z", x = xM[3], y = y[5], scalingY = .3, cex = ptcex, posCol=colVer2, lwd = ptlwd, path = c("m_y","m_z"))
  bars(6, c(6,7,8), xM[1], y[5], yG)
  bars(6, c(3,6,8), xM[2], y[5], yG)
  bars(6, c(4,6,8), xM[3], y[5], yG)
  
  dev.off()
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Figure 4: drivers stack
  png(
    here::here(out, "ncea_stack.png"), 
    res = 300, 
    width = 50, 
    height = 125,
    units = "mm"
  )
  
  par(mar = c(rep(.5,4)), family = 'serif', bg = bg)
  graphicsutils::plot0(x = c(-10,20), y = c(-5,20))
  
  # Rasters 
  plnceasp(nceasp_ras, -1.25, 10)
  plv(nceafull, -7.25, -2, pal = palImpact)
  
  # Graphical elements 
  # lines(x = c(-11.25,13.75), y = c(6.25,6.25), lwd = 2)
  lines(x = c(1.5,1.5), y = c(6.25+.75,6.25-.75), lwd = 2)
  points(x = 1.5, y = 5.5, pch = 25, cex = .5, bg = "#000000")
  text(x = 4, y = 6.25, labels = latex2exp::TeX("\\sum"), cex = .75)
  
  xL <- 16
  y <- c(7.5,17)
  lines(x = c(xL,xL), y = y, lwd = 1.5, col = "#000000")
  text(x = xL+.75, y = mean(y), adj = c(.5,0), srt = 270, labels = "Species\ncumulative effects", cex = .7, font = 2)
  y <- c(-2.5,5)
  lines(x = c(xL,xL), y = y, lwd = 1.5, col = "#000000")
  text(x = xL+.75, y = mean(y), adj = c(.5,0), srt = 270, labels = "Community\ncumulative effects", cex = .7, font = 2)
  dev.off()
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Figure 5: motifs
  png(
    here::here(out, "motif.png"), 
    res = 500, 
    width = 50, 
    height = 30,
    units = "mm"
  )
  
  par(mar = c(rep(0,4)), bg = bg, family = 'serif')
  graphicsutils::plot0(x = c(0,5), y = c(0,8))
  
  # Beluga 
  # graphicsutils::pchImage(x = 3, 7, obj = bew, cex.x = 1.4, cex.y = 1, col = colVer)
  graphicsutils::pchImage(x = 3.75, 6.25, obj = bew, cex.x = 1, cex.y = .8, col = colVer)
  graphicsutils::pchImage(x = 2.25+.38, 6.25, obj = cod, cex.x = 0.8, cex.y = .8, col = colVer)
  graphicsutils::pchImage(x = 2.25-.35, 6.25, obj = cap, cex.x = 0.7, cex.y = .8, col = colVer)
  graphicsutils::pchImage(x = 3.75, 7.77, obj = Shipping, cex.x = .55, cex.y = .85, col = colDr[4])
  graphicsutils::pchImage(x = 2.25, 7.77, obj = DD, cex.x = .55, cex.y = .85, col = colDr[3])
  
  # Motif
  # plotMotifs("om","z", x = 4, y = 7, scalingY = .6, , scalingX = .3, cex = 0.5, posCol=colVer2, lwd = 1, path = c("r_x","m_y","m_z"), lwd2 = 1)
  plotMotifs("om","z", x = 3, y = 7, scalingY = .45, , scalingX = .3, cex = 0.5, posCol=colVer2, lwd = 1, path = c("r_x","m_y","m_z"), lwd2 = 1)
  
  # bars(6, c(6,7,8), xM[1], y[5], yG)
  
  # Lines and boxes 
  box(lty=3, lwd = 2, col = colBox)
  lines(x = c(1.25,4.75), y = c(5,5), lwd = 1)
  lines(x = c(3,3), y = c(5.75, 1.1), lwd = .8)#, lty = 2)
  lines(x = c(3.75,3.75), y = c(7.25, 6.7), lty = 3, col = colDr[4], lwd = .8) # SHP to bew
  lines(x = c(2.25, 2.25+.38), y = c(7.25,6.7), lty = 3, col = colDr[3], lwd = .8) # FISH to cod
  lines(x = c(2.25, 2.25-.35), y = c(7.25,6.7), lty = 3, col = colDr[3], lwd = .8) # FISH to cap
  points(x = 3, y = 1, pch = 25, cex = .3, bg = "#000000")
  points(x = 3, y = 0.3, pch = 21, bg = colsPalette[10], cex = 1.5, lwd = 0.5)
  
  # Text 
  tcex <- .35
  text(x = 2.25, y = 5.5, adj = .5, labels = "Indirect", cex = 0.45, font = 2)
  text(x = 3.75, y = 5.5, adj = .5, labels = "Direct", cex = 0.45, font = 2)
  text(x = .5, y = 4.5, adj = .5, labels = "Stressor intensity", cex = tcex, font = 3)
  text(x = .5, y = 3.5, adj = .5, labels = "Species sensitivity", cex = tcex, font = 3)
  text(x = .5, y = 2.5, adj = .5, labels = "Trophic sensitivity", cex = tcex, font = 3)
  text(x = .5, y = 1.5, adj = .5, labels = "Relative importance", cex = tcex, font = 3)
  text(x = .5, y = 0.3, adj = .5, labels = "Total effect", cex = tcex, font = 3)
  text(x = .5, y = c(4,3,2), adj = .5, labels = "*", cex = tcex, font = 3)
  text(x = .5, y = .9, adj = .5, labels = "=", cex = tcex, font = 3)
  text(x = 2.25, y = 6.25, adj = .5, labels = "+", cex = .5, font = 2)
  # text(x = 3.025, y = .7, labels = latex2exp::TeX("\\{"), cex = 1.75, srt = 90)
  
  # Bars 
  xI = 3
  yG = .2
  y <- c(4.5,3.5,2.5,1.5)
  rel <- 0.25
  
  ## Direct effects 
  st <- 1
  sp <- st*1
  tr <- sp*(6.17*.5)
  w <- tr*.5 
  
  # Plot bars - beluga
  rect(xI, y[1]+yG, xI+(st*rel), y[1]-yG, col = paste0(colDr[4],"BB"))
  rect(xI, y[2]+yG, xI+(sp*rel), y[2]-yG, col = paste0(colDr[4],"BB"))
  rect(xI, y[3]+yG, xI+(tr*rel), y[3]-yG, col = paste0(colDr[4],"BB"))
  rect(xI, y[4]+yG, xI+(w*rel), y[4]-yG, col = paste0(colDr[4],"BB"))
  
  ## Indirect effects
  st <- c(0.8,0.8) 
  sp <- st * c(1,0.75)
  tr <- sp * (c(5.08,3.91)*.5)
  w <- tr * .25
  
  # Plot bars - capelin
  rect(xI, y[1]+yG, xI-(st[1]*rel), y[1]-yG, col = paste0(colDr[3],"BB"))
  rect(xI, y[2]+yG, xI-(sp[1]*rel), y[2]-yG, col = paste0(colDr[3],"BB"))
  rect(xI, y[3]+yG, xI-(tr[1]*rel), y[3]-yG, col = paste0(colDr[3],"BB"))
  rect(xI, y[4]+yG, xI-(w[1]*rel), y[4]-yG, col = paste0(colDr[3],"BB"))
  
  rect(xI-(st[1]*rel), y[1]+yG, xI-(st[1]*rel)-(sum(st)*rel), y[1]-yG, col = paste0(colDr[3],"BB"))
  rect(xI-(sp[1]*rel), y[2]+yG, xI-(sp[1]*rel)-(sum(sp)*rel), y[2]-yG, col = paste0(colDr[3],"BB"))
  rect(xI-(tr[1]*rel), y[3]+yG, xI-(tr[1]*rel)-(sum(tr)*rel), y[3]-yG, col = paste0(colDr[3],"BB"))
  rect(xI-(w[1]*rel), y[4]+yG, xI-(w[1]*rel)-(sum(w)*rel), y[4]-yG, col = paste0(colDr[3],"BB"))
  
  dev.off()
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Figure 6: blank
  png(
    here::here(out, "blank.png"), 
    res = 500, 
    width = 150, 
    height = 125,
    units = "mm"
  )
  
  par(mar = c(rep(0,4)), bg = "#ffffff", family = 'serif')
  graphicsutils::plot0()
  rect(-.48,.38,-.512,.55, lty = 3, lwd = 1, border = colBox)  
  lines(x = c(-.494, -.494), y = c(.55, .625), lwd = 1, lty = 3, col = colBox)
  
  # Motifs
  x = c(-.1, -.17)
  lines(x = x, y = c(.1, .1), lwd = .2, lty = 1)
  lines(x = c(-.1, -.08), y = c(.1, .145), lwd = .2, lty = 1)
  polygon(x = c(-.08,-.08,-.085), y = c(.145,.137,.141), col = "#000000", lwd = .25)
  lines(x = x, y = c(.25, .25), lwd = .2, lty = 1)
  lines(x = c(-.1, -.08), y = c(.25, .205), lwd = .2, lty = 1)
  polygon(x = c(-.08,-.08,-.085), y = c(.205,.213,.209), col = "#000000", lwd = .25)
  text(x = mean(x), y = .265, adj = .5, cex = .25, labels = latex2exp::TeX("\\textit{\\sum / n}"))
  text(x = mean(x), y = .085, adj = .5, cex = .25, labels = latex2exp::TeX("\\textit{\\sum / n}"))
  
  # Letters
  text(x = -1.045, y = .98, labels = "A", font = 2, cex = .45)
  text(x = -1.045, y = .66, labels = "B", font = 2, cex = .45)
  text(x = -1.045, y = .2, labels = "C", font = 2, cex = .45)
  text(x = -.625, y = .59, labels = "D", font = 2, cex = .45)
  text(x = -.55, y = .52, labels = "E", font = 2, cex = .45)
  text(x = -.625, y = .975, labels = "F", font = 2, cex = .45)
  text(x = -.22, y = .21, labels = "G", font = 2, cex = .45)
  text(x = 0.27, y = .85, labels = "H", font = 2, cex = .45)
  text(x = 0.27, y = .3, labels = "I", font = 2, cex = .45)
  
  dev.off()
  

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Figure final: build it 
  back <- magick::image_read(here::here(out, "blank.png"))
  i1 <- magick::image_read(here::here(out, "raster_stack.png"))
  i2 <- magick::image_read(here::here(out, "foodweb.png"))
  i3 <- magick::image_read(here::here(out, "motif.png"))
  i4 <- magick::image_read(here::here(out, "effects.png"))
  i5 <- magick::image_read(here::here(out, "ncea_stack.png"))
  
  # Crop effects
  # yG = 415
  bel <- magick::image_crop(i4, "500x400+550+150")
  cod <- magick::image_crop(i4, "500x400+550+565") 
  cap <- magick::image_crop(i4, "800x400+550+980")
  kri <- magick::image_crop(i4, "500x400+550+1395") 
  cop <- magick::image_crop(i4, "500x400+550+1810") 
  
  # Combine
  back <- magick::image_composite(back, i1) 
  back <- magick::image_composite(back, i2, offset = "+525+500") 
  back <- magick::image_composite(back, magick::image_scale(i3,"700"), offset = "+600+100") 
  back <- magick::image_composite(back, i5, offset = "+1350") 
  back <- magick::image_composite(back, magick::image_scale(bel,"200"), offset = "+740+620") 
  back <- magick::image_composite(back, magick::image_scale(cod,"200"), offset = "+1200+950") 
  back <- magick::image_composite(back, magick::image_scale(cap,"315"), offset = "+590+1000") 
  back <- magick::image_composite(back, magick::image_scale(kri,"200"), offset = "+775+1275") 
  back <- magick::image_composite(back, magick::image_scale(cop,"200"), offset = "+1200+1275") 
  back <- magick::image_crop(back, "-1000-950")

  magick::image_write(
    back, 
    path = here::here(out, "method2.png"),
    format = "png"
  )
  
  file.remove(here::here(out, "blank.png"))
  file.remove(here::here(out, "effects.png"))
  file.remove(here::here(out, "foodweb.png"))
  file.remove(here::here(out, "motif.png"))
  file.remove(here::here(out, "ncea_stack.png"))
  file.remove(here::here(out, "raster_stack.png"))  
}
