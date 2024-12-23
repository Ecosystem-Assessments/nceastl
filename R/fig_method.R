#' Conceptual figure for methodology
#'
#' @describeIn fig_method metanetwork figure
#' @export

fig_method <- function() {
  # source('./code/figures/f4-method.R')
  # ----------------------------------------------------------------------
  # Param setup
  # ----------------------------------------------------------------------
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Libraries & default parameters
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  library(latex2exp)
  library(tidyverse)
  # library(deSolve)
  figwd <- 112
  figwd2 <- figwd / .6
  fight <- 100
  figres <- 250
  xT <- -1.3
  yT <- 1.5
  yG <- .14
  xG <- .225
  cT <- .9
  cxy1 <- .9
  lwdD <- 2
  Ts <- c(.35, .61, .98, .82)
  cexMath <- 1.5
  bg <- "#00000000"
  bg <- "#ffffff"
  col1 <- "#32506A"
  col1_1 <- lighten(col1)
  colDr <- "#000000"
  col3 <- colB <- colM <- colP <- "#db9318"
  col2 <- "#986983"
  col2_2 <- lighten(col2)

  dat <- data.frame(id = c(5, 4, 3, 2, .75), sensitivity = c(.3, .75, .48, .9, .61))

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Species
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Humpback whale - Megaptera novaeangliae
  # by Chris huh
  # http://phylopic.org/image/ce70490a-79a5-47fc-afb9-834e45803ab4/
  # License https://creativecommons.org/licenses/by-sa/3.0/
  hump <- png::readPNG("./img/PhyloPic.ce70490a.Chris-huh.Balaenoptera-novaeangliae.png", native = T)

  # Beluga whale - Delphinapterus leucas
  # by Xavier Giroux-Bougard
  # http://phylopic.org/image/f1367ab1-40cf-4e9a-a84b-37508f11a7c7/
  bew <- png::readPNG("./img/PhyloPic.f1367ab1.Xavier-Giroux-Bougard.Delphinapterus_Delphinapterus-leucas_Monodontidae.png", native = T)

  # Atlantic cod - Gadus morhua
  # Milton Tan
  # http://phylopic.org/image/bba1800a-dd86-451d-a79b-c5944cfe5231/
  cod <- png::readPNG("./img/PhyloPic.bba1800a.Milton-Tan.Gadariae_Gadidae_Gadiformes_Gadinae_Gadus_Gadus-morhua_Zeiogadaria.png", native = T)

  # Capelin - Mallotus villosus
  # by xgirouxb
  # http://phylopic.org/image/f1f91d08-b850-4600-ad64-622ce87f0199/
  cap <- png::readPNG("./img/PhyloPic.f1f91d08.xgirouxb.Osmeridae_Osmeriformes_Osmerinae_Osmerini_Osmeroidea_Osmeroidei_Thaleichthys_Thaleichthys-pacificus.png", native = T)

  # Krill - Meganyctiphanes norvegica
  # by Steven Haddock • Jellywatch.org
  # http://phylopic.org/image/44a3628d-aafd-45cc-97a6-1cb74bd43dec/
  kri <- png::readPNG("./img/PhyloPic.44a3628d.Steven-Haddock-Jellywatch-org.Copepoda-Malacostraca_Eucarida_Eumalacostraca_Euphausiacea_Euphausiidae_Malacostraca.png", native = T)

  # Copepoda
  # by Joanna Wolfe
  # http://phylopic.org/image/c5dbd85a-c4be-4990-a369-c830ad23cb22/
  cop <- png::readPNG("./img/PhyloPic.c5dbd85a.Joanna-Wolfe.Calanoida_Copepoda_Epacteriscidae_Erebonectes_Gymnoplea_Neocopepoda.png", native = T)
  cop[cop == 16777216] <- 0


  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Drivers
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  SST <- png::readPNG("./img/SST.png", native = T) # modified from https://fontawesome.com/icons/thermometer-full?style=solid
  Shipping <- png::readPNG("./img/ship-solid.png", native = T) # https://fontawesome.com/icons/ship?style=solid
  DD <- png::readPNG("./img/trawl.png", native = T)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Motifs functions
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

  # Motif 1
  m1 <- function(fade = FALSE) {
    if (fade) {
      lines(x = c(-.5725, -.6575), y = c(-1.6, -1.225), col = lighten(col1, 75), lwd = lwdD)
      lines(x = c(-.5275, -.4475), y = c(-1.6, -1.225), col = lighten(col1, 75), lwd = lwdD)
      pchImage(x = -.68, -1.125, obj = hump, cex.x = .8, cex.y = cxy1, col = lighten(col3, 75))
      pchImage(x = -.42, -1.1, obj = cod, cex.x = .8, cex.y = cxy1, col = lighten(col3, 75))
      pchImage(x = -.55, -1.7, obj = cap, cex.x = .6, cex.y = cxy1, col = lighten(col3, 75))
    } else {
      lines(x = c(-.5725, -.6575), y = c(-1.6, -1.225), col = col1, lwd = lwdD)
      lines(x = c(-.5275, -.4475), y = c(-1.6, -1.225), col = col1, lwd = lwdD)
      pchImage(x = -.68, -1.125, obj = hump, cex.x = .8, cex.y = cxy1, col = col3)
      pchImage(x = -.42, -1.1, obj = cod, cex.x = .8, cex.y = cxy1, col = col3)
      pchImage(x = -.55, -1.7, obj = cap, cex.x = .6, cex.y = cxy1, col = col3)
    }
  }

  # Motif 2
  m2 <- function(fade = FALSE) {
    if (fade) {
      lines(x = c(-.15, -.15), y = c(-1.2, -1.3), col = lighten(col1, 75), lwd = lwdD)
      lines(x = c(-.15, -.15), y = c(-1.5, -1.6), col = lighten(col1, 75), lwd = lwdD)
      pchImage(x = -.15, -1.1, obj = cod, cex.x = .8, cex.y = cxy1, col = lighten(col3, 75))
      pchImage(x = -.15, -1.4, obj = cap, cex.x = .8, cex.y = cxy1, col = lighten(col3, 75))
      pchImage(x = -.15, -1.7, obj = kri, cex.x = .5, cex.y = cxy1, col = lighten(col1, 75))
    } else {
      lines(x = c(-.15, -.15), y = c(-1.2, -1.3), col = col1, lwd = lwdD)
      lines(x = c(-.15, -.15), y = c(-1.5, -1.6), col = col1, lwd = lwdD)
      pchImage(x = -.15, -1.1, obj = cod, cex.x = .8, cex.y = cxy1, col = col3)
      pchImage(x = -.15, -1.4, obj = cap, cex.x = .8, cex.y = cxy1, col = col3)
      pchImage(x = -.15, -1.7, obj = kri, cex.x = .5, cex.y = cxy1, col = col1)
    }
  }

  # Motif 3
  m3 <- function(fade = FALSE) {
    if (fade) {
      lines(x = c(.15, .15), y = c(-1.2, -1.3), col = lighten(col1, 75), lwd = lwdD)
      lines(x = c(.15, .15), y = c(-1.5, -1.6), col = lighten(col1, 75), lwd = lwdD)
      pchImage(x = .15, -1.1, obj = cod, cex.x = .8, cex.y = 1, col = lighten(col3, 75))
      pchImage(x = .15, -1.4, obj = cap, cex.x = .8, cex.y = 1, col = lighten(col3, 75))
      pchImage(x = .15, -1.7, obj = cop, cex.x = .6, cex.y = 1, col = lighten(col3, 75))
    } else {
      lines(x = c(.15, .15), y = c(-1.2, -1.3), col = col1, lwd = lwdD)
      lines(x = c(.15, .15), y = c(-1.5, -1.6), col = col1, lwd = lwdD)
      pchImage(x = .15, -1.1, obj = cod, cex.x = .8, cex.y = 1, col = col3)
      pchImage(x = .15, -1.4, obj = cap, cex.x = .8, cex.y = 1, col = col3)
      pchImage(x = .15, -1.7, obj = cop, cex.x = .6, cex.y = 1, col = col3)
    }
  }

  # Motif 4
  m4 <- function(fade = FALSE) {
    if (fade) {
      lines(x = c(.45, .45), y = c(-1.2, -1.6), col = lighten(col1, 75), lwd = lwdD)
      lines(x = c(.51, .6), y = c(-1.17, -1.27), col = lighten(col1, 75), lwd = lwdD)
      lines(x = c(.6075, .4875), y = c(-1.41, -1.63), col = lighten(col1, 75), lwd = lwdD)
      pchImage(x = .45, -1.1, obj = bew, cex.x = .8, cex.y = 1, col = lighten(col3, 75))
      pchImage(x = .65, -1.33, obj = cod, cex.x = .8, cex.y = 1, col = lighten(col3, 75))
      pchImage(x = .45, -1.7, obj = cap, cex.x = .6, cex.y = 1, col = lighten(col3, 75))
    } else {
      lines(x = c(.45, .45), y = c(-1.2, -1.6), col = col1, lwd = lwdD)
      lines(x = c(.51, .6), y = c(-1.17, -1.27), col = col1, lwd = lwdD)
      lines(x = c(.6075, .4875), y = c(-1.41, -1.63), col = col1, lwd = lwdD)
      pchImage(x = .45, -1.1, obj = bew, cex.x = .8, cex.y = 1, col = col3)
      pchImage(x = .65, -1.33, obj = cod, cex.x = .8, cex.y = 1, col = col3)
      pchImage(x = .45, -1.7, obj = cap, cex.x = .6, cex.y = 1, col = col3)
    }
  }



  plotEffect <- function(fade = FALSE, full = FALSE) {
    # =-=-=-=-=-=-=-=-=-=-=-= #
    # Plot effects per motif
    # =-=-=-=-=-=-=-=-=-=-=-= #
    par(mar = c(2, .5, 4, .5), family = "serif")
    graphicsutils::plot0(x = c(-1, 1), y = c(.5, 5.5))
    # lines(x = c(-.75,7.15), y = c(1.2,1.2), lty = 2, lwd = .75)
    # abline(h = 1.2, lty = 2, lwd = .75)

    if (fade) {
      # Competitive exploitation
      lines(x = c(-.85, -.6), y = c(5.2, 4.8), col = lighten(col1, 75), lwd = lwdD)
      lines(x = c(-.15, -.4), y = c(5.2, 4.8), col = lighten(col1, 75), lwd = lwdD)
      # ellipse(-.1, 5.4, .2, mjradi = .25, mnradi = .15, lwd = lwdD)
      pchImage(x = -.4 - .5, 5.375, obj = hump, cex.x = 2, cex.y = cxy1 * .4, col = lighten(col3, 75))
      pchImage(x = .4 - .5, 5.4, obj = cod, cex.x = 2, cex.y = cxy1 * .4, col = lighten(col3, 75))
      pchImage(x = 0 - .5, 4.6, obj = cap, cex.x = 1.8, cex.y = cxy1 * .4, col = lighten(col3, 75))

      # Food chain 1
      lines(x = c(.5, .5), y = c(4.15, 4.3), lwd = lwdD, col = lighten(col1, 75))
      lines(x = c(.5, .5), y = c(3.7, 3.85), lwd = lwdD, col = lighten(col1, 75))
      # ellipse(.5, 4.45, .2, mjradi = .25, mnradi = .15, lwd = lwdD)
      pchImage(x = 0 + .5, 4.45, obj = cod, cex.x = 2, cex.y = cxy1 * .4, col = lighten(col3, 75))
      pchImage(x = 0 + .5, 4, obj = cap, cex.x = 1.8, cex.y = cxy1 * .4, col = lighten(col3, 75))
      pchImage(x = 0 + .5, 3.55, obj = kri, cex.x = 1.5, cex.y = cxy1 * .4, col = lighten(col1, 75)) # , col = colP[1])

      # Food chain 2
      lines(x = c(-.5, -.5), y = c(3.15, 3.3), col = lighten(col1, 75), lwd = lwdD)
      lines(x = c(-.5, -.5), y = c(2.7, 2.85), col = lighten(col1, 75), lwd = lwdD)
      # ellipse(-.5, 3.45, .2, mjradi = .25, mnradi = .15, lwd = lwdD)
      pchImage(x = 0 - .5, 3.45, obj = cod, cex.x = 2, cex.y = cxy1 * .4, col = lighten(col3, 75))
      pchImage(x = 0 - .5, 3, obj = cap, cex.x = 1.8, cex.y = cxy1 * .4, col = lighten(col3, 75))
      pchImage(x = 0 - .5, 2.55, obj = cop, cex.x = 1.5, cex.y = cxy1 * .4, col = lighten(col3, 75))
    } else {
      # Competitive exploitation
      lines(x = c(-.85, -.6), y = c(5.2, 4.8), col = col1, lwd = lwdD)
      lines(x = c(-.15, -.4), y = c(5.2, 4.8), col = col1, lwd = lwdD)
      # ellipse(-.1, 5.4, .2, mjradi = .25, mnradi = .15, lwd = lwdD)
      pchImage(x = -.4 - .5, 5.375, obj = hump, cex.x = 2, cex.y = cxy1 * .4, col = col3)
      pchImage(x = .4 - .5, 5.4, obj = cod, cex.x = 2, cex.y = cxy1 * .4, col = col3)
      pchImage(x = 0 - .5, 4.6, obj = cap, cex.x = 1.8, cex.y = cxy1 * .4, col = col3)

      # Food chain 1
      lines(x = c(.5, .5), y = c(4.15, 4.3), lwd = lwdD, col = col1)
      lines(x = c(.5, .5), y = c(3.7, 3.85), lwd = lwdD, col = col1)
      # ellipse(.5, 4.45, .2, mjradi = .25, mnradi = .15, lwd = lwdD)
      pchImage(x = 0 + .5, 4.45, obj = cod, cex.x = 2, cex.y = cxy1 * .4, col = col3)
      pchImage(x = 0 + .5, 4, obj = cap, cex.x = 1.8, cex.y = cxy1 * .4, col = col3)
      pchImage(x = 0 + .5, 3.55, obj = kri, cex.x = 1.5, cex.y = cxy1 * .4, col = col1) # , col = colP[1])

      # Food chain 2
      lines(x = c(-.5, -.5), y = c(3.15, 3.3), col = col1, lwd = lwdD)
      lines(x = c(-.5, -.5), y = c(2.7, 2.85), col = col1, lwd = lwdD)
      # ellipse(-.5, 3.45, .2, mjradi = .25, mnradi = .15, lwd = lwdD)
      pchImage(x = 0 - .5, 3.45, obj = cod, cex.x = 2, cex.y = cxy1 * .4, col = col3)
      pchImage(x = 0 - .5, 3, obj = cap, cex.x = 1.8, cex.y = cxy1 * .4, col = col3)
      pchImage(x = 0 - .5, 2.55, obj = cop, cex.x = 1.5, cex.y = cxy1 * .4, col = col3)
    }

    # Omnivory
    lines(x = c(.25, .25), y = c(2.2, 1.7), col = col1, lwd = lwdD)
    lines(x = c(.4, .7), y = c(2.3, 2.15), col = col1, lwd = lwdD)
    lines(x = c(.4, .7), y = c(1.65, 1.9), lwd = lwdD)
    # ellipse(.85, 2.05, .2, mjradi = .25, mnradi = .15, lwd = lwdD)
    pchImage(x = .25, 2.4, obj = bew, cex.x = 2, cex.y = cxy1 * .4, col = col3)
    pchImage(x = .85, 2.05, obj = cod, cex.x = 2, cex.y = cxy1 * .4, col = col3)
    pchImage(x = .25, 1.5, obj = cap, cex.x = 1.8, cex.y = cxy1 * .4, col = col3)

    if (full) {
      # Cod
      pchImage(x = 0, .75, obj = cod, cex.x = 4.5, cex.y = cxy1 * .9)
      abline(h = 1.2, lty = 2, lwd = .75)
    }
  }

  plotSens <- function(fade = FALSE, full = FALSE) {
    # =-=-=-=-=-=-=-=-=-=-=-= #
    # Plot sensitivity
    # =-=-=-=-=-=-=-=-=-=-=-= #
    cols <- c(rep("#444643", 4), "#4B7894")
    par(mar = c(2, .5, 4, .5), family = "serif")
    graphicsutils::plot0(x = c(0, 1), y = c(.5, 5.5))
    rect(0, dat$id[4] - 0.2, dat$sensitivity[4], dat$id[4] + .2, col = cols[4], border = NA)
    lines(x = c(0, 0), y = c(1, 5.75))
    axis(3, at = c(0, 1), tck = -.055, labels = FALSE)
    mtext(side = 3, at = c(0, 1), text = c(0, 1), cex = .4, line = .5)
    mtext(side = 3, at = .5, adj = c(.5, .5), cex = .5, line = 1.85, text = "Stressors effect")
    mtext(side = 3, at = .5, adj = c(.5, .5), cex = .5, line = 1, text = TeX("($\\textit{S_m}$)"))
    # txt <- data.frame(x = c(.005,.005,.005,-.005,.005), y = c(5:2,.75), adjX = c(0,0,0,1,0), adjY = c(.5,.5,.5,.5,.6),
    # lab = c('$S_{m,M_1}$','$S_{m,M_2}$','$S_{m,M_3}$','$S_{m,M_4}$','$\\sum_i S_{m,M_i}$'))
    # for(i in 1:(nrow(txt)-1)) text(x = txt$x[i], y = txt$y[i], labels = TeX(txt$lab[i]), cex = .7, adj = c(txt$adjX[i],txt$adjY[i]))
    if (!fade) {
      for (i in 1:3) rect(0, dat$id[i] - 0.2, dat$sensitivity[i], dat$id[i] + .2, col = cols[i], border = NA)
    }

    if (full) {
      abline(h = 1.2, lty = 2, lwd = .75)
      axis(1, at = c(0, 1), tck = -.055, labels = FALSE)
      mtext(side = 1, at = c(0, 1), text = c(0, 1), cex = .4, line = .5)
      lines(x = c(0, 0), y = c(.25, 1))
      rect(0, dat$id[5] - 0.2, dat$sensitivity[5], dat$id[5] + .2, col = cols[5], border = NA)
    }
  }


  plotMath <- function(spid = NULL, drid = NULL, pathid = NULL) {
    # Species
    xsp <- c(-.5, .01, .5, 0, -.3, .3)
    ysp <- c(.5, 1, .66, .04, -.66, -.66)
    ysp2 <- c(ysp[1] + .15, ysp[2] - .1, ysp[3] - .1, ysp[4] - .11, ysp[5] - .13, ysp[6] - .13)
    spnm <- c(
      "\\textit{$E_{bal}$}", "$\\textit{E_{bel}$}",
      "\\textit{$E_{mor}$}", "$\\textit{E_{cap}$}",
      "\\textit{$E_{kri}$}", "\\textit{$E_{cop}$}"
    )
    text(x = xsp[spid], y = ysp2[spid], labels = TeX(spnm[spid]), cex = cexMath * .9, adj = c(.5, .5), col = colText1)

    # Drivers
    drnm <- c(
      "\\textit{$D_{tem}$}", "\\textit{$D_{nav}$}",
      "\\textit{$D_{pec}$}"
    )
    xdr <- c(-.4, 0, .4)
    ydr <- 1.4
    text(x = xdr[drid], y = ydr + .25, labels = TeX(drnm[drid]), cex = cexMath * .9, adj = c(.5, .5), col = colText1)

    # Susceptibilities
    unm <- c(
      "\\textit{$\\mu_{cop,tem}$}", "\\textit{$\\mu_{bal,nav}$}",
      "\\textit{$\\mu_{bel,nav}$}", "\\textit{$\\mu_{mor,pec}$}",
      "\\textit{$\\mu_{cap,pec}$}"
    )
    xmu <- c(-.4, 0, 0, .4, .4)
    ymu <- rep(ydr - .2, 5)
    xGap <- c(-.09, -.2, .12, -.15, .14)
    yGap <- c(0, 0, -.05, .05, 0)
    text(x = xmu[pathid] + xGap[pathid], y = ymu[pathid] + yGap[pathid], labels = TeX(unm[pathid]), cex = cexMath * .8, adj = c(.5, .5), col = colText1)
  }

  speciesLine <- function(lab = "names", focus = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1, 0, 1)
    ysp <- .6
    ydr <- 1.4

    # Colors
    colsp <- rep(col1, 6)

    # Focus
    if (!is.null(focus)) colsp[-focus] <- sapply(colsp[-focus], lighten, percentage = 85, USE.NAMES = FALSE)

    # Species
    cxy <- .6
    sh <- .75
    y <- ysp
    x <- xsp
    pchImage(x = x[1], y = y - .025, obj = hump, cex.x = 2.2 * sh, cex.y = cxy + .1, col = colsp[1])
    pchImage(x = x[2], y = y, obj = bew, cex.x = 2 * sh, cex.y = cxy, col = colsp[2])
    pchImage(x = x[3], y = y, obj = cod, cex.x = 1.55 * sh, cex.y = cxy, col = colsp[3])
    pchImage(x = x[4], y = y, obj = cap, cex.x = 1.25 * sh, cex.y = cxy, col = colsp[4])
    pchImage(x = x[5], y = y, obj = kri, cex.x = .85 * sh, cex.y = cxy - .1, col = colsp[5])
    pchImage(x = x[6], y = y, obj = cop, cex.x = .7 * sh, cex.y = cxy, col = colsp[6])

    if (!is.null(focus)) {
      uid <- focus
    } else {
      uid <- 1:6
    }

    if (!is.null(lab)) {
      # Names
      if (lab == "names") {
        nm <- c("Baleine à bosse", "Béluga", "Morue", "Capelan", "Krill", "Copépodes")
        sc <- c(
          "\\textit{(Megaptera novaeangliae)}", "\\textit{(Delphinapterus leucas)}",
          "\\textit{(Gadus morhua)}", "\\textit{(Mallotus villotus)}",
          "(Euphausiacea)", "(Copepoda)"
        )
        text(x = x[uid], y = ysp - .25, labels = nm[uid], cex = .8, adj = c(.5, .5), col = colText1)
        text(x = x[uid], y = ysp - .35, labels = TeX(sc[uid]), cex = .6, adj = c(.5, .5), col = colText2)
      }

      # Math
      if (lab == "math") {
        nm <- c(
          "\\textit{$E_{bal}$}", "$\\textit{E_{bel}$}",
          "\\textit{$E_{mor}$}", "$\\textit{E_{cap}$}",
          "\\textit{$E_{kri}$}", "\\textit{$E_{cop}$}"
        )
        text(x = x[uid], y = ysp - .25, labels = TeX(nm[uid]), cex = .8, adj = c(.5, .5), col = colText1)
      }
    }
  }

  # Drivers in line
  driversLine <- function(lab = "names", focusSt = NULL, focusPt = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1, 0, 1)
    ysp <- .6
    ydr <- 1.4

    # Colors
    colst <- rep(colDr, 3)
    colpt <- rep(colDr, 5)

    # Focus
    if (!is.null(focusSt)) colst[-focusSt] <- sapply(colst[-focusSt], lighten, percentage = 85, USE.NAMES = FALSE)
    if (!is.null(focusPt)) colpt[-focusPt] <- sapply(colpt[-focusPt], lighten, percentage = 85, USE.NAMES = FALSE)


    # Drivers
    x <- xdr
    y <- ydr
    pchImage(x = x[1] + .05, y = y, obj = SST, cex.x = .55, cex.y = .7, col = colst[1])
    pchImage(x = x[2], y = y, obj = Shipping, cex.x = .55, cex.y = .65, col = colst[2])
    pchImage(x = x[3], y = y, obj = DD, cex.x = .55, cex.y = .65, col = colst[3])

    if (!is.null(focusSt)) {
      uid <- focusSt
    } else {
      uid <- 1:3
    }

    if (!is.null(lab)) {
      if (lab == "names") {
        # Names
        nm <- c("Hausse de température", "Navigation", "Pêcheries")
        text(x = x[uid], y = 1.4 + .25, labels = nm[uid], cex = .8, adj = c(.5, .5), col = colText1)
      }
      if (lab == "math") {
        nm <- c(
          "\\textit{$D_{tem}$}", "\\textit{$D_{nav}$}",
          "\\textit{$D_{pec}$}"
        )
        text(x = x[uid], y = 1.4 + .25, labels = TeX(nm[uid]), cex = .8, adj = c(.5, .5), col = colText1)
      }
    }

    # Effects
    lines(x = c(x[1], xsp[6]), y = c(y - .15, ysp + .15), col = colpt[1], lwd = 1, lty = 4)
    lines(x = c(x[2], xsp[1]), y = c(y - .15, ysp + .15), col = colpt[2], lwd = 1, lty = 4)
    lines(x = c(x[2], xsp[2]), y = c(y - .15, ysp + .15), col = colpt[3], lwd = 1, lty = 4)
    lines(x = c(x[3], xsp[3]), y = c(y - .15, ysp + .15), col = colpt[4], lwd = 1, lty = 4)
    lines(x = c(x[3], xsp[4]), y = c(y - .15, ysp + .15), col = colpt[5], lwd = 1, lty = 4)

    if (!is.null(focusPt)) {
      uid <- focusPt
    } else {
      uid <- 1:5
    }

    if (!is.null(lab)) {
      if (lab == "math") {
        nm <- c(
          "\\textit{$\\mu_{cop,tem}$}", "\\textit{$\\mu_{bal,nav}$}",
          "\\textit{$\\mu_{bel,nav}$}", "\\textit{$\\mu_{mor,pec}$}",
          "\\textit{$\\mu_{cap,pec}$}"
        )
        xmu <- c(
          mean(c(xdr[1], xsp[6])),
          mean(c(xdr[2], xsp[1])),
          mean(c(xdr[2], xsp[2])),
          mean(c(xdr[3], xsp[3])),
          mean(c(xdr[3], xsp[4]))
        )
        ymu <- mean(c(ydr, ysp))
        xGap <- c(-.1, -.3, .15, -.1, .2)
        yGap <- c(.1, 0, -.05, .1, 0)
        text(x = xmu[uid] + xGap[uid], y = ymu + yGap[uid], labels = TeX(nm[uid]), cex = .8, adj = c(.5, .5), col = colText1)
      }
    }
  }

  # Food web
  species <- function(sp = NULL, fadeSp = NULL, stress = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1, 0, 1)
    ysp <- .6
    ydr <- 1.4

    # Colors
    sts <- c(colB, colB, colM, colM, col1, colP)
    colsSp <- rep(col1, 6)
    if (!is.null(sp)) colsSp[sp] <- col3
    if (!is.null(stress)) colsSp[stress] <- sts[stress]
    if (!is.null(fadeSp)) colsSp[fadeSp] <- sapply(colsSp[fadeSp], lighten, percentage = 75, USE.NAMES = FALSE)

    # Species
    cxy <- .6
    sh <- .75
    pchImage(x = -.51, .5, obj = hump, cex.x = 2.2 * sh, cex.y = cxy + .1, col = colsSp[1])
    pchImage(x = .01, 1, obj = bew, cex.x = 2 * sh, cex.y = cxy, col = colsSp[2])
    pchImage(x = .5, .66, obj = cod, cex.x = 1.55 * sh, cex.y = cxy, col = colsSp[3])
    pchImage(x = 0, .04, obj = cap, cex.x = 1.25 * sh, cex.y = cxy, col = colsSp[4])
    pchImage(x = -.3, -.66, obj = kri, cex.x = .85 * sh, cex.y = cxy - .1, col = colsSp[5])
    pchImage(x = .3, -.66, obj = cop, cex.x = .7 * sh, cex.y = cxy, col = colsSp[6])
  }

  interactions <- function(int = NULL, fadeInt = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1, 0, 1)
    ysp <- .6
    ydr <- 1.4

    # Colors
    colsInt <- rep(col1, 7)
    if (!is.null(int)) colsInt[int] <- col3
    if (!is.null(fadeInt)) colsInt[fadeInt] <- sapply(colsInt[fadeInt], lighten, percentage = 75, USE.NAMES = FALSE)

    # Interactions
    lines(x = c(0, 0), y = c(.15, .85), col = colsInt[1]) # bel to cap
    lines(x = c(-.1, -.4), y = c(.1, .4), col = colsInt[2]) # hump to cap
    lines(x = c(.1, .4), y = c(.1, .52), col = colsInt[3]) # cod to cap
    lines(x = c(.1, .4), y = c(.9325, .7275), col = colsInt[4]) # bel to cod
    lines(x = c(.045, .2375), y = c(-.1, -.52), col = colsInt[5]) # cap to cop
    lines(x = c(-.045, -.2375), y = c(-.1, -.52), col = colsInt[6]) # cap to kri
    lines(x = c(-.4725, -.325), y = c(.36, -.52), col = colsInt[7]) # hump to kri
  }

  drivers <- function(fadeDr = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1, 0, 1)
    ysp <- .6
    ydr <- 1.4

    # Colors
    colsDr <- rep(colDr, 3)
    if (!is.null(fadeDr)) colsDr[fadeDr] <- sapply(colsDr[fadeDr], lighten, percentage = 75, USE.NAMES = FALSE)

    # Drivers
    pchImage(x = -.4, 1.45, obj = SST, cex.x = .55, cex.y = .7, col = colsDr[1])
    pchImage(x = 0, 1.45, obj = Shipping, cex.x = .55, cex.y = .65, col = colsDr[2])
    pchImage(x = .4, 1.45, obj = DD, cex.x = .55, cex.y = .65, col = colsDr[3])
  }

  pathways <- function(pathRem = NULL, fadePath = NULL) {
    # Species in line
    xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
    xdr <- c(-1, 0, 1)
    ysp <- .6
    ydr <- 1.4

    # Colors
    colsPt <- rep(colDr, 5)
    if (!is.null(pathRem)) colsPt[pathRem] <- "#00000000"
    if (!is.null(fadePath)) colsPt[fadePath] <- sapply(colsPt[fadePath], lighten, percentage = 75, USE.NAMES = FALSE)

    # Pathways of effect
    lines(x = c(-.4, .25), y = c(1.3, -.52), lty = 4, col = colsPt[1]) # SST to cop
    lines(x = c(0, 0), y = c(1.3, 1.1), lty = 4, col = colsPt[2]) # SHP to bew
    lines(x = c(0, -.41), y = c(1.3, .6), lty = 4, col = colsPt[3]) # SHP to hump
    lines(x = c(.4, .485), y = c(1.3, .75), lty = 4, col = colsPt[4]) # FISH to cod
    lines(x = c(.4, .03), y = c(1.3, .1), lty = 4, col = colsPt[5]) # FISH to cap
  }


  # # Motif census
  # circles(.5, .67, .17, lwd = 2, lty = 2, border = col2)
  # drivers()

  # Math
  math0 <- "\\textit{$\\textbf{S_{mor}} =$}"
  math1 <- "\\textit{$+$}"
  math2 <- "\\textit{$D_{pec} \\;*\\; \\mu_{mor,pec}$}"
  math3 <- "\\textit{$D_{nav} \\;*\\; \\mu_{bel,nav}$}"
  math4 <- "\\textit{$D_{pec} \\;*\\; \\mu_{cap,pec}$}"
  math5 <- "\\textit{$\\textbf{C_{mor}} = S_{mor} \\;*\\; T_{mor_{m_4}}$}"

  # layout
  mat <- matrix(c(1, 1, 2, 3, 2, 4, 2, 5), nrow = 2, ncol = 4)

  # ----------------------------------------------------------------------
  # Figure: food web + focus morue + omnivorie + beauchesne + trophic sens
  # ----------------------------------------------------------------------
  # Output
  out <- here::here("figures")
  rcea::chk_create(out)

  png(
    here::here(out, "method.png"),
    res = figres,
    width = figwd2,
    height = fight,
    units = "mm"
  )

  layout(mat, widths = c(.61, .13, .13, .13), heights = c(.25, .75))
  par(mar = c(0, 0, 0, 0), family = "serif", bg = bg)
  graphicsutils::plot0(x = c(-1.25, .75), y = c(-.8, 1.75))
  interactions()
  pathways()
  species(sp = c(1:4, 6))
  drivers()
  circles(.5, .67, .17, lwd = 2, lty = 2, border = col2)
  text(x = -.65, 1.71, labels = substitute(paste(bold("a"))), adj = c(0, .5), cex = 1)


  # Motifs
  par(mar = c(0, 0, 0, 0))
  graphicsutils::plot0(x = c(-.85, .85), y = c(-1.9, -.8))
  m1(FALSE)
  m2(FALSE)
  m3(FALSE)
  m4(FALSE)
  # Text
  text(x = c(-.55, -.15, .15, .55), y = rep(-1.9, 4), labels = TeX(c("$M_1$", "$M_2$", "$M_3$", "$M_4$")), cex = cexMath * .55)
  text(x = -.88, -1, labels = substitute(paste(bold("b"))), adj = c(0, .5), cex = 1)

  # Effects
  plotEffect(FALSE, TRUE)
  # text(x = -.75, 5.75, labels = substitute(paste(bold('(c)'))), adj = c(0,.5), cex = 1)
  mtext("c", side = 3, line = 0, adj = 0, cex = .7, font = 2)
  plotSens(FALSE, TRUE)

  dev.off()

  # Crop with image magick
  library(magick)
  path <- here::here(out, "method.png")
  magick::image_read(path) |>
    magick::image_crop("+300") |>
    magick::image_write(path = path, format = "png")
}
