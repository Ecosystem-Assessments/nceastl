#' Figure of globe centered on St. Lawrence System
#'
#' @export

fig_globe <- function() {
  library(globe)
  library(dplyr)
  library(sf)

  globeearth2 <- function(gdata, runlen,
                          eye, top,
                          ..., do.plot = TRUE) {
    if (missing(gdata)) {
      # gdata <- get("earth")$coords
      gdata <- globe::earth$coords
    }
    if (missing(runlen)) {
      # runlen <- get("earth")$runlen
      runlen <- globe::earth$runlen
    }


    spos <- spatialpos(gdata[, 1], gdata[, 2])
    mpos <- orthogproj(eye = eye, top = top, spos)

    if (do.plot) {
      par(mar = c(0, 0, 0, 0))
    }
    plot(c(-1, 1), c(-1, 1), type = "n", asp = 1, axes = FALSE, xlab = "", ylab = "")

    x <- mpos[, 1]
    y <- mpos[, 2]
    ok <- (mpos[, 3] < 0)

    # remove initial 0
    runlen <- runlen[runlen != 0]

    breaks <- cumsum(runlen)
    ok[breaks] <- FALSE

    s <- seq(x)[ok]

    if (do.plot) {
      segments(x[s], y[s], x[s + 1], y[s + 1], ...)
      ## draw globe
      a <- seq(0, 2 * pi, length = 360)
      lines(cos(a), sin(a), lty = 1, lwd = 7, col = "#7E8581")
    }
    result <- cbind(x[s], y[s], x[s + 1], y[s + 1])
    attr(result, "piece") <- as.integer(factor(cumsum(!ok)[ok]))
    return(invisible(result))
  }

  # london_lonlat = st_point(c(-0.1, 51.5)) %>%
  #   st_sfc() %>%
  #   st_sf(crs = 4326, geometry = .)
  # london_osgb = st_transform(london_lonlat, 27700)
  # origin_osgb = st_point(c(0, 0)) %>%
  #   st_sfc() %>%
  #   st_sf(crs = 27700, geometry = .)
  # london_orign = rbind(london_osgb, origin_osgb)

  out <- here::here("figures")
  rcea::chk_create(out)

  png(here::here(out, "globe.png"))
  globeearth2(eye = c(-80, 20), col = "#7E8581", lwd = 5)
  globe::globepoints(loc = c(-63, 48.5), pch = 21, cex = 8, lwd = 6, col = "#ff6767", bg = "#ff676766")
  dev.off()
}
