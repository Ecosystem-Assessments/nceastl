#' Composite figure of main ncea results
#'
#' @export

fig_ncea_full <- function() {
  out <- here::here("figures")
  rcea::chk_create(out)

  # Map and metanetwork
  png(here::here(out, "cumul_metanetwork.png"), res = 300, width = 600, height = 300, units = "mm")
  mat <- matrix(c(1, 2), ncol = 2)
  par(mfrow = c(1, 2))
  fig_ncea()
  fig_metanetwork()
  dev.off()

  # Add globe
  fig_globe()
  i1 <- magick::image_read(here::here(out, "cumul_metanetwork.png"))
  i2 <- magick::image_read(here::here(out, "globe.png"))

  c1 <- magick::image_composite(
    i1, magick::image_scale(i2, "600"),
    offset = "+180+700"
    # i1, i2, offset = "+180+700"
  )

  magick::image_write(
    c1,
    path = here::here(out, "ncea.png"),
    format = "png"
  )

  file.remove(here::here(out, "globe.png"))
  file.remove(here::here(out, "cumul_metanetwork.png"))
}
