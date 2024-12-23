#' Script to extract cumulative effects scores / km2 for each species from the species and network-scale assessments
#'
#' @export

out_cea_km2 <- function() {
  # Output
  out <- here::here("output", "cea_km2")
  rcea::chk_create(out)

  # Species distributions
  load(here::here("data", "FormatData", "bt.RData"))
  hr <- data.frame(
    species = colnames(bt),
    area = colSums(bt, na.rm = TRUE)
  )
  rownames(hr) <- NULL

  # Species-scale cumulative effects assessment
  folder <- here::here("output", "cea")
  files <- dir(folder, full.names = TRUE)
  nm <- basename(files) |>
    stringr::str_replace(".tif", "")

  dat <- lapply(files, function(x) {
    stars::read_stars(x) |>
      split() |>
      as.data.frame() |>
      dplyr::select(-x, -y) |>
      # colSums(na.rm = TRUE)
      colSums(na.rm = TRUE)
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(species = nm)

  # Join together and divide by area
  cekm <- dplyr::left_join(hr, dat, by = "species") |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ .x / area)) |>
    dplyr::select(-area)

  # Export
  write.csv(cekm, file = here::here(out, "cea_km2.csv"), row.names = FALSE)


  # NOTE: The network-scale assessment performed with `rcea::ncea` already provides these results.
  #       We only need to reformat and export them
  folder <- here::here("output", "ncea", "cekm") # WARNING: Rename once folder name is ok
  files <- dir(folder, full.names = TRUE)
  net <- stringr::str_detect(files, "_net.csv")
  direct <- stringr::str_detect(files, "_direct.csv")
  indirect <- stringr::str_detect(files, "_indirect.csv")

  # Load & retransform in cea/km2, as it is currently in cea/m2
  loaddat <- function(uid) {
    lapply(files[uid], read.csv) |>
      data.table::rbindlist() |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.numeric), ~ .x * 1e6)
      ) |>
      dplyr::rename(species = vc)
  }
  net <- loaddat(net)
  direct <- loaddat(direct)
  indirect <- loaddat(indirect)

  # Export
  write.csv(net, file = here::here(out, "ncea_km2.csv"), row.names = FALSE)
  write.csv(direct, file = here::here(out, "ncea_direct_km2.csv"), row.names = FALSE)
  write.csv(indirect, file = here::here(out, "ncea_indirect_km2.csv"), row.names = FALSE)
}
