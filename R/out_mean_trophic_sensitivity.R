#' Script to extract the mean trophic sensitivity per km2 for all species
#'
#' @export

out_cea_km2 <- function() {
  # Output
  out <- here::here("output", "mean_trophic_sensitivity")
  rcea::chk_create(out)

  # Species distributions
  load(here::here("data", "FormatData", "bt.RData"))
  hr <- data.frame(
    species = colnames(bt),
    area = colSums(bt, na.rm = TRUE)
  )
  rownames(hr) <- NULL

  # ------------------------------------------------------------------
  # Identify and select cells for simulations
  net <- stars::read_stars("output/cea_full/ncea.tif")
  direct <- stars::read_stars("output/cea_full/ncea_direct.tif")
  indirect <- stars::read_stars("output/cea_full/ncea_indirect.tif")
  species <- stars::read_stars("output/footprint/species_richness.tif")
  diff <- direct - indirect

  # Divide data to select 4 points
  xy_id <- dim(net)
  xy_id <- expand.grid(1:xy_id["x"], 1:xy_id["y"]) |>
    dplyr::rename(x_id = Var1, y_id = Var2)
  df <- cbind(
    xy_id,
    data.frame(net),
    data.frame(diff) |> dplyr::select(-x, -y),
    data.frame(species) |> dplyr::select(-x, -y)
  ) |>
    dplyr::rename(net = `ncea.tif`, diff = `ncea_direct.tif`, species = `species_richness.tif`) |>
    na.omit() |>
    dplyr::mutate(
      quantile = cut(net, breaks = quantile(net, c(.6, .9, .95))),
      quantile = as.numeric(quantile),
      direct = diff > 0
    ) |>
    na.omit() |>
    dplyr::filter(abs(diff) > 10 & species > 75) |>
    dplyr::group_by(quantile, direct) |>
    dplyr::group_split()

  # Select random points
  set.seed(180)
  pt_sample <- lapply(df, function(x) {
    x[sample(seq_len(nrow(x)), round(nrow(x) * 0.1), 0), ]
    # x[sample(seq_len(nrow(x)), 5), ]
  }) |>
    dplyr::bind_rows() |>
    sf::st_as_sf(coords = c("x", "y"), crs = 32198, remove = FALSE) |>
    sf::st_transform(sf::st_crs(net))
  # image(net, col = viridis::magma(100))
  # plot(sf::st_geometry(pt_sample), add = TRUE, col = "#1de231", pch = 16, cex = 2)

  # Get number of interactions and connectance
  load("data/FormatData/driversRaster.RData")
  load("data/FormatData/biotic.RData")
  load("data/FormatData/species_sensitivity.RData")
  load("data/FormatData/metaweb.RData")
  load("data/FormatData/TrophicSensitivity.RData")

  # Subset drivers and biotic
  pt_sample$n_links <- 0
  for (i in seq_len(nrow(pt_sample))) {
    x_id <- pt_sample$x_id[i]
    y_id <- pt_sample$y_id[i]
    b <- biotic[, x_id, y_id]

    # Subset metaweb
    tmp <- data.frame(b) |>
      dplyr::select(-x, -y) |>
      t() |>
      na.omit()
    sp <- rownames(tmp)[tmp == 1]
    pt_sample$n_links[i] <- sum(metaweb[sp, sp])
  }
  pt_sample <- dplyr::mutate(pt_sample, connectance = round(n_links / (species^2), 4))


  # ------------------------------------------------------------------
  # Assess effects and trophic sensitivity
  motifs <- rcea::triads(metaweb, trophic_sensitivity)
  system.time({
    meanT <- list()
    for (i in 1:nrow(pt_sample)) {
      # Data
      d <- drivers[, pt_sample$x_id[i], pt_sample$y_id[i]]
      b <- biotic[, pt_sample$x_id[i], pt_sample$y_id[i]]

      # Assessment
      direct_effect <- rcea::cea(d, b, species_sensitivity) |>
        rcea::make_array()
      direct_pathways <- rcea::cea_pathways(direct_effect, b)
      indirect_pathways <- rcea::ncea_pathways_(direct_pathways, motifs)
      motif_summary <- rcea::ncea_motifs(direct_effect, indirect_pathways)
      motif_effects <- rcea::ncea_effects(motif_summary, w_d = 0.5, w_i = 0.25)
      net <- rcea::get_net(motif_effects) |>
        dplyr::select(-id_cell) |>
        dplyr::rowwise() |>
        dplyr::mutate(ncea = sum(
          dplyr::across(!vc_id),
          na.rm = TRUE
        )) |>
        dplyr::select(vc_id, ncea)

      # # Direct effects
      # cea <- rowSums(direct_effect[1, , ])
      # cea <- data.frame(
      #   species = names(cea),
      #   cea = cea
      # ) |>
      #   na.omit()
      # row.names(cea) <- NULL

      # Mean sensitivity
      Ts <- indirect_pathways[[1]] |>
        dplyr::group_by(vc_id) |>
        dplyr::summarize(Sensitivity = mean(Sensitivity))

      # Combine
      meanT[[i]] <- dplyr::left_join(Ts, net, by = "vc_id") |>
        cbind(species = names(direct_effect[1, , 1])[!is.na(direct_effect[1, , 1])]) #|>
      # dplyr::left_join(cea, by = "species")

      # Remove species that are not present in the cell
      spid <- names(b)[which(data.frame(b) == 1) - 2]
      meanT[[i]] <- meanT[[i]] |>
        dplyr::filter(species %in% spid)


      print(glue::glue("{i} of {nrow(pt_sample)}\n"))
    }
  })

  # ------------------------------------------------------------------------------
  # Graph
  param()
  dat <- dplyr::bind_rows(meanT) |>
    dplyr::left_join(spList[, c("shortname", "gr1")], by = c("species" = "shortname"))

  dat2 <- dat |>
    dplyr::group_by(species, gr1) |>
    dplyr::summarize(Sensitivity = mean(Sensitivity), ncea = mean(ncea)) |>
    dplyr::group_by(gr1) |>
    dplyr::group_split()

  maxVal <- c(
    max(dat$Sensitivity),
    max(dat$ncea)
  )

  out <- here::here("figures")
  rcea::chk_create(out)
  png(here::here(out, "mean_trophic_sensitivity.png"), res = 900, width = 150, height = 150, units = "mm")
  par(mar = c(4.5, 4.5, 2, 2), family = "serif")
  graphicsutils::plot0(x = c(0, maxVal[1]), y = c(0, maxVal[2]))
  # axis(1, at = round(seq(0, maxVal[1], length.out = 5),2), labels = round(seq(0, maxVal[1], length.out = 5),2))
  # axis(2, at = round(seq(0, maxVal[2], length.out = 5),2), labels = round(seq(0, maxVal[2], length.out = 5),2))
  axis(1, at = 0:4, labels = 0:4)
  axis(2, at = 0:4, labels = 0:4)
  mtext(side = 1, text = "Mean trophic sensitivity", line = 2.5, font = 2)
  mtext(side = 2, text = "Cumulative effects", line = 2.5, font = 2)
  points(x = dat$Sensitivity, y = dat$ncea, pch = 16, cex = .15, col = "#00000033")
  points(x = dat2[[1]]$Sensitivity, y = dat2[[1]]$ncea, pch = 16, cex = 1, col = "#1BB2BAEE")
  points(x = dat2[[2]]$Sensitivity, y = dat2[[2]]$ncea, pch = 17, cex = 1, col = "#d89c42EE")

  # Legend
  points(x = 0, y = 3.5, pch = 16, cex = 1, col = "#1BB2BA")
  points(x = 0, y = 3.25, pch = 17, cex = 1, col = "#d89c42")
  points(x = 0, y = 3, pch = 16, cex = 0.5, col = "#000000")
  text(x = 0.15, y = 3.5, adj = c(0, .5), labels = "Invertebrates")
  text(x = 0.15, y = 3.25, adj = c(0, .5), labels = "Vertebrates")
  text(x = 0.15, y = 3, adj = c(0, .5), labels = "Cell values")
  dev.off()

  summary(lm(dat2[[1]]$ncea ~ dat2[[1]]$Sensitivity))
  summary(lm(dat2[[2]]$ncea ~ dat2[[2]]$Sensitivity))
}
