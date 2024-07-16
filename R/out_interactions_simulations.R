#' Script to extract the mean trophic sensitivity per km2 for all species
#'
#' @export

out_cea_km2 <- function() {
  # Output
  out <- here::here("output", "interactions_simulations")
  rcea::chk_create(out)

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
  set.seed(160)
  pt_sample <- lapply(df, function(x) {
    x[sample(seq_len(nrow(x)), 1), ]
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
  # Function for simulations
  simul <- function(x_id, y_id, n_simulations = 100, n_removal = seq(1, 400, by = 5), id = "") {
    # Load files for analysis
    load("data/FormatData/driversRaster.RData")
    load("data/FormatData/biotic.RData")
    load("data/FormatData/species_sensitivity.RData")
    load("data/FormatData/metaweb.RData")
    load("data/FormatData/TrophicSensitivity.RData")

    # Subset drivers and biotic
    d <- drivers[, x_id, y_id]
    b <- biotic[, x_id, y_id]

    # Subset metaweb
    tmp <- data.frame(b) |>
      dplyr::select(-x, -y) |>
      t() |>
      na.omit()
    sp <- rownames(tmp)[tmp == 1]
    meta <- metaweb[sp, sp]

    # Original assessment cumulative effects
    ori <- rcea::ncea(d, b, species_sensitivity, meta, trophic_sensitivity, exportAs = "df")

    # Simulations
    sim_res <- list()
    # i = number of interactions to remove
    # j = number of of times to remove interactions
    # for (i in seq_len(n_removal)) {
    for (i in seq_len(length(n_removal))) {
      sim_res[[i]] <- list()
      for (j in seq_len(n_simulations)) {
        # Randomly remove interactions
        int <- which(meta == 1)
        int_remove <- sample(int, n_removal[i], replace = FALSE)
        meta_remove <- meta
        meta_remove[int_remove] <- 0

        # Assessment
        sim_res[[i]][[j]] <- rcea::ncea(d, b, species_sensitivity, meta_remove, trophic_sensitivity, exportAs = "df")
        for (k in c("net", "direct", "indirect")) {
          sim_res[[i]][[j]][[k]] <- sim_res[[i]][[j]][[k]] |>
            dplyr::select(-id_cell) |>
            dplyr::mutate(
              effects = rowSums(dplyr::across(c(-vc_id))),
              remove_inter = n_removal[i],
              simulation = j,
              type = k
            ) |>
            dplyr::select(remove_inter, simulation, type, vc_id, effects)
        }
        sim_res[[i]][[j]] <- dplyr::bind_rows(sim_res[[i]][[j]][c("net", "direct", "indirect")])
        save(sim_res, file = here::here(out, glue::glue("simulation_results{id}.RData")))
      }
      sim_res[[i]] <- dplyr::bind_rows(sim_res[[i]])
      save(sim_res, file = here::here(out, glue::glue("simulation_results{id}.RData")))
    }
    sim_res <- dplyr::bind_rows(sim_res)
    save(sim_res, file = here::here(out, glue::glue("simulation_results{id}.RData")))
  }

  # ------------------------------------------------------------------
  # Simulations
  # sim_res <- apply(pt_sample, 1, function(x) simul(x$x_id, x$y_id))
  sim_res1 <- simul(pt_sample$x_id[1], pt_sample$y_id[1], 100, id = "1_all")
  sim_res2 <- simul(pt_sample$x_id[2], pt_sample$y_id[2], 100, id = "2_all")
  sim_res3 <- simul(pt_sample$x_id[3], pt_sample$y_id[3], 100, id = "3_all")
  sim_res4 <- simul(pt_sample$x_id[4], pt_sample$y_id[4], 100, id = "4_all")


  # ------------------------------------------------------------------
  # Comparisons (all of this should be included in the function)
  out <- here::here("figures")
  rcea::chk_create(out)
  library(graphicsutils)
  png(here::here(out, "interactions_simulations.png"), res = 300, width = 400, height = 200, units = "mm", pointsize = 10)
  layout(matrix(nrow = 2, data = 1:8))
  for (cell in 1:4) {
    load(glue::glue("output/interactions_simulations/simulation_results_{cell}.RData"))
    tmp <- sim_res
    # load(glue::glue("output/interactions_simulations/simulation_results{cell}_2.RData"))
    # tmp <- dplyr::bind_rows(tmp, sim_res)
    # load(glue::glue("output/interactions_simulations/simulation_results{cell}.RData"))
    # tmp <- dplyr::bind_rows(tmp, sim_res)

    x_id <- pt_sample$x_id[cell]
    y_id <- pt_sample$y_id[cell]
    d <- drivers[, x_id, y_id]
    b <- biotic[, x_id, y_id]

    # Subset metaweb
    tmp <- data.frame(b) |>
      dplyr::select(-x, -y) |>
      t() |>
      na.omit()
    sp <- rownames(tmp)[tmp == 1]
    meta <- metaweb[sp, sp]
    metaH <- metaweb
    metaH[sp, ] <- 0
    metaH[, sp] <- 0
    index <- dplyr::left_join(
      data.frame(
        sp = colnames(metaweb),
        id = 1:nrow(metaH)
      ),
      data.frame(
        sp = sp,
        id2 = 1:length(sp)
      ),
      by = "sp"
    ) |>
      dplyr::select(-sp)

    # Original assessment cumulative effects
    ori <- rcea::ncea(d, b, species_sensitivity, meta, trophic_sensitivity, exportAs = "df")
    for (k in c("net", "direct", "indirect")) {
      ori[[k]] <- ori[[k]] |>
        dplyr::select(-id_cell) |>
        dplyr::mutate(
          effects = rowSums(dplyr::across(c(-vc_id))),
          type = k
        ) |>
        dplyr::select(type, vc_id, effects_original = effects)
    }
    ori <- dplyr::bind_rows(ori[c("net", "direct", "indirect")])

    # Disconnected assessment cumulative effects
    halp <- rcea::ncea(d, b, species_sensitivity, metaH, trophic_sensitivity, exportAs = "df")
    for (k in c("net", "direct", "indirect")) {
      halp[[k]] <- halp[[k]] |>
        dplyr::left_join(index, by = c("vc_id" = "id")) |>
        dplyr::select(-id_cell, -vc_id) |>
        dplyr::rename(vc_id = id2) |>
        dplyr::mutate(
          effects = rowSums(dplyr::across(c(-vc_id))),
          type = k
        ) |>
        dplyr::select(type, vc_id, effects_halpern = effects)
    }
    halp <- dplyr::bind_rows(halp[c("net", "direct", "indirect")]) |>
      na.omit()


    # Delta effect
    delta <- sim_res |>
      dplyr::left_join(ori, by = c("vc_id", "type")) |>
      dplyr::mutate(
        remove_inter_percent = remove_inter / pt_sample$n_links[cell],
        delta = effects - effects_original,
      ) |>
      dplyr::left_join(halp, by = c("vc_id", "type")) |>
      dplyr::mutate(delta_halp = effects - (2 * effects_halpern)) |>
      dplyr::filter(type == "net")

    # Mean delta effects
    mean_delta <- delta |>
      dplyr::group_by(vc_id, type, remove_inter, remove_inter_percent) |>
      dplyr::summarize(
        mean_delta = mean(delta),
        mean_delta_halp = mean(delta_halp)
      )

    # Divide by type
    delta <- dplyr::group_by(delta, type) |> dplyr::group_split()
    mean_delta <- dplyr::group_by(mean_delta, type) |> dplyr::group_split()


    # Graph
    for (i in 1) {
      dat <- delta[[i]]
      mean_dat <- mean_delta[[i]]
      # R <- range(dat$delta)
      R <- c(-1.5, 1.5)
      par(mar = c(4.5, 4.5, 2, 2))
      plot0(x = c(0, .75), y = c(R[1], R[2]))
      axis(1, at = seq(0, 1, length.out = 5), labels = seq(0, 1, length.out = 5))
      axis(2, at = seq(R[1], R[2], length.out = 5), labels = seq(round(R[1], 4), round(R[2], 4), length.out = 5))
      mtext(side = 1, text = "Interactions removed (%)", line = 2.5, font = 1, cex = .8)
      mtext(side = 2, text = "Effect after removal - effects with all interactions", line = 2.5, font = 1, cex = .8)
      if (cell == 2) mtext(side = 3, text = "Absolute difference in effects", line = .5, font = 2, cex = .9, at = .9)
      lines(x = c(0, 1), y = c(0, 0), col = "#000000BB", lty = 2, lwd = 2)
      points(x = dat$remove_inter_percent, y = dat$delta, pch = 16, cex = 0.2, col = "#00000088")
      points(x = mean_dat$remove_inter_percent, y = mean_dat$mean_delta, pch = 16, cex = .7, col = "#1BB2BACC")

      # Infos on cells
      d <- ifelse(pt_sample$direct[cell], "Direct > indirect effects", "Direct < indirect effects")
      y0 <- 1.5
      yG <- .06
      text(x = 0, y = y0, adj = c(0, .5), labels = d, cex = .75)
      text(x = 0, y = y0 - yG, adj = c(0, .5), labels = glue::glue("Species: {pt_sample$species[cell]}"), cex = .75)
      text(x = 0, y = y0 - 2 * yG, adj = c(0, .5), labels = glue::glue("Interactions: {pt_sample$n_links[cell]}"), cex = .75)
      text(x = 0, y = y0 - 3 * yG, adj = c(0, .5), labels = glue::glue("Connectance: {pt_sample$connectance[cell]}"), cex = .75)
    }
    for (i in 1) {
      dat <- delta[[i]]
      mean_dat <- mean_delta[[i]]
      # R <- range(dat$delta)
      R <- c(-5, 5)
      par(mar = c(4.5, 4.5, 2, 2))
      plot0(x = c(0, .75), y = c(R[1], R[2]))
      axis(1, at = seq(0, 1, length.out = 5), labels = seq(0, 1, length.out = 5))
      axis(2, at = seq(R[1], R[2], length.out = 5), labels = seq(round(R[1], 4), round(R[2], 4), length.out = 5))
      mtext(side = 1, text = "Interactions removed (%)", line = 2.5, font = 1, cex = .8)
      mtext(side = 2, text = "Effects after interactions removal - 2 * disconnected effects", line = 2.5, font = 1, cex = .8)
      if (cell == 2) mtext(side = 3, text = "Difference between effects and 2*disconnected", line = .5, font = 2, cex = .9, , at = .9)
      lines(x = c(0, 1), y = c(0, 0), col = "#000000BB", lty = 2, lwd = 2)
      points(x = dat$remove_inter_percent, y = dat$delta_halp, pch = 16, cex = 0.2, col = "#00000088")
      points(x = mean_dat$remove_inter_percent, y = mean_dat$mean_delta_halp, pch = 16, cex = .7, col = "#1BB2BACC")
    }
  }
  dev.off()
}
