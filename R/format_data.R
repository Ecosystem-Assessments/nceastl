#' Script to format data in preparation for the assessment
#'
#' @export

format_data <- function() {
  out <- here::here("data", "FormatData")
  out2 <- here::here("data", "input")
  rcea::chk_create(out)
  rcea::chk_create(out2)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Species list
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  load("./Data/SpeciesList/SpeciesList.RData")

  # New column with sp. removed from taxa names
  sp$sp <- gsub("\\b sp.\\b", "", sp$species)

  # New columns with . instead of spaces
  sp$sp. <- gsub(" ", "\\.", sp$sp)
  sp$species. <- gsub(" ", "\\.", sp$species)

  # Export
  save(sp, file = here::here(out, "SpeciesList.RData"))
  write.csv(sp, file = here::here(out2, "SpeciesList.csv"), row.names = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Simulated trophic sensitivities
  # (Adapted from Beauchesne et al. in review hopefully)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  load("./Data/TrophicSensitivity/TrophicSensitivitySpecies.RData")
  trophic_sensitivity <- sensitivity
  save(trophic_sensitivity, file = here::here(out, "TrophicSensitivity.RData"))
  write.csv(trophic_sensitivity, file = here::here(out2, "TrophicSensitivity.csv"), row.names = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Biotic data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  load("./Data/Biotic/Biotic.RData")

  # Taxa names
  txNames <- names(biotic)

  # Transform data to matrix
  library(raster)
  bioticData <- as.matrix(biotic)

  # Identify cells with biotic data
  idBiotic <- apply(bioticData, 1, function(x) !all(is.na(x)))

  # Select only relevant data
  bt <- bioticData[idBiotic, ]

  # Add phytoplankton and zooplankton everywhere
  message("WARNING: We are assuming that phytoplankton and zooplankton are everywhere in the study area. A better assessment with proper data would be desirable.")
  bt <- cbind(bt, Zooplankton = 1, Phytoplankton = 1)
  biotic$Zooplankton <- biotic$Phytoplankton <- 1
  values(biotic$Zooplankton)[!idBiotic] <- values(biotic$Phytoplankton)[!idBiotic] <- NA

  # Transform biotic data as logical
  bt <- apply(bt, 2, as.logical)

  # Empty raster
  r <- biotic[[1]]
  names(r) <- ""
  values(r)[!is.na(values(r))] <- 0

  # Transform rasters as stars objects
  r <- stars::st_as_stars(r)
  biotic <- stars::st_as_stars(biotic) |>
    split()

  # Export
  save(txNames, file = here::here(out, "txNames.RData"))
  save(bt, file = here::here(out, "bt.RData"))
  save(biotic, file = here::here(out, "biotic.RData"))
  save(r, file = here::here(out, "emptyRaster.RData"))
  save(idBiotic, file = here::here(out, "idBiotic.RData"))

  # Export human readable
  write.csv(txNames, file = here::here(out2, "txNames.csv"), row.names = FALSE)
  write.csv(bt, file = here::here(out2, "bt.csv"), row.names = FALSE)
  write.csv(idBiotic, file = here::here(out2, "idBiotic.csv"), row.names = FALSE)
  stars::write_stars(r, dsn = here::here(out2, "r.tif"))

  out_biotic <- here::here(out2, "biotic")
  chk_create(out_biotic)
  for (i in seq_len(length(biotic))) {
    stars::write_stars(
      biotic,
      dsn = here::here(out_biotic, glue::glue("{names(biotic)[i]}.tif")),
      layer = i
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Metaweb
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  load("./Data/Metaweb/metaweb.RData")
  diag(metaweb) <- 0

  # Change species names
  # There are MUCH better ways to do this, but I'm tired and can't think straight
  for (i in 1:ncol(metaweb)) {
    idnm <- sp$sp %in% colnames(metaweb)[i]
    if (any(idnm)) colnames(metaweb)[i] <- rownames(metaweb)[i] <- sp$species.[i]
  }

  # Take only species in biotic dataset
  uid <- colnames(metaweb) %in% colnames(bt)
  metaweb <- metaweb[uid, uid]
  all(colnames(bt) == colnames(metaweb))

  # Export
  save(metaweb, file = here::here(out, "metaweb.RData"))
  write.csv(metaweb, file = here::here(out2, "metaweb.csv"), row.names = TRUE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Species-specific vulnerabilities
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  load("./Data/Vulnerability/Vulnerability.RData")

  if (!all(rownames(vulnerability) == sp$species)) {
    stop("Names in vulnerability dataset should be the same as in the species list")
  }

  # Change taxa names
  rownames(vulnerability) <- sp$species.

  # Add phytoplankton and zooplankton with no vulnerability
  message("WARNING: We are assuming that phytoplankton and zooplankton are insensitive to all drivers. A better assessment with proper data would be desirable.")
  vulnerability <- rbind(vulnerability, Zooplankton = 0, Phytoplankton = 0)

  # Take only species in biotic dataset
  uid <- rownames(vulnerability) %in% colnames(bt)
  vulnerability <- vulnerability[uid, ]
  all(colnames(bt) == rownames(vulnerability))

  # Export
  species_sensitivity <- vulnerability
  save(species_sensitivity, file = here::here(out, "species_sensitivity.RData"))
  write.csv(species_sensitivity, file = here::here(out2, "species_sensitivity.csv"), row.names = TRUE)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Drivers data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  library(raster)
  load("./Data/Drivers/Drivers.RData")

  # Transform data to matrix
  driversData <- as.matrix(drivers)

  # Select only relevant data
  # Locations with information on taxa distribution
  dr <- driversData[idBiotic, ]

  # Drivers for which we have vulnerability assessment
  uid <- colnames(dr) %in% colnames(vulnerability)
  dr <- dr[, uid]
  drivers <- drivers[[colnames(vulnerability)]]

  # Make sure that the order of the vulnerability db column is the same as
  # the order of the columns in the drivers db
  uid <- sort(colnames(dr))
  dr <- dr[, uid]
  vulnerability <- vulnerability[, uid]
  all(colnames(dr) == colnames(vulnerability))

  # Transform as stars object
  drivers <- stars::st_as_stars(drivers) |>
    split()

  # Export
  save(dr, file = here::here(out, "drivers.RData"))
  save(drivers, file = here::here(out, "driversRaster.RData"))

  # Export human readable
  write.csv(dr, file = here::here(out2, "dr.csv"), row.names = FALSE)
  out_drivers <- here::here(out2, "drivers")
  chk_create(out_drivers)
  for (i in seq_len(length(drivers))) {
    stars::write_stars(
      drivers,
      dsn = here::here(out_drivers, glue::glue("{names(drivers)[i]}.tif")),
      layer = i
    )
  }


  # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # # Evaluate metaweb motif triplets
  # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # # Triads
  # triads <- motif_census_triplet(metaweb)
  #
  # # Select only motifs of interest
  # motifs <- c('exploitative competition','linear chain','apparent competition','omnivory')
  # uid <- triads$name_uni %in% motifs
  # triads <- triads[uid, ]
  #
  # # Modify index for positions i,j,k (currently starts at 0)
  # triads$i <- triads$i + 1
  # triads$j <- triads$j + 1
  # triads$k <- triads$k + 1
  #
  # # Modify position numbers
  # # Species:
  # #   1: exploitative competition bottom
  # #   2: exploitative competition top
  # #   3: linear chains bottom
  # #   4: linear chains middle
  # #   5: linear chains top
  # #   9: apparent competition bottom
  # #  10: apparent competition top
  # #  11: omnivory bottom
  # #  12: omnivory middle
  # #  13: omnivory top
  #
  # mid <- data.frame(motifcensus = c(1,2,3,4,5,9,10,11,12,13),
  #                   id = c(1,3,1,2,3,1,3,1,2,3))
  #
  # # Import in the table
  # triads <- triads %>%
  #           left_join(., mid, by = c("pos_i" = "motifcensus")) %>%
  #           rename(pi = id) %>%
  #           left_join(., mid, by = c("pos_j" = "motifcensus")) %>%
  #           rename(pj = id) %>%
  #           left_join(., mid, by = c("pos_k" = "motifcensus")) %>%
  #           rename(pk = id)
  #
  # # Transform duplicated positions
  # triads$pj[triads$pj == triads$pi] <- 2
  # triads$pj[triads$pj == triads$pk] <- 2
  # triads$pk[triads$pk == triads$pi] <- 2
  #
  # # Export
  # save(triads, file = './Data/FormatData/triads.RData')
}
