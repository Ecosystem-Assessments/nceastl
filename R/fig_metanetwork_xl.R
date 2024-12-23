#' Export figures for metanetwork
#'
#' @describeIn fig_metanetwork metanetwork figure
#' @export
fig_metanetwork_xl <- function() {
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Libraries
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  library(raster)
  library(tidyverse)
  param()
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Data
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Impacts per km2
  # cekm <- readRDS('./Data2/Results/Cumulative_Effects/CumulativeEffects_Area.rds')
  cekm <- read.csv("output/cea_km2/ncea_km2.csv") |>
          dplyr::rowwise() |>
          dplyr::mutate(ncea = sum(
            dplyr::across(!species), 
            na.rm = TRUE
          )) |>
          dplyr::rename(Taxa = species)

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors for stressors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    col <- hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n] |> 
           lapply(graphicsutils::darken, percentage = 30) |>
           unlist() 
    glue::glue("{col}80")
  }
  
  # Colors for species 
  colInv <- '#226c61BB'
  colVer <- '#6a5024BB'
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Function to add transparent nodes for spacing between each node groups
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  randomString <- function() paste0(letters[runif(20,1,26)], collapse = '')
  insertRow <- function(dat, group, network) {
    # New row to add 
    newrow <- data.frame(
      group = group, 
      network = network, 
      name = randomString(), 
      cex = 0, 
      cols = '#00000000'
    )
    
    # ID in data.frame where to add rows
    uid <- which(dat$network == network) |>
           sort(decreasing = TRUE)
    
    # Add rows
    for(i in uid) {
      dat <- dplyr::add_row(dat, newrow, .after = i)
      dat <- dplyr::add_row(dat, newrow, .before = i)
    }

    # Return 
    dat    
  }
  
  insertRow_ <- function(dat, group, network, nrep) {
    for(i in 1:nrep) {
      dat <- insertRow(dat, group, network)    
    }  
    dat
  }
  
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Plotting function
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#  
  arctext2 <- function(var1, l1, l2, cl = TRUE, cx = .9) {
    uid <- metanetwork$networkGroup$Var1 == var1
    middle <- mean(c(metanetwork$networkGroup$lower[uid],
                     metanetwork$networkGroup$upper[uid]))
    plotrix::arctext(x = as.character(l1),radius = rad2-.02, middle = middle, 
                     col = "#ffffff", clockwise = cl, font = 2, cex = cx)
    plotrix::arctext(x = as.character(l2), radius = rad1+.02, middle = middle, 
                     col = "#ffffff", clockwise = cl, font = 2, cex = cx)  
  }

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Data
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#  
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
  spGroup <- dplyr::select(spList, gr1, gr2) |>
             dplyr::distinct() |> 
             dplyr::arrange(gr1, gr2) |>
             dplyr::mutate(col = ifelse(gr1 == "Invertebrates", colInv, colVer))
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Organize data
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Reorder data
  cekm <- left_join(cekm, spList, by = c("Taxa" = "shortname")) |>
          arrange(gr1, gr2, desc(ncea))

  # Groups
  gr1 <- cekm[, 'gr1', drop = FALSE] |>
         dplyr::mutate(id = 1:dplyr::n()) |>
         dplyr::group_by(gr1) |>
         dplyr::summarize(min = min(id), max = max(id)) |>
         as.data.frame(stringsAsFactors = FALSE)

  gr2 <- cekm[, 'gr2', drop = FALSE] %>%
         dplyr::mutate(id = 1:dplyr::n()) %>%
         dplyr::group_by(gr2) %>%
         dplyr::summarize(min = min(id), max = max(id)) %>%
         as.data.frame(stringsAsFactors = FALSE)
  gr2$gr2 <- gsub('Others2','Others',gr2$gr2)

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Metaweb
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  load(here::here("data","metaweb","metaweb.RData"))
  metaweb <- metaweb[cekm$scientific_name_no_sp, cekm$scientific_name_no_sp]
  colnames(metaweb) <- rownames(metaweb) <- cekm$Taxa

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Link and nodes for metaweb
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Extract links from metaweb
  uid <- which(metaweb == 1, arr.ind = T)
  links <- matrix(nrow = nrow(uid), ncol = 2, dimnames = list(c(), c('from','to')))
  for(i in 1:nrow(uid)) {
    links[i, 'from'] <- colnames(metaweb)[uid[i,1]]
    links[i, 'to'] <- colnames(metaweb)[uid[i,2]]
  }
  linksTx <- as.data.frame(links)

  # Add color for links
  # linksTx$cols <- '#99836211'
  linksTx$cols <- '#e3e1e1'

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Nodes list with proper groups included
  nodesTx <- data.frame(
    group = cekm$gr1, 
    network = cekm$gr2, 
    name = cekm$Taxa,
    cex = cekm$ncea*.6
  )
  
  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Add colors to node now
  metanetwork <- list()
  metanetwork$nodes <- nodesTx
  metanetwork$links <- linksTx

  # Colors
  metanetwork$networkGroup <- bound(metanetwork, order = spGroup$gr2)
  metanetwork <- colGroups(metanetwork, colPal = spGroup$col)
  nodesTx <- metanetwork[[1]]
  colGr <- metanetwork[[3]][,c('Var1','cols')]

  # # Add transparent nodes for spacing
  # nodesTx <- insertRow_(nodesTx, "Invertebrates", "Others", 1)
  # nodesTx <- insertRow_(nodesTx, "Vertebrates", "Others2", 1)
  # 
  # # Remove last line if empty
  # ll <- nrow(nodesTx)
  # if (is.na(nodesTx$group[ll])) nodesTx <- nodesTx[-ll, ]

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Link and nodes for drivers
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Identify columns with total effects per stressor
  uid <- colnames(cekm) %in% drList$drivers
  dat <- cekm[, uid]
  dat <- round(dat, 2)

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Links with taxa
  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # For now, only select links > 0.05
  uid <- which(dat > 0.00, arr.ind = T)
  links <- matrix(nrow = nrow(uid), ncol = 2, dimnames = list(c(), c('from','to')))
  for(i in 1:nrow(uid)) {
    links[i, 'from'] <- colnames(dat)[uid[i,2]]
    links[i, 'to'] <- cekm$Taxa[uid[i,1]]
  }
  linksDr <- as.data.frame(links, stringsAsFactors = FALSE)

  # Add color for links
  linksDr <- left_join(linksDr, drList[,c('drivers','cols')], by = c('from' = 'drivers'))

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Nodes
  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Node size
  cexDr <- data.frame(
    drivers = colnames(dat), 
    cex = log(colMeans(dat, na.rm = TRUE)+1)*10+.2
  )

  nodesDr <- dplyr::select(
    drList, 
    network = group,
    name = drivers,
    cols
  ) |>
  dplyr::mutate(group = "Stressors") |>
  dplyr::left_join(cexDr, by = c("name" = "drivers")) |>
  dplyr::select(group, network, name, cex, cols)
  
  # Add transparent nodes for spacing
  nodesDr <- insertRow_(nodesDr, "Stressors", "Marine traffic", 2)
  nodesDr <- insertRow_(nodesDr, "Stressors", "Fisheries", 1)
  nodesDr <- insertRow_(nodesDr, "Stressors", "Coastal", 1)
  nodesDr <- insertRow_(nodesDr, "Stressors", "Climate", 1)
    
  # Remove last line if empty
  ll <- nrow(nodesDr)
  if (is.na(nodesDr$group[ll])) nodesDr <- nodesDr[-ll, ]

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Combine
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Combine in a single object
  metanetwork <- vector('list', 0)
  metanetwork$nodes <- rbind(nodesDr, nodesTx)
  metanetwork$links <- rbind(linksTx, linksDr)
    

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Graph elements
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Network order
  orderNet <- c(unique(nodesDr$network), unique(nodesTx$network))

  # Network boundaries
  metanetwork$networkGroup <- bound(metanetwork, order = orderNet)

  # Node coordinates
  metanetwork <- nodePos(metanetwork, edgeRad = .875, groupRad = .6)
  
  # Add labels 
  labs <- nodePos(metanetwork, edgeRad = .925, groupRad = .6)
  labs <- labs$nodes |>
    dplyr::filter(cex > 0) |>
    dplyr::group_by(group, network) |>
    dplyr::mutate(lab = 1:dplyr::n()) |>
    dplyr::ungroup()

  # Manually add colors
  # metanetwork[[3]]$cols <- NA
  metanetwork[[3]] <- dplyr::left_join(
    metanetwork[[3]], 
    drGroup[, c('group','cols')], 
    by = c('Var1' = 'group')
  )
  metanetwork[[3]]$cols[match(colGr$Var1, metanetwork[[3]]$Var1)] <- colGr$cols

  # Others
  rad1 = .95
  rad2 = 1.25
  shadowEdge = TRUE
  
  # # Output
  out <- here::here("figures")
  rcea::chk_create(out)
  
  png(
    here::here(out, "metanetwork_xl.png"), 
    res = 300, 
    width = 400, 
    height = 400,
    units = "mm"
  )
  
  # Plot
  par(mar = c(0,0,0,0), bg = "#ffffff")
  graphicsutils::plot0(x = c(-1.1, 1.1))
  
  # Adjust some group names
  uid <- metanetwork$networkGroup$Var1 == "Others2"
  metanetwork$networkGroup$Var1[uid] <- 'Others'
  # uid <- metanetwork$networkGroup$Var1 == "Marine traffic"
  # metanetwork$networkGroup$Var1[uid] <- '.'
  boxGroup(metanetwork,
           rad1 = rad1,
           colBox = metanetwork$networkGroup$cols,
           colNames = "#ffffff",
           border = 'transparent',
           # border = '#000000',
           cexNetwork = 1.25)
  # arctext2(".", "Marine", "traffic")
  
  plotLinks(metanetwork, cols = metanetwork$links$cols, lwd = 0.5)
  
  if (shadowEdge) {
    points(metanetwork$nodes$x,
           metanetwork$nodes$y,
           pch = 20,
           cex = (metanetwork$nodes$cex * 5),
           col = '#d7d7d7')
  }
  
  points(metanetwork$nodes$x,
         metanetwork$nodes$y,
         pch = 20,
         cex = (metanetwork$nodes$cex * 3),
         col = metanetwork$nodes$cols)
  
  # Point labels 
  text(x = labs$x, y = labs$y, labels = labs$lab, cex = .45)
  
  # Add Vertebrates, Invertebrates and Stressors
  metanetwork$nodes$network <- metanetwork$nodes$group
  metanetwork$networkGroup <- bound(metanetwork, order = unique(metanetwork$nodes$network))
  metanetwork <- nodePos(metanetwork, edgeRad = .875, groupRad = .5)
  
  boxGroup2(metanetwork,
           rad1 = 1.03, rad2 = 1.13,
           colBox = '#00000000', colNames = '#000000',
           border = '#000000',
           cexNetwork = 2)
  
  dev.off()
  
  # ------------------------------------------------------------------------------------------
  # Legend species & stressors
  png(
    here::here(out, "metanetwork_legend.png"), 
    res = 300, 
    width = 400, 
    height = 100,
    units = "mm"
  )

  # Plot
  par(mar = c(.5,.5,.5,.5), bg = "#ffffff")
  graphicsutils::plot0(x = c(1, 13.75), y = c(3, -31))
  
  # Labels
  labs <- dplyr::left_join(labs, drList[,c("drivers","full")], by = c("name" = "drivers")) |>
    dplyr::left_join(spList[c("shortname","scientific_name")], by = c("name" = "shortname"))
  idSp <- !is.na(labs$scientific_name)
  idSt <- !is.na(labs$full)
  labs$name <- c(labs$full[idSt], labs$scientific_name[idSp])
  
  # Graphical elements 
  cexT <- 0.7
  cexL <- 0.4
  labs$name = glue::glue("{labs$lab}: {labs$name}")
  
  # Boxes
  x <- c(1:6, 8:13)
  x2 <- x + 1
  x2[6] <- x2[6]+1
  xG <- 0.05
  nX <- length(x)
  y <- rep(1, nX)
  yG <- 0.75
  pols <- data.frame(
    x1 = x-xG, x2 = x2-(xG*2),
    y1 = y-yG, y2 = y+yG,
    col = c(rep(colInv, 5), rep(colVer, 3), gg_color_hue(4))
  )
  rect(pols$x1, pols$y1, pols$x2, pols$y2, col = pols$col, border = "#00000000")
  
  # Titles 
  y <- 2.2
  lines(x = c(1-xG, 6-(2*xG)), y = c(y,y), lwd = 1.5)
  lines(x = c(6-xG, 10-(2*xG)), y = c(y,y), lwd = 1.5)
  lines(x = c(10-xG, 14-(2*xG)), y = c(y,y), lwd = 1.5)
  text(
    x = c(1,6,10)-xG,
    y = c(3.45,3.45,3.45),
    labels = c("Invertebrates","Vertebrates","Stressors"),
    cex = .9, 
    col = "#000000",
    font = 2,
    adj = c(0,.5)
  )
  
  # Invertebrates 
  uid <- labs$group == "Invertebrates"
  dat <- labs[uid, ]
  ct <- unique(dat$network)
  nct <- length(ct)
  for(i in 1:nct) {
    uid <- dat$network == ct[i]
    tmp <- dat[uid, ]
    text(x = i, y = 1, labels = ct[i], cex = cexT, adj = c(0,.5), font = 2, col = "#ffffff")
    text(x = i, y = -tmp$lab, labels = tmp$name, cex = cexL, adj = c(0,1))    
  }

  # Vertebrates 
  uid <- labs$group == "Vertebrates"
  dat <- labs[uid, ]
  
  # Fishes
  uid <- dat$network == "Actinopterygii"
  tmp <- dat[uid, ]
  text(x = 6, y = 1, labels = "Actinopterygii", cex = cexT, adj = c(0,.5), font = 2, col = "#ffffff")
  text(x = 6, y = -1:-31, labels = tmp$name[1:31], cex = cexL, adj = c(0,1))    
  text(x = 7, y = -1:-31, labels = tmp$name[32:62], cex = cexL, adj = c(0,1))    

  # Mammalia
  uid <- dat$network == "Mammalia"
  tmp <- dat[uid, ]
  text(x = 8, y = 1, labels = "Mammalia", cex = cexT, adj = c(0,.5), font = 2, col = "#ffffff")
  text(x = 8, y = -tmp$lab, labels = tmp$name, cex = cexL, adj = c(0,1))    

  # Others
  uid <- dat$network == "Others2"
  tmp <- dat[uid, ]
  text(x = 9, y = 1, labels = "Others", cex = cexT, adj = c(0,.5), font = 2, col = "#ffffff")
  text(x = 9, y = -tmp$lab, labels = tmp$name, cex = cexL, adj = c(0,1))    

  # Stressors 
  uid <- labs$group == "Stressors"
  dat <- labs[uid, ]
  ct <- unique(dat$network)
  nct <- length(ct)
  for(i in c(2,4)) {
    uid <- dat$network == ct[i]
    tmp <- dat[uid, ]
    text(x = i+9, y = 1, labels = ct[i], cex = cexT, adj = c(0,.5), font = 2, col = "#ffffff")
    text(x = i+9, y = -tmp$lab, labels = tmp$name, cex = cexL, adj = c(0,1))    
  }
  
  # Stressors 
  xG <- 0.065
  uid <- labs$group == "Stressors"
  dat <- labs[uid, ]
  dat <- dat |>
    dplyr::mutate(
      full = stringr::str_replace(full,
      "Negative sea bottom temperature anomalies",
      "Negative sea bottom\ntemperature anomalies"
      )
    ) |>
    dplyr::mutate(
      full = stringr::str_replace(full,
      "Negative sea surface temperature anomalies",
      "Negative sea surface\ntemperature anomalies"
      )
    ) |>  
    dplyr::mutate(
      full = stringr::str_replace(full,
      "Positive sea bottom temperature anomalies",
      "Positive sea bottom\ntemperature anomalies"
      )
    ) |>
    dplyr::mutate(
      full = stringr::str_replace(full,
      "Positive sea surface temperature anomalies",
      "Positive sea surface\ntemperature anomalies"
      )
    ) |>
    dplyr::mutate(
      full = stringr::str_replace(full,
      "Demersal, non-destructive, high-bycatch fisheries",
      "Demersal, non-destructive,\nhigh-bycatch fisheries"
      )
    ) |>
    dplyr::mutate(
      full = stringr::str_replace(full,
      "Demersal, non-destructive, low-bycatch fisheries",
      "Demersal, non-destructive,\nlow-bycatch fisheries"
      )
    )

  # Climate
  uid <- dat$network == "Climate"
  tmp <- dat[uid, ]
  y <- c(-1,-2,-3,-5,-7,-9)
  text(x = 10, y = 1, labels = "Climate", cex = cexT, adj = c(0,.5), font = 2, col = "#ffffff")
  text(x = 10, y = y, labels = glue::glue("{1:6}:"), cex = cexL, adj = c(0,1))    
  text(x = 10+xG, y = y, labels = tmp$full, cex = cexL, adj = c(0,1))    

  # Fisheries
  uid <- dat$network == "Fisheries"
  tmp <- dat[uid, ]
  y <- c(-1,-2,-4,-6,-7)
  text(x = 12, y = 1, labels = "Fisheries", cex = cexT, adj = c(0,.5), font = 2, col = "#ffffff")
  text(x = 12, y = y, labels = glue::glue("{1:5}:"), cex = cexL, adj = c(0,1))    
  text(x = 12+xG, y = y, labels = tmp$full, cex = cexL, adj = c(0,1))    
  
  dev.off()
  # ------------------------------------------------------------------------------------------
  
  
  # Combine figures together
  i1 <- magick::image_read(here::here(out, "metanetwork_xl.png"))
  i2 <- magick::image_read(here::here(out, "metanetwork_legend.png"))
  img <- magick::image_append(c(i1,i2), stack = TRUE)
  
  magick::image_write(
    img, 
    path = here::here(out, "metanetwork_xl.png"),
    format = "png"
  )
  
  file.remove(here::here(out, "metanetwork_legend.png"))
}
