#' Global parameters for assessment
#'
#' @export

param <- function() {
  # Libraries
  library(graphicsutils)
  library(tidyverse)
  library(magrittr)
  library(stringr)
  
  # Individual colors
  focus  <<- '#25364A'
  off    <<- '#f2f2f2'
  colOcc <<- '#5461b8'
  colLines2 <<- "#c7cbce"

  # Color palettes
  colsPalette <<- c("#c7cbce", "#96a3a3", "#687677", "#222d3d", "#25364a", "#c77f20","#e69831", "#e3af16", "#e4be29", "#f2ea8b")
  palImpact <<- colorRampPalette(colsPalette)
  palImpact2 <<- colorRampPalette(c('#ffffff',colsPalette))
  colsPalette <<- c("#ffffff","#80B0D2","#EEB956","#E59144","#552010")
  palDist <<- colorRampPalette(colsPalette)
  palReg <<- colorRampPalette(c('#c8e6d399', '#031B1099'), alpha = T)
  palReg2 <<- colorRampPalette(c('#ffc9cb99', '#752F3C99'), alpha = T)
  palDist2 <<- colorRampPalette(c('#022441','#319FB2','#73C341','#F4D530','#300202'))


  # Graphical paramters
  # extFig <<- as.numeric(st_bbox(egslSimple)) + c(-1.5,-.5,.5,.5)
  extFig <<- c(-184094.7, 194566.3, 861938.7, 958534.7)
  prj <<- 32198

  # Spatial data
  # load('./Data/Spatial/quebec_ext.RData') # Canada
  # load('./Data/Spatial/US_ext.RData') # US
  # load('./Data/Grids/Data/egslSimple.RData') # Simple EGSL geometry
  # load('./Data/Spatial/egslSub.RData') # EGSL subregions
  # load('./Data/Spatial/egslOutline.RData') # EGSL outline (has different crs)

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Drivers
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

  # Driver names data.frame
  drNames <<- data.frame(var = character(),
                        full = character(),
                        accr = character(),
                        group = character(),
                        stringsAsFactors = F)

  drNames[1, ] <<- c("Acidification","Acidification","ACID","Cl")
  drNames[2, ] <<- c("CoastalDevelopment","Coastal development","CD","Co")
  drNames[3, ] <<- c("DirectHumanImpact","Direct human impact","DHI","Co")
  drNames[4, ] <<- c("FisheriesDD","Demersal destructive fisheries","DD","F")
  drNames[5, ] <<- c("FisheriesDNH","Demersal, non-destructive, high-bycatch fisheries","DNH","F")
  drNames[6, ] <<- c("FisheriesDNL","Demersal, non-destructive, low-bycatch fisheries","DNL","F")
  drNames[7, ] <<- c("FisheriesPHB","Pelagic, high-bycatch fisheries","PHB","F")
  drNames[8, ] <<- c("FisheriesPLB","Pelagic, low-bycatch fisheries","PLB","F")
  drNames[9, ] <<- c("Hypoxia","Hypoxia","HYP","Cl")
  drNames[10, ] <<- c("InorganicPollution","Inorganic pollution","IP","Co")
  drNames[11, ] <<- c("MarinePollution","Marine pollution","MP","T")
  drNames[12, ] <<- c("NutrientInput","Nutrient input","NI","Co")
  drNames[13, ] <<- c("OrganicPollution","Organic pollution","OP","Co")
  drNames[14, ] <<- c("NegativeSBT","Negative sea bottom temperature anomalies","SBT-","Cl")
  drNames[15, ] <<- c("PositiveSBT","Positive sea bottom temperature anomalies","SBT+","Cl")
  drNames[16, ] <<- c("Shipping","Shipping","SHP","T")
  drNames[17, ] <<- c("NegativeSST","Negative sea surface temperature anomalies","SST-","Cl")
  drNames[18, ] <<- c("PositiveSST","Positive sea surface temperature anomalies","SST+","Cl")
  drNames <<- dplyr::arrange(drNames, group, var)

  # Groups
  nGroup <<- length(unique(drNames$group))
  grNames <<- data.frame(accr = character(), name = character(), stringsAsFactors = F)
  grNames[1, ] <<- c('Cl','Climate')
  grNames[2, ] <<- c('Co','Coastal')
  grNames[3, ] <<- c('F','Fisheries')
  grNames[4, ] <<- c('T','Marine traffic')

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # At risk
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # https://www.canada.ca/en/environment-climate-change/services/species-risk-public-registry.html
  endangered <<- c('Delphinapterus.leucas','Balaenoptera.musculus','Eubalaena.glacialis',
                  'Hyperoodon.ampullatus','Balaenoptera.borealis','Morone.saxatilis')
  threatened <<- c('Anarhichas.minor','Anarhichas.denticulatus')
  concern <<- c('Balaena.mysticetus', 'Balaenoptera.physalus', 'Anarhichas.lupus','Malacoraja.senta')
  colEnd <<- '#9d3131'
  colThr <<- '#86a12e'
  colCon <<- '#455e8f'


  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Taxa targetted by fisheries
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  fish <<- c("Buccinum.sp.","Cancer.irroratus",
            "Chionoecetes.opilio","Chlamys.islandica",
            "Clupea.harengus","Cucumaria.frondosa",
            "Cyclopterus.lumpus","Ensis.leei",
            "Gadus.morhua","Glyptocephalus.cynoglossus",
            "Hippoglossoides.platessoides","Hippoglossus.hippoglossus",
            "Homarus.americanus","Hyas.araneus",
            "Limanda.ferruginea","Lophius.americanus",
            "Mactromeris.polynyma","Mallotus.villosus",
            "Pandalus.borealis","Pollachius.virens",
            "Pseudopleuronectes.americanus","Reinhardtius.hippoglossoides",
            "Scomber.scombrus","Scophthalmus.aquosus",
            "Sebastes.sp.","Spisula.solidissima",
            "Strongylocentrotus.sp.","Thunnus.thynnus",
            "Urophycis.tenuis","Xiphias.gladius")


  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Taxonomic groups
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  load('./Data/Taxonomy/TaxonomyEGSL.RData')
  load('./Data/FormatData/txNames.RData')
  load('./Data/FormatData/SpeciesList.RData')
  taxonomy <- str_split(taxonomy$taxonomy, pattern = ' | ', simplify = T) %>%
              .[, c(1,3,5,7,9,11)] %>%
              cbind(rownames(taxonomy), .) %>%
              set_colnames(c('Taxa','Kingdom','Phylum','Class','Order','Family','Genus')) %>%
              as.data.frame(stringsAsFactors = FALSE)

  sp <- sp[sp$species. %in% txNames, ]
  sp <- left_join(sp, taxonomy, by = c('sp' = 'Taxa'))

  # Manual groupings
  sp$gr1 <- ifelse(sp$Phylum == 'Chordata', 'Vertebrates', 'Invertebrates')
  sp$gr2 <- NA
  uid <- sp$Phylum %in% c('Annelida','Brachiopoda','Bryozoa','Ctenophora','Porifera')
  sp$gr2[uid] <- 'Others'
  sp$gr2[!uid] <- sp$Phylum[!uid]
  sp$gr2[sp$Phylum == 'Chordata'] <- sp$Class[sp$Phylum == 'Chordata']
  sp$gr2[sp$Class %in% c('Ascidiacea','Myxini','Elasmobranchii')] <- 'Others2'
  sp$gr3 <- 'X'
  sp$gr3[sp$Class == 'Asteroidea'] <- 'Asteroidea'
  sp$gr3[sp$Class == 'Holothuroidea'] <- 'Holothuroidea'
  sp$gr3[sp$Class == 'Ophiuroidea'] <- 'Ophiuroidea'
  sp$gr3[sp$Class == 'Bivalvia'] <- 'Bivalvia'
  sp$gr3[sp$Class == 'Cephalopoda'] <- 'Cephalopoda'
  sp$gr3[sp$Class == 'Gastropoda'] <- 'Gastropoda'
  sp$gr3[sp$Class == 'Echinoidea'] <- 'Echinoidea' 

  spList <<- sp |>
    dplyr::select(
      shortname = species.,
      scientific_name = species,
      scientific_name_no_sp = sp,
      gr1, gr2, gr3
    )
  rm(sp)

  ######
  # # If reclassify with quantiles, as in Halpern
  # x <<- quantile(connect, probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95), type=7,names = FALSE)
  # mat <<- matrix(0, nrow = length(x), ncol = 3)
  # mat[,1] <<- x
  # mat[,2] <<- c(x[2:nrow(mat)], maxValue(connect))
  # mat[,3] <<- 1:nrow(mat)
  # z <<- reclassify(connect, mat)
  # plot(z, col = palImpact(nrow(mat)))
  # plot(connect, col = palImpact(10))


  ### Group specific cumulative effects
  # with code in './Code/Figures/ContributionTaxa.R'
  # source('./Code/Functions/getMetric.R')
  # out <<- getMetric('Impact_Normalized')
  # plot(out)
  #
  # uid <<- which(names(out) %in% sp$species.[sp$gr1 == 'Vertebrates'])
  # ver <<- sum(out[[uid]], na.rm = TRUE)
  #
  # uid <<- which(names(out) %in% sp$species.[sp$gr1 == 'Invertebrates'])
  # inv <<- sum(out[[uid]], na.rm = TRUE)
  # plot(inv)
  #
  #
  # x <<- which(names(out) %in% sp$species.[sp$gr2 == 'Cnidaria'])
  # y <<- sum(out[[x]], na.rm = TRUE)
  # plot(y)
}