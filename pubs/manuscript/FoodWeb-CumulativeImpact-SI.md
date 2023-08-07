---
title: Interactions amplify cumulative effects in marine ecosystems
fontsize: 12pt
output:
  pdf_document:
    toc: false
    number_sections: false
header-includes:
   # - \usepackage{lineno}
   # - \linenumbers
   - \usepackage{listings}
   # - \usepackage{titling}
   # - \pretitle{\begin{flushleft}}
   # - \posttitle{\end{flushleft}}
   - \usepackage{float}
   - \restylefloat{table}
   - \usepackage{setspace}
   - \usepackage{longtable}
   - \usepackage{array}
   - \usepackage{multirow}
   - \usepackage{booktabs}
   - \usepackage{wrapfig}
   - \usepackage{colortbl}
   - \usepackage{pdflscape}
   - \usepackage{tabu}
   - \usepackage{threeparttable}
   - \usepackage{threeparttablex}
   - \usepackage[normalem]{ulem}
   - \usepackage{makecell}
   - \usepackage{xcolor}
   - \righthyphenmin=62
   - \lefthyphenmin=62
   - \usepackage{cite}
# bibliography: FoodWeb-CumulativeImpact-SI.bib
# csl: frontiers.csl
# link-citations: yes
# relativeurls: true
# nocite: '@*'
---

<!--
rmarkdown::render('./FoodWeb-CumulativeImpact-SI.md')
rmarkdown::render('./FoodWeb-CumulativeImpact-SI.md', output_format = "word_document")
-->

\begin{center}
David Beauchesne$^{1,2\*}$, Kevin Cazelles$^3$, Rémi M. Daigle${^4,5}$, Dominique Gravel$^6$, Philippe Archambault$^2$
\end{center} 

^1^Department of Health and Society, University of Toronto, Toronto, ON, Canada \newline
^2^Québec Océan, Département de biologie, Université Laval, Québec, QC, Canada \newline
^3^Department of Integrative Biology, University Of Guelph, Guelph, Ontario, Canada N1G 2W1 \newline
^4^Bedford Institute of Oceanography, Fisheries and Oceans Canada, Dartmouth, NS, B2Y 4A2 \newline
^5^Marine Affairs Program, Dalhousie University, Halifax, NS, B3H 4R2 \newline
^6^Département de biologie, Université de Sherbrooke, Sherbrooke, QC, Canada \newline

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Supplementary information

## Supplementary tables

<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->
<!-- ### Stressors data -->
<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->
<!--
\begin{singlespace}

{\fontsize{9}{11}\selectfont
\begin{longtable}{ p{0.1\textwidth} p{0.15\textwidth} m{0.09\textwidth} m{0.1\textwidth} m{0.15\textwidth} m{0.14\textwidth} m{0.15\textwidth} }
\caption{List of stressors from Beauchesne et al. (2020)}
\label{t-stressors}
\tabularnewline

\toprule
Groups & Drivers & Spatial resolution & Temporal resolution & Years & Units & Source\\
\midrule
\endfirsthead

\toprule
Groups & Drivers & Spatial resolution & Temporal resolution & Years & Units & Source\\
\midrule
\endhead


Climate & Acidification & Lat/Lon & August-September & 2018 & $\Omega$ $Aragonite$ & Starr and Chassé 2019 \\
Climate & Bottom-water temperature & \textasciitilde{}2 $km^2$ & Monthly & Climatology: 1980-2010; Current: 2013-2017 & negative anomalies & Galbraith et al. 2018\\
Climate & Bottom-water temperature & \textasciitilde{}2 $km^2$ & Monthly & Climatology: 1980-2010; Current: 2013-2017 & positive anomalies & Galbraith et al. 2018\\
Climate & Hypoxia & Lat/Lon & August-September & 2013-2017 & $ml$ $L^{-1}$ & Blais et al. 2019\\
Climate & Surface-water temperature & \textasciitilde{}2 $km^2$ & Monthly & Climatology: 1980-2010; Current: 2013-2017 & negative anomalies & Galbraith et al. 2018\\
Climate & Surface-water temperature & \textasciitilde{}2 $km^2$ & Monthly & Climatology: 1980-2010; Current: 2013-2017 & positive anomalies & Galbraith et al. 2018\\
Coastal & Coastal development & 15 arc-second & Annual & 2015-2016 & $nanoWatts$ $cm^{-2}$ $sr^{-1}$ & Earth observation group 2019\\
Coastal & Direct human impact & Variable: $<1$ to $>40000$ $km^2$ & Annual & 2016 & population count & Statistics Canada 2017\\
Coastal & Inorganic pollution & Modeled 1 $km^2$ & Annual & 2000-2001 & - & Halpern et al. 2015\\
Coastal & Nutrient import & Modeled 1 $km^2$ & Annual & 2007-2010 & $t$ fertilizer & Halpern et al. 2015\\
Coastal & Organic pollution & Modeled 1 $km^2$ & Annual & 2007-2010 & $t$ pesticide & Halpern et al. 2015\\
Fisheries & Demersal, destructive & Lat/Lon & Event based & 2010-2015 & $kg$ & DFO 2016\\
Fisheries & Demersal, non-destructive, high-bycatch & Lat/Lon & Event based & 2010-2015 & $kg$ & DFO 2016\\
Fisheries & Demersal, non-destructive, low-bycatch & Lat/Lon & Event based & 2010-2015 & $kg$ & DFO 2016\\
Fisheries & Pelagic, high-bycatch & Lat/Lon & Event based & 2010-2015 & $kg$ & DFO 2016\\
Fisheries & Pelagic, low-bycatch & Lat/Lon & Event based & 2010-2015 & $kg$ & DFO 2016\\
Marine traffic & Marine pollution & Modeled 1 $km^2$ & Event based \& annual & 2003-2011 \& 2011 & $n$ lanes + $t$ port volume & Halpern et al. 2015\\
Marine traffic & Shipping & 0.1 degree & Event based & 2003-2011 & $n$ lanes & Halpern et al. 2015\\
\bottomrule
\end{longtable}
}
\end{singlespace}


\newpage
-->

<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->
<!-- ### Biotic data -->
<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->

<!--
DFO's annual multidisciplinary research survey is held in August for the Northern Gulf and September for the Southern Gulf [@bourdages2018]. The survey is conducted with a shrimp trawl using a stratified random sampling plan. All fish and invertebrate species caught are identified and measured. Due to the scientific nature of the survey, this is the only dataset from which we considered taxa absences. DFO's Fisheries Logbooks Program was also used to obtain occurrence data on species in the St. Lawrence System [@dfo2016]. Whereas logbooks are not mandatory for all fisheries in the St. Lawrence System, they still provide a thorough overview of the distribution of fishing activities and the presence of species caught. DFO's At-Sea Observers Program stations certified private-sector observers aboard fishing vessels to monitor fishing activities and collect scientific data. DFO's Sentinel Fisheries Program seeks to develop abundance indicators for cod stock assessments in the Northern Gulf by involving the industry in controlled fishing activities. Fixed gear (*i.e.* longlines and gillnets) sampling is conducted on participating fisherman's fishing grounds throughout the fishing season. Mobile gear sampling is conducted using a trawl and a stratified random sampling plan in July. Marine mammals are an important component of the communities of the St. Lawrence System, especially in the Estuary. We included the list of 30 whale and seal species to build the list of taxa and to predict the metaweb (see below). However, we could not consider them for the spatial analyses as occurrence data were inaccessible.
-->

<!-- \begin{singlespace}
{\fontsize{9}{11}\selectfont
\begin{center}
\begin{longtable}{ p{0.18\textwidth} p{0.12\textwidth} p{0.12\textwidth} p{0.1\textwidth} p{0.05\textwidth} p{0.12\textwidth} p{0.06\textwidth} p{0.13\textwidth} }
\caption{Description of the data used to obtain taxa list and occurrences. Except for IUCN data, monitoring programs are all conducted or managed by the Government of Canada's Department of Fisheries and Oceans (DFO).}
\label{t-biotic}
\tabularnewline

 \toprule
 Programs & Description & Sampling & Period & Taxa & Occurrences Absences & Years & Source \\
 \midrule
 \endfirsthead

 \toprule
 Programs & Description & Sampling & Period & Taxa & Occurrences Absences & Years & Source \\
 \midrule
 \endhead
=

 Multidisciplinary Research Survey & Scientific survey & Random stratified & August-September & 293 & 44497 434851 & 2011-2015 & Bourdages et al. 2018 \\
 \addlinespace

 Fisheries Logbooks & Fisheries landings & Fisheries-specific & Fishing season & 48 & 271236 \hspace{1cm} 0 & 2010-2015 & DFO 2016 \\
 \addlinespace

 At-Sea Observers & Fishing observers & Fisheries-specific & Fishing season & 200 & 93230 \hspace{1cm} 0 & 2010-2015 & - \\
 \addlinespace


 Sentinel Fisheries & Scientific fisheries & Fisheries or random stratified & Fishing season or July & 99 & 15990 \hspace{1cm} 0 & 2011-2015 & - \\
 \addlinespace

 Marine Mammals & IUCN Species Red List of Threatened Species & - & - & 30 & 24 & - & IUCN 2020 \\
 \midrule
 Total & - & - & - & 391 & 424953 434851  \\
 \bottomrule
\end{longtable}
\end{center}
}
\end{singlespace}

\newpage -->

<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->
<!-- ### Abiotic data -->
<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->

<!-- \begin{singlespace}
{\fontsize{9}{11}\selectfont
\begin{longtable}{ l l l l }
\caption{List of abiotic descriptors used to extrapolate and map taxa distribution in the St. Lawrence System.}
\label{t-abiotic}
\tabularnewline

 \toprule
 Environmental descriptors & Units & Spatial resolution & Source \\
 \midrule
 \endfirsthead

 \toprule
 Environmental descriptors & Units & Spatial resolution & Source \\
 \midrule
 \endhead

 Depth & $m$ & 10 x 10 km & Dutil et al. 2011 \\
 Latitude & $m$ & - & - \\
 Longitude & $m$ & - & - \\

 \addlinespace
 \textbf{Surface} \\
 pH & - & 5 arcmin & Assis et al. 2018 \\
 Dissolved oxygen & $mol.m^{-3}$ & 5 arcmin & Assis et al. 2018 \\
 Primary productivity & $g.m^{-3}.day^{-1}$ & 5 arcmin & Assis et al. 2018 \\
 Salinity & - & 2.5 x 2.5 km & Dutil et al. 2012 \\
 Temperature & \textit{\textdegree{}C} & \textasciitilde{}2 $km^2$ & Galbraith et al. 2018 \\

 \addlinespace
 \textbf{Bottom} \\
 Aragonite & $\Omega$ $Aragonite$ & Modeled $1 km^2$ & Starr and Chassé 2019\\
 Oxygen saturation & $\%$ & Modeled $1 km^2$ & Blais et al. 2019 \\
 Primary productivity & $g.m^{-3}.day^{-1}$ & 5 arcmin & Assis et al. 2018 \\
 Salinity & - & 10 x 10 km & Dutil et al. 2011 \\
 Temperature & \textit{\textdegree{}C} &  \textasciitilde{}2 $km^2$ & Galbraith et al. 2018 \\

 \bottomrule
 \end{longtable}
 }
 \end{singlespace}


\newpage
 -->


<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->
<!-- ### Taxa list -->
<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->

<!--
# Version 1:
# Not reproducible:
library(magrittr)
library(tidyverse)
load('../ebiotic/biotic/BioticData/Combine_RelevePluriSp_MPO/Data/CombinePluriSP.RData')
load('../ebiotic/biotic/BioticData/ZIF-Fisheries-2010-2015/Data/Biotic/zifSp.RData')
load('../ebiotic/biotic/BioticData/ObsMer_1999-2015_MPO/Data/Biotic/SeaObserverSP.RData')
load('../ebiotic/biotic/BioticData/Peche_sentinelle_2011-2015/Data/Biotic/SentinelleFixedSP.RData')
load('../ebiotic/biotic/BioticData/Peche_sentinelle_2011-2015/Data/Biotic/SentinelleMobileSP.RData')
load('../ebiotic/biotic/BioticData/MarineMammals/Data/Biotic/MarineMammalsSP.RData')
spList$CountRec <- spList$combRec
spList <- spList[, c('species','CountRec')]

spList$db <- 'pluri'
zifSp$db <- 'zif'
obsSp$db <- 'obs'
sentMobSp$db <- 'sentmob'
sentSp$db <- 'sentfix'
mmSp$db <- 'mm'

sp <- rbind(spList, zifSp, obsSp, sentMobSp, sentSp, mmSp) %>%
      spread(db, CountRec) %>%
      mutate(sent = rowSums(.[,c('sentfix','sentmob')], na.rm = T)) %>%
      select(-sentfix, -sentmob) %>%
      mutate(Total = rowSums(.[,c('mm','obs','pluri','zif','sent')], na.rm = T)) %>%
      select(species, pluri, zif, obs, sent, mm, Total) %>%
      rename(Taxa = species,
             'Multidisciplinary Research Survey' = pluri,
             'Fisheries Logbooks' = zif,
             'At-Sea Observers' = obs,
             'Sentinel Fisheries' = sent,
             'Marine Mammals' = mm)

# Replace 0 with '-'
options(knitr.kable.NA = '-')
kableExtra::kable(sp, "latex", booktabs = T)

-->

<!--

# Version 2:
# Not reproducible:
library(magrittr)
library(tidyverse)
library(sf)
load('../ebiotic/biotic/BioticData/SpeciesOccurrence/Data/SpeciesOccurrences.RData')
load('./Data/Biotic/ValidRF.RData')
load('./Data/SpeciesList/SpeciesList.RData')
spList <- sp %>%
          rename(Taxa = species, Occurrences = Count)

sp <- sort(unique(biotic$species))
species <- vector('list', length(sp))
names(species) <- sp
for(i in 1:length(sp)) {
  uid <- biotic$species == sp[i]
  dat <- biotic[uid, 'presence']
  species[[i]] <- data.frame(Occurrences = sum(dat$presence == 1),
                             Absences = sum(dat$presence == 0))
}

validRF[,2:5] <- round(validRF[,2:5], 2)
colnames(validRF) <- c('Taxa','Sensitivity','Specificity','Accuracy','TSS')

obs <- bind_rows(species) %>%
       mutate(Taxa = sp) %>%
       select(Taxa, Absences) %>%
       left_join(validRF, by = 'Taxa') %>%
       left_join(spList, ., by = 'Taxa')


library(raster)
load('/Users/davidbeauchesne/PhDProj/eBiotic/Biotic/BioticDistribution/MarineMammals/Data/MarineMammals.RData')
mm <- names(mm) %>%
      gsub('\\.',' ', .)
uid <- obs$Taxa %in% mm
obs$Occurrences[uid] <- 1
obs[uid,3:7] <- c('-','-','-','-','-')


# Replace 0 with ''
options(knitr.kable.NA = '')
kableExtra::kable(obs, "latex", booktabs = T)
-->



\textbf{Supplementary Table 1 | Taxa list.} List of taxa included in the assessment with occurrences and absences from biotic data, and assessment of sensitibity, specificity, accuracy and True Skilled Statitics (TSS) of species distribution modeling.

\begin{singlespace}
{\fontsize{9}{11}\selectfont
\begin{longtable}{ p{0.25\textwidth} c c c c c c c }
\label{t-taxa}
\tabularnewline

\toprule
Taxa & Occurrences & Absences & Sensitivity & Specificity & Accuracy & TSS\\
\midrule
\endfirsthead

\toprule
Taxa & Occurrences & Absences & Sensitivity & Specificity & Accuracy & TSS\\
\midrule
\endhead

Acipenser oxyrinchus & 7 &  &  &  &  & \\
Actinauge sp. & 253 & 1383 & 0.63 & 0.96 & 0.9 & 0.58\\
Actinostola sp. & 301 & 1335 & 0.56 & 0.95 & 0.88 & 0.52\\
Aega psora & 47 &  &  &  &  & \\
Alcyonidium sp. & 13 &  &  &  &  & \\
\addlinespace
Alepocephalus bairdii & 5 &  &  &  &  & \\
Alosa aestivalis & 1 &  &  &  &  & \\
Alosa pseudoharengus & 155 & 1527 & 0.6 & 0.98 & 0.94 & 0.58\\
Alosa sapidissima & 19 &  &  &  &  & \\
Aluterus schoepfii & 4 &  &  &  &  & \\
\addlinespace
Amblyraja hyperborea & 1 &  &  &  &  & \\
Amblyraja radiata & 6105 & 766 & 0.99 & 0.81 & 0.97 & 0.8\\
Amicula vestita & 3 &  &  &  &  & \\
Ammodytes sp. & 521 & 1280 & 0.42 & 0.89 & 0.76 & 0.32\\
Ampelisca sp. & 3 &  &  &  &  & \\
\addlinespace
Amphiura sp. & 19 &  &  &  &  & \\
Anarhichas denticulatus & 17 &  &  &  &  & \\
Anarhichas lupus & 1149 & 1427 & 0.85 & 0.94 & 0.91 & 0.79\\
Anarhichas minor & 413 & 1588 & 0.64 & 0.98 & 0.93 & 0.62\\
Anguilla rostrata & 16 &  &  &  &  & \\
\addlinespace
Anisarchus medius & 127 & 1514 & 0.57 & 0.98 & 0.95 & 0.55\\
Anomia sp. & 7 &  &  &  &  & \\
Anonyx sp. & 88 & 1548 & 0.07 & 0.99 & 0.94 & 0.06\\
Antalis sp. & 1 &  &  &  &  & \\
Anthoptilum grandiflorum & 292 & 1344 & 0.7 & 0.95 & 0.91 & 0.65\\
\addlinespace
Aphroditella hastata & 129 & 1507 & 0.19 & 0.99 & 0.92 & 0.17\\
Arcoscalpellum michelottianum & 16 &  &  &  &  & \\
Arctica islandica & 20 &  &  &  &  & \\
Arctozenus risso & 1403 & 1056 & 0.95 & 0.92 & 0.93 & 0.86\\
Argentina silus & 42 &  &  &  &  & \\
\addlinespace
Argis dentata & 229 & 1438 & 0.82 & 0.98 & 0.96 & 0.8\\
Argyropelecus aculeatus & 37 &  &  &  &  & \\
Argyropelecus gigas & 21 &  &  &  &  & \\
Aristaeopsis edwardsiana & 2 &  &  &  &  & \\
Arrhoges occidentalis & 104 & 1532 & 0.41 & 0.99 & 0.95 & 0.4\\
\addlinespace
Artediellus sp. & 623 & 1217 & 0.6 & 0.9 & 0.81 & 0.5\\
Ascidiacea & 1139 & 777 & 0.75 & 0.72 & 0.74 & 0.47\\
Aspidophoroides monopterygius & 748 & 1120 & 0.7 & 0.82 & 0.77 & 0.52\\
Aspidophoroides olrikii & 110 & 1527 & 0.31 & 0.98 & 0.94 & 0.29\\
Astarte sp. & 210 & 1426 & 0.23 & 0.97 & 0.88 & 0.2\\
\addlinespace
Asterias sp. & 137 & 1509 & 0.13 & 0.98 & 0.91 & 0.11\\
Astropecten duplicatus & 11 &  &  &  &  & \\
Atlantopandalus propinqvus & 55 & 1583 & 0.15 & 1 & 0.97 & 0.14\\
Atolla wyvillei & 21 &  &  &  &  & \\
Aulacofusus brevicauda & 1 &  &  &  &  & \\
\addlinespace
Aurelia aurita & 143 & 1493 & 0.14 & 0.97 & 0.9 & 0.11\\
Balaena mysticetus & 0 &  &  &  &  & \\
Balaenoptera acutorostrata & 1 & - & - & - & - & -\\
Balaenoptera borealis & 1 & - & - & - & - & -\\
Balaenoptera musculus & 1 & - & - & - & - & -\\
\addlinespace
Balaenoptera physalus & 1 & - & - & - & - & -\\
Balanidae & 24 &  &  &  &  & \\
Balistes vetula & 1 &  &  &  &  & \\
Bathyarca sp. & 1 &  &  &  &  & \\
Bathylagus euryops & 4 &  &  &  &  & \\
\addlinespace
Bathypolypus sp. & 421 & 1392 & 0.56 & 0.92 & 0.83 & 0.48\\
Bathyraja spinicauda & 574 & 1613 & 0.84 & 0.95 & 0.92 & 0.79\\
Benthodesmus elongatus & 1 &  &  &  &  & \\
Beringius turtoni & 2 &  &  &  &  & \\
Bolocera sp. & 392 & 1244 & 0.66 & 0.94 & 0.87 & 0.6\\
\addlinespace
Boltenia ovifera & 72 & 1564 & 0.34 & 0.99 & 0.96 & 0.33\\
Boreogadus saida & 438 & 1534 & 0.75 & 0.94 & 0.9 & 0.69\\
Boreomysis sp. & 1 &  &  &  &  & \\
Boreotrophon sp. & 47 &  &  &  &  & \\
Borostomias antarcticus & 1 &  &  &  &  & \\
\addlinespace
Bothus ocellatus & 4 &  &  &  &  & \\
Brisaster fragilis & 457 & 1203 & 0.87 & 0.95 & 0.93 & 0.82\\
Brosme brosme & 15 &  &  &  &  & \\
Bryozoa & 207 & 1441 & 0.16 & 0.98 & 0.88 & 0.14\\
Buccinum sp. & 11288 & 1108 & 0.98 & 0.84 & 0.97 & 0.83\\
\addlinespace
Caberea ellisii & 18 &  &  &  &  & \\
Calocaris templemani & 19 &  &  &  &  & \\
Cancer borealis & 6 &  &  &  &  & \\
Cancer irroratus & 23823 & 1473 & 1 & 0.94 & 0.99 & 0.94\\
Cardium sp. & 1 &  &  &  &  & \\
\addlinespace
Careproctus reinhardti & 157 & 1579 & 0.65 & 0.98 & 0.95 & 0.64\\
Centroscyllium fabricii & 534 & 1529 & 0.82 & 0.94 & 0.91 & 0.76\\
Ceramaster granularis & 83 & 1553 & 0.28 & 0.99 & 0.96 & 0.27\\
Ceratias holboelli & 2 &  &  &  &  & \\
Cetorhinus maximus & 3 &  &  &  &  & \\
\addlinespace
Chauliodus sloani & 6 &  &  &  &  & \\
Chionoecetes opilio & 68291 & 493 & 1 & 0.46 & 0.99 & 0.46\\
Chiridota laevis & 1 &  &  &  &  & \\
Chirona hameri & 15 &  &  &  &  & \\
Chlamys islandica & 10821 & 1461 & 0.99 & 0.92 & 0.98 & 0.9\\
\addlinespace
Ciliatocardium ciliatum & 99 & 1538 & 0.26 & 0.98 & 0.94 & 0.25\\
Clupea harengus & 15658 & 778 & 0.99 & 0.48 & 0.96 & 0.47\\
Colga villosa & 34 &  &  &  &  & \\
Colus sp. & 84 & 1553 & 0.02 & 0.99 & 0.94 & 0.02\\
Coregonus clupeaformis & 1 &  &  &  &  & \\
\addlinespace
Coryphaena hippurus & 2 &  &  &  &  & \\
Coryphaenoides rupestris & 320 & 0 & 0.94 & 0.91 & 0.93 & 0.86\\
Cottunculus microps & 10 &  &  &  &  & \\
Cottunculus thomsonii & 1 &  &  &  &  & \\
Crangon septemspinosa & 4 &  &  &  &  & \\
\addlinespace
Crenella faba & 3 &  &  &  &  & \\
Crossaster papposus & 697 & 952 & 0.87 & 0.91 & 0.89 & 0.78\\
Cryptacanthodes maculatus & 394 & 1574 & 0.79 & 0.98 & 0.94 & 0.77\\
Cryptopsaras couesii & 3 &  &  &  &  & \\
Ctenodiscus crispatus & 716 & 958 & 0.79 & 0.86 & 0.83 & 0.65\\
\addlinespace
Cucumaria frondosa & 988 & 1609 & 0.97 & 0.99 & 0.98 & 0.96\\
Cuspidaria sp. & 174 & 1462 & 0.49 & 0.98 & 0.93 & 0.47\\
Cyanea capillata & 250 & 1387 & 0.28 & 0.92 & 0.82 & 0.2\\
Cyclocardia borealis & 8 &  &  &  &  & \\
Cyclopterus lumpus & 607 & 1480 & 0.71 & 0.94 & 0.88 & 0.65\\
\addlinespace
Cyrtodaria siliqua & 30 &  &  &  &  & \\
Cystophora cristata & 1 & - & - & - & - & -\\
Delphinapterus leucas & 1 & - & - & - & - & -\\
Delphinus delphis & 1 & - & - & - & - & -\\
Dendronotus sp. & 33 &  &  &  &  & \\
\addlinespace
Diplopteraster multipes & 9 &  &  &  &  & \\
Dipturus laevis & 4 &  &  &  &  & \\
Dipturus linteus & 27 &  &  &  &  & \\
Doridoxa ingolfiana & 10 &  &  &  &  & \\
Drifa glomerata & 65 & 1571 & 0.03 & 0.99 & 0.95 & 0.02\\
\addlinespace
Duva florida & 89 & 1547 & 0.06 & 0.99 & 0.94 & 0.05\\
Echinarachnius parma & 21 &  &  &  &  & \\
Echiodon dawsoni & 1 &  &  &  &  & \\
Enchelyopus cimbrius & 1981 & 952 & 0.94 & 0.89 & 0.92 & 0.83\\
Ensis leei & 128 & 0 & 0.99 & 1 & 1 & 0.99\\
\addlinespace
Epimeria loricata & 32 &  &  &  &  & \\
Epizoanthus erdmanni & 118 & 1518 & 0.08 & 0.99 & 0.92 & 0.06\\
Epizoanthus incrustatus & 1 &  &  &  &  & \\
Erignathus barbatus & 0 &  &  &  &  & \\
Eualus fabricii & 73 & 1563 & 0.34 & 0.99 & 0.96 & 0.33\\
\addlinespace
Eualus gaimardii & 66 & 1572 & 0.2 & 0.99 & 0.96 & 0.19\\
Eualus macilentus & 129 & 1510 & 0.49 & 0.98 & 0.94 & 0.47\\
Eubalaena glacialis & 1 & - & - & - & - & -\\
Eudistoma vitreum & 35 &  &  &  &  & \\
Eumesogrammus praecisus & 355 & 1379 & 0.5 & 0.94 & 0.85 & 0.44\\
\addlinespace
Eumicrotremus derjugini & 1 &  &  &  &  & \\
Eumicrotremus spinosus & 317 & 1423 & 0.48 & 0.95 & 0.87 & 0.43\\
Eusergestes arcticus & 69 & 1571 & 0.68 & 0.99 & 0.98 & 0.67\\
Eusirus cuspidatus & 35 &  &  &  &  & \\
Flabellum sp. & 29 &  &  &  &  & \\
\addlinespace
Gadus morhua & 22539 & 500 & 0.99 & 0.32 & 0.98 & 0.31\\
Gadus ogac & 618 & 1544 & 0.83 & 0.98 & 0.93 & 0.8\\
Gaidropsarus sp. & 103 & 1606 & 0.57 & 0.99 & 0.97 & 0.56\\
Gasterosteus aculeatus aculeatus & 115 & 1548 & 0.16 & 0.98 & 0.93 & 0.14\\
Gersemia rubiformis & 436 & 1200 & 0.54 & 0.87 & 0.78 & 0.4\\
\addlinespace
Globicephala melas & 1 & - & - & - & - & -\\
Glyptocephalus cynoglossus & 8860 & 779 & 0.99 & 0.84 & 0.98 & 0.83\\
Gonatus fabricii & 1 &  &  &  &  & \\
Gonostomatidae & 59 & 1593 & 0.08 & 0.99 & 0.96 & 0.08\\
Gorgonocephalus sp. & 622 & 1091 & 0.71 & 0.89 & 0.82 & 0.59\\
\addlinespace
Grampus griseus & 1 & - & - & - & - & -\\
Gymnelus viridis & 67 & 1569 & 0.22 & 0.99 & 0.97 & 0.22\\
Gymnocanthus tricuspis & 374 & 1268 & 0.48 & 0.91 & 0.82 & 0.4\\
Halichoerus grypus & 1 & - & - & - & - & -\\
Halichondria panicea & 78 & 1558 & 0.1 & 0.99 & 0.95 & 0.09\\
\addlinespace
Halipteris finmarchica & 92 & 1544 & 0.39 & 0.99 & 0.96 & 0.38\\
Hamingia arctica & 2 &  &  &  &  & \\
Helicolenus dactylopterus & 7 &  &  &  &  & \\
Heliometra glacialis & 7 &  &  &  &  & \\
Hemithiris psittacea & 30 &  &  &  &  & \\
\addlinespace
Hemitripterus americanus & 461 & 1521 & 0.63 & 0.97 & 0.9 & 0.6\\
Henricia sp. & 627 & 1012 & 0.57 & 0.79 & 0.71 & 0.36\\
Hiatella arctica & 23 &  &  &  &  & \\
Hippasteria phrygiana & 458 & 1380 & 0.59 & 0.91 & 0.83 & 0.51\\
Hippoglossina oblonga & 2 &  &  &  &  & \\
\addlinespace
Hippoglossoides platessoides & 18677 & 248 & 1 & 0.33 & 0.99 & 0.33\\
Hippoglossus hippoglossus & 21086 & 1267 & 0.99 & 0.5 & 0.96 & 0.48\\
Homarus americanus & 10671 & 1499 & 0.99 & 0.97 & 0.99 & 0.96\\
Hormathia nodosa & 59 & 1585 & 0.07 & 1 & 0.97 & 0.07\\
Howella sherborni & 2 &  &  &  &  & \\
\addlinespace
Hyas araneus & 1688 & 1250 & 0.91 & 0.91 & 0.91 & 0.81\\
Hyas coarctatus & 798 & 934 & 0.77 & 0.82 & 0.8 & 0.6\\
Hymenopenaeus debilis & 4 &  &  &  &  & \\
Hyperia galba & 30 &  &  &  &  & \\
Hyperoodon ampullatus & 1 & - & - & - & - & -\\
\addlinespace
Icelus sp. & 208 & 1437 & 0.29 & 0.96 & 0.88 & 0.25\\
Illex illecebrosus & 360 & 1419 & 0.39 & 0.92 & 0.82 & 0.31\\
Isurus oxyrinchus & 19 &  &  &  &  & \\
Kajikia albida & 1 &  &  &  &  & \\
Kogia breviceps & 1 & - & - & - & - & -\\
\addlinespace
Lagenorhynchus acutus & 1 & - & - & - & - & -\\
Lagenorhynchus albirostris & 1 & - & - & - & - & -\\
Lamna nasus & 34 &  &  &  &  & \\
Lampanyctus sp. & 17 &  &  &  &  & \\
Larus sp. & 3 &  &  &  &  & \\
\addlinespace
Lebbeus groenlandicus & 58 & 1578 & 0.67 & 1 & 0.99 & 0.66\\
Lebbeus microceros & 11 &  &  &  &  & \\
Lebbeus polaris & 243 & 1393 & 0.54 & 0.96 & 0.9 & 0.49\\
Leptagonus decagonus & 673 & 1369 & 0.76 & 0.93 & 0.87 & 0.69\\
Leptasterias sp. & 410 & 1236 & 0.6 & 0.9 & 0.83 & 0.51\\
\addlinespace
Leptoclinus maculatus & 869 & 1025 & 0.79 & 0.82 & 0.8 & 0.6\\
Leptychaster arcticus & 8 &  &  &  &  & \\
Leucoraja erinacea & 37 &  &  &  &  & \\
Leucoraja ocellata & 331 & 1623 & 0.78 & 0.97 & 0.94 & 0.75\\
Limanda ferruginea & 7434 & 1237 & 0.98 & 0.94 & 0.98 & 0.92\\
\addlinespace
Liparis atlanticus & 2 &  &  &  &  & \\
Liparis fabricii & 28 &  &  &  &  & \\
Liparis gibbus & 268 & 1405 & 0.41 & 0.95 & 0.86 & 0.36\\
Liparis liparis liparis & 1 &  &  &  &  & \\
Liponema multicorne & 27 &  &  &  &  & \\
\addlinespace
Lithodes maja & 2592 & 1289 & 0.96 & 0.83 & 0.92 & 0.79\\
Lophius americanus & 1550 & 1591 & 0.89 & 0.86 & 0.87 & 0.75\\
Lumpenus fabricii & 6 &  &  &  &  & \\
Lumpenus lampretaeformis & 645 & 1413 & 0.83 & 0.94 & 0.91 & 0.78\\
Lycenchelys paxillus & 129 & 1626 & 0.7 & 0.99 & 0.97 & 0.69\\
\addlinespace
Lycenchelys verrillii & 69 & 1585 & 0.17 & 1 & 0.96 & 0.17\\
Lycodes lavalaei & 345 & 1343 & 0.54 & 0.93 & 0.85 & 0.48\\
Lycodes pallidus & 495 & 0 & 0.99 & 0.95 & 0.97 & 0.93\\
Lycodes polaris & 20 &  &  &  &  & \\
Lycodes reticulatus & 31 &  &  &  &  & \\
\addlinespace
Lycodes terraenovae & 129 & 1594 & 0.51 & 0.98 & 0.94 & 0.49\\
Lycodes turneri & 2 &  &  &  &  & \\
Lycodes vahlii & 552 & 1380 & 0.81 & 0.95 & 0.91 & 0.76\\
Macoma sp. & 18 &  &  &  &  & \\
Macrourus berglax & 39 &  &  &  &  & \\
\addlinespace
Mactromeris polynyma & 959 & 1632 & 0.98 & 1 & 0.99 & 0.98\\
Maera loveni & 3 &  &  &  &  & \\
Malacocephalus occidentalis & 1 &  &  &  &  & \\
Malacoraja senta & 1765 & 1000 & 0.95 & 0.87 & 0.92 & 0.82\\
Malacosteus niger & 1 &  &  &  &  & \\
\addlinespace
Mallotus villosus & 7766 & 602 & 0.98 & 0.63 & 0.95 & 0.61\\
Margarites sp. & 125 & 1515 & 0.12 & 0.99 & 0.93 & 0.11\\
Maurolicus muelleri & 1 &  &  &  &  & \\
Megaptera novaeangliae & 1 & - & - & - & - & -\\
Megayoldia thraciaeformis & 92 & 1544 & 0.35 & 0.98 & 0.95 & 0.33\\
\addlinespace
Melanogrammus aeglefinus & 322 & 1616 & 0.78 & 0.98 & 0.95 & 0.76\\
Melanostigma atlanticum & 1014 & 1289 & 0.93 & 0.94 & 0.93 & 0.87\\
Melita dentata & 2 &  &  &  &  & \\
Menidia menidia & 2 &  &  &  &  & \\
Mentodus rostratus & 1 &  &  &  &  & \\
\addlinespace
Mercenaria mercenaria & 12 &  &  &  &  & \\
Merluccius bilinearis & 1581 & 1286 & 0.89 & 0.83 & 0.86 & 0.71\\
Mesodesma sp. & 19 &  &  &  &  & \\
Metridium senile & 2 &  &  &  &  & \\
Microgadus tomcod & 64 & 0 & 0.95 & 0.94 & 0.94 & 0.89\\
\addlinespace
Micromesistius poutassou & 18 &  &  &  &  & \\
Modiolus modiolus & 5 &  &  &  &  & \\
Molpadia sp. & 10 &  &  &  &  & \\
Molva dypterygia & 1 &  &  &  &  & \\
Monodon monoceros & 0 &  &  &  &  & \\
\addlinespace
Morone saxatilis & 3 &  &  &  &  & \\
Morus bassanus & 5 &  &  &  &  & \\
Mugil cephalus & 1 &  &  &  &  & \\
Munida valida & 1 &  &  &  &  & \\
Munidopsis curvirostra & 122 & 1515 & 0.58 & 0.98 & 0.95 & 0.57\\
\addlinespace
Musculus sp. & 20 &  &  &  &  & \\
Mustelus canis & 1 &  &  &  &  & \\
Mya arenaria & 2 &  &  &  &  & \\
Mya truncata & 9 &  &  &  &  & \\
Myctophidae & 270 & 1517 & 0.66 & 0.96 & 0.92 & 0.62\\
\addlinespace
Myoxocephalus sp. & 1681 & 1239 & 0.87 & 0.88 & 0.88 & 0.75\\
Mytilus sp. & 79 & 1560 & 0.18 & 0.99 & 0.95 & 0.17\\
Myxine glutinosa & 3412 & 1066 & 0.98 & 0.91 & 0.97 & 0.89\\
Naticidae & 273 & 1523 & 0.58 & 0.99 & 0.93 & 0.57\\
Naucrates ductor & 1 &  &  &  &  & \\
\addlinespace
Nealotus tripes & 1 &  &  &  &  & \\
Nemichthys scolopaceus & 21 &  &  &  &  & \\
Neohela monstrosa & 19 &  &  &  &  & \\
Neolithodes grimaldii & 20 &  &  &  &  & \\
Neoscopelus macrolepidotus & 9 &  &  &  &  & \\
\addlinespace
Neptunea sp. & 114 & 1522 & 0.04 & 0.99 & 0.92 & 0.02\\
Nezumia bairdii & 1838 & 1153 & 0.97 & 0.93 & 0.95 & 0.9\\
Novodinia americana & 1 &  &  &  &  & \\
Nucella lapillus & 1 &  &  &  &  & \\
Nuculana sp. & 39 &  &  &  &  & \\
\addlinespace
Nymphon sp. & 50 & 1586 & 0 & 1 & 0.97 & 0\\
Oceanites sp. & 1 &  &  &  &  & \\
Odobenus rosmarus & 0 &  &  &  &  & \\
Oediceros saginatus & 1 &  &  &  &  & \\
Ommastrephes sp. & 1 &  &  &  &  & \\
\addlinespace
Omosudis lowii & 1 &  &  &  &  & \\
Ophiacantha bidentata & 113 & 1523 & 0.19 & 0.98 & 0.93 & 0.17\\
Ophiopholis aculeata & 187 & 1454 & 0.38 & 0.96 & 0.89 & 0.34\\
Ophioscolex glacialis & 18 &  &  &  &  & \\
Ophiura sp. & 276 & 1407 & 0.55 & 0.95 & 0.89 & 0.5\\
\addlinespace
Orcinus orca & 1 & - & - & - & - & -\\
Osmerus mordax mordax & 148 & 1536 & 0.72 & 0.99 & 0.97 & 0.71\\
Pagophilus groenlandicus & 1 & - & - & - & - & -\\
Pagurus sp. & 512 & 1152 & 0.7 & 0.9 & 0.84 & 0.6\\
Palio dubia & 1 &  &  &  &  & \\
\addlinespace
Pandalus borealis & 48179 & 895 & 1 & 0.88 & 1 & 0.88\\
Pandalus montagui & 468 & 1193 & 0.77 & 0.95 & 0.9 & 0.71\\
Panomya norvegica & 3 &  &  &  &  & \\
Paralichthys dentatus & 1 &  &  &  &  & \\
Paraliparis calidus & 33 &  &  &  &  & \\
\addlinespace
Paraliparis copei copei & 29 &  &  &  &  & \\
Paramphithoe hystrix & 26 &  &  &  &  & \\
Parvicardium pinnulatum & 1 &  &  &  &  & \\
Pasiphaea multidentata & 1282 & 1136 & 0.94 & 0.91 & 0.92 & 0.85\\
Pasiphaea tarda & 6 &  &  &  &  & \\
\addlinespace
Pennatula aculeata & 412 & 1224 & 0.7 & 0.91 & 0.86 & 0.61\\
Pennatula grandis & 504 & 1437 & 0.76 & 0.93 & 0.89 & 0.69\\
Peprilus triacanthus & 13 &  &  &  &  & \\
Periphylla periphylla & 187 & 1449 & 0.21 & 0.95 & 0.87 & 0.16\\
Petromyzon marinus & 11 &  &  &  &  & \\
\addlinespace
Phascolion strombus strombus & 9 &  &  &  &  & \\
Phoca vitulina & 1 & - & - & - & - & -\\
Phocoena phocoena & 1 & - & - & - & - & -\\
Pholis gunnellus & 1 &  &  &  &  & \\
Phycis chesteri & 399 & 1439 & 0.73 & 0.96 & 0.91 & 0.69\\
\addlinespace
Physeter macrocephalus & 1 & - & - & - & - & -\\
Plesionika martia & 3 &  &  &  &  & \\
Pleurobrachia pileus & 94 & 1542 & 0.11 & 0.98 & 0.93 & 0.09\\
Pollachius virens & 250 & 1627 & 0.73 & 0.98 & 0.95 & 0.71\\
Polyipnus clarus & 45 &  &  &  &  & \\
\addlinespace
Polynoidae & 87 & 1551 & 0.02 & 0.99 & 0.94 & 0.02\\
Pomatomus saltatrix & 1 &  &  &  &  & \\
Pontophilus norvegicus & 334 & 1304 & 0.7 & 0.95 & 0.9 & 0.65\\
Poraniomorpha sp. & 22 &  &  &  &  & \\
Porifera & 1172 & 694 & 0.77 & 0.6 & 0.7 & 0.37\\
\addlinespace
Prionace glauca & 12 &  &  &  &  & \\
Pseudarchaster parelii & 60 & 1576 & 0 & 0.99 & 0.96 & -0.01\\
Pseudopleuronectes americanus & 10889 & 1422 & 0.99 & 0.95 & 0.98 & 0.94\\
Psilaster andromeda & 79 & 1557 & 0.51 & 0.99 & 0.97 & 0.5\\
Psolus fabricii & 135 & 1501 & 0.3 & 0.97 & 0.92 & 0.27\\
\addlinespace
Psolus phantapus & 54 & 1582 & 0.11 & 1 & 0.97 & 0.11\\
Pteraster militaris & 186 & 1450 & 0.17 & 0.97 & 0.88 & 0.14\\
Pteraster obscurus & 3 &  &  &  &  & \\
Pteraster pulvillus & 65 & 1571 & 0.33 & 0.99 & 0.96 & 0.32\\
Ptychogena lactea & 189 & 1447 & 0.17 & 0.95 & 0.86 & 0.13\\
\addlinespace
Pusa hispida & 0 &  &  &  &  & \\
Pycnogonum litorale & 8 &  &  &  &  & \\
Rajella fyllae & 21 &  &  &  &  & \\
Reinhardtius hippoglossoides & 27772 & 765 & 1 & 0.8 & 0.99 & 0.79\\
Remora remora & 1 &  &  &  &  & \\
\addlinespace
Reteporella grimaldii & 1 &  &  &  &  & \\
Rhachotropis aculeata & 93 & 1543 & 0.32 & 0.99 & 0.95 & 0.3\\
Rossia sp. & 341 & 1388 & 0.4 & 0.91 & 0.81 & 0.32\\
Sabinea sarsii & 58 & 1578 & 0.38 & 0.99 & 0.97 & 0.37\\
Sabinea septemcarinata & 136 & 1500 & 0.69 & 0.98 & 0.96 & 0.67\\
\addlinespace
Salmo salar & 19 &  &  &  &  & \\
Scaphander punctostriatus & 106 & 1530 & 0.22 & 0.98 & 0.93 & 0.19\\
Sclerocrangon boreas & 95 & 1570 & 0.6 & 0.99 & 0.97 & 0.59\\
Scomber scombrus & 9523 & 1436 & 0.97 & 0.89 & 0.96 & 0.86\\
Scomberesox saurus saurus & 6 &  &  &  &  & \\
\addlinespace
Scophthalmus aquosus & 5612 & 1603 & 0.99 & 0.99 & 0.99 & 0.99\\
Sebastes sp. & 15084 & 614 & 0.99 & 0.78 & 0.98 & 0.77\\
Securiflustra securifrons & 37 &  &  &  &  & \\
Sepioloidea sp. & 2 &  &  &  &  & \\
Sergia robusta & 8 &  &  &  &  & \\
\addlinespace
Serripes groenlandicus & 18 &  &  &  &  & \\
Serrivomer beanii & 10 &  &  &  &  & \\
Similipecten greenlandicus & 29 &  &  &  &  & \\
Solaster endeca & 254 & 1412 & 0.3 & 0.96 & 0.85 & 0.25\\
Somniosus microcephalus & 2 &  &  &  &  & \\
\addlinespace
Spirontocaris liljeborgii & 144 & 1492 & 0.19 & 0.98 & 0.91 & 0.17\\
Spirontocaris phippsii & 13 &  &  &  &  & \\
Spirontocaris spinus & 119 & 1517 & 0.62 & 0.99 & 0.96 & 0.61\\
Spisula solidissima & 315 & 1634 & 0.99 & 1 & 1 & 0.99\\
Squalus acanthias & 149 & 1634 & 0.47 & 0.98 & 0.94 & 0.46\\
\addlinespace
Staurostoma mertensii & 59 & 1608 & 0.15 & 0.99 & 0.96 & 0.15\\
Stauroteuthis syrtensis & 2 &  &  &  &  & \\
Stegocephalus inflatus & 110 & 1526 & 0.15 & 0.98 & 0.93 & 0.13\\
Stenella coeruleoalba & 1 & - & - & - & - & -\\
Stenella frontalis & 0 &  &  &  &  & \\
\addlinespace
Stephanasterias albula & 13 &  &  &  &  & \\
Stephanauge sp. & 75 & 1561 & 0.24 & 0.99 & 0.96 & 0.23\\
Stichaeus punctatus punctatus & 14 &  &  &  &  & \\
Stomias boa ferox & 7 &  &  &  &  & \\
Stomphia coccinea & 179 & 1459 & 0.41 & 0.96 & 0.9 & 0.37\\
\addlinespace
Strongylocentrotus sp. & 2929 & 841 & 0.9 & 0.84 & 0.87 & 0.73\\
Stylocordyla borealis & 75 & 1561 & 0.05 & 0.99 & 0.95 & 0.04\\
Symbolophorus veranyi & 20 &  &  &  &  & \\
Synaphobranchus kaupii & 3 &  &  &  &  & \\
Syscenus infelix & 264 & 1377 & 0.68 & 0.95 & 0.9 & 0.62\\
\addlinespace
Tachyrhynchus erosus & 2 &  &  &  &  & \\
Tautogolabrus adspersus & 36 &  &  &  &  & \\
Tellina sp. & 1 &  &  &  &  & \\
Terebratulina septentrionalis & 61 & 1575 & 0.11 & 0.99 & 0.96 & 0.11\\
Teredo navalis & 3 &  &  &  &  & \\
\addlinespace
Thuiaria thuja & 43 &  &  &  &  & \\
Thunnus alalunga & 1 &  &  &  &  & \\
Thunnus obesus & 1 &  &  &  &  & \\
Thunnus thynnus & 3941 & 0 & 0.99 & 0.99 & 0.99 & 0.98\\
Thysanoessa longicaudata & 2 &  &  &  &  & \\
\addlinespace
Tonicella sp. & 6 &  &  &  &  & \\
Trachyrincus murrayi & 7 &  &  &  &  & \\
Tremaster mirabilis & 1 &  &  &  &  & \\
Triglops sp. & 420 & 1266 & 0.54 & 0.93 & 0.84 & 0.47\\
Trigonolampa miriceps & 2 &  &  &  &  & \\
\addlinespace
Tritia sp. & 2 &  &  &  &  & \\
Tursiops truncatus & 1 & - & - & - & - & -\\
Ulvaria subbifurcata & 20 &  &  &  &  & \\
Urasterias lincki & 1 &  &  &  &  & \\
Urophycis chuss & 7 &  &  &  &  & \\
\addlinespace
Urophycis regia & 1 &  &  &  &  & \\
Urophycis tenuis & 6242 & 1120 & 0.97 & 0.73 & 0.93 & 0.7\\
Urticina felina & 89 & 1634 & 0.7 & 0.99 & 0.97 & 0.69\\
Velutinidae & 8 &  &  &  &  & \\
Wimvadocus torelli & 2 &  &  &  &  & \\
\addlinespace
Xenodermichthys copei & 14 &  &  &  &  & \\
Xiphias gladius & 1 &  &  &  &  & \\
Xylophaga atlantica & 3 &  &  &  &  & \\
Yoldia sp. & 7 &  &  &  &  & \\
Ziphius cavirostris & 1 & - & - & - & - & -\\
\addlinespace
Zoarces americanus & 62 & 1615 & 0.37 & 0.99 & 0.97 & 0.36\\
\bottomrule

\end{longtable}
}
\end{singlespace}


\newpage

<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->
<!-- ### Traits -->
<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->

\textbf{Supplementary Table 2 | Traits.} List of traits used to characterize the relative species-specific sensitivity of taxa to stressors and description of each trait category.

\begin{singlespace}
{\fontsize{9}{11}\selectfont
\begin{longtable}{ p{0.15\textwidth} p{0.4\textwidth} p{0.45\textwidth} }
\label{t-traits}
\tabularnewline
 \toprule
 Traits & Categories & Description \\
 \midrule
 \endfirsthead

 \toprule
 Traits & Categories & Description \\
 \midrule
 \endhead

% =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
Body composition & Biogenic silica & Organism with skeleton formed by microscopic particles of silica \\
& Bone & Organism with solid skeleton made of bones \\
& Cartilaginous & Organism with solid skeleton made of cartilage \\
& Chitinous & Organism with solid skeleton made of chitin \\
& Non-calcifying & Organism devoid of calcifying skeleton \\
& Soft-bodied, aragonite & Organism with aragonite in soft tissues \\
& Soft-bodied, calcite & Organism with calcite in soft tissues \\
& Soft-bodied, calcium phosphate & Organism with calcium phosphate in soft tissues \\
& Soft-bodied, calcium sulfate & Organism with calcium sulfate in soft tissues \\
& Skeleton, aragonite & Organism with solid skeleton made of or containing aragonite \\
& Skeleton, calcite & Organism with solid skeleton made of or containing calcite \\
& Skeleton, calcium phosphate & Organism with solid skeleton made of or containing calcium phosphate \\
& Skeleton, high-magnesium calcite & Organism with solid skeleton made of or containing high-magnesium calcite \\
& Skeleton, phosphatic & Organism with solid skeleton made of or containing phosphates \\
\midrule

% =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
Environment & Bathydemersal & Living and/or feeding on or near the bottom, below 200 m \\
& Bathypelagic & Occurring mainly in open water below 200 m, not feeding on benthic organisms \\
& Benthic & Living and feeding on the bottom \\
& Benthopelagic & Living and/or feeding on or near the bottom, as well as in midwater, between 0 and 200 m \\
& Demersal & Living and/or feeding on or near the bottom, between 0 and 200 m \\
& Pelagic & Occurring mainly in the water column between 0 and 200 m, not feeding on benthic organisms \\
\midrule

% =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
Feeding type & Deposit feeder & Organism that lives on or in the sediments and consumes organic material on the sea floor \\
& Selective filter feeder & Organism that actively and selectively filters waters to consume plankton or nutrients suspended in the water \\
& Grazer & Organism that feeds on plants \\
& Parasite & Organism that lives in or on another organism, benefiting at the other organism's expense \\
& Plankton & Organism feeding exclusively on plankton \\
& Predator & Organism that actively hunts prey \\
& Scavenger & Organism that feeds on dead plant or animal material, or refuse \\
& Suspension feeder & Organism that captures and consumes particules suspended in the water, such as plankton, bacteria, detritus and particulate organic matter \\
& Xylophagous & Organism feeding on or boring into wood \\
\midrule

% =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
Fisheries landings & Targeted species & Applicable to fisheries only. Taxa targeted by fishing activities. \\
& Bycatch & Applicable to fisheries only. Taxa bycatch by fishing activities. \\
& Others & Applicable to fisheries only. Taxa not targeted or caught by fishing activities \\
\midrule

% =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
Mobility & Burrower & Organism that lives in a burrow dug in the sediments \\
& Crawler & Organism that crawls slowly on the bottom\\
& Mobile & Free-ranging organism \\
& Sessile & Immobile or fixed organism \\
& Swimmer & Organism with limited swimming ability \\
\midrule

% =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
Phylum & Annelida & - \\
& Arthropoda & - \\
& Brachiopoda & - \\
& Bryozoa & - \\
& Chordata & - \\
& Cnidaria & - \\
& Ctenophora & - \\
& Echinodermata & - \\
& Echiura & - \\
& Mollusca & - \\
& Porifera & - \\
& Sipuncula & - \\
\midrule

% =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
Size & 0-100 cm & Body length between 0 and 100 cm \\
& 100-200 cm & Body length between 100 and 200 cm \\
& 200-300 cm & Body length between 200 and 300 cm \\
& 300+ cm & Body length greater than 300 cm \\ 

\bottomrule
\end{longtable}
}
\end{singlespace}



\newpage
-->

<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->
<!-- ### Species-specific sensitivity -->
<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->

\textbf{Supplementary Table 3 | Species-specific sensitivity.} Trait matching rules between stressors and traits to assess the relative species-specific sensitivity scores for each stressor.

\begin{singlespace}
{\fontsize{9}{11}\selectfont
\begin{center}
\begin{longtable}{ p{0.3\textwidth} p{0.15\textwidth} l c }
\label{t-vulnerability}
\tabularnewline

 \toprule
 Stressors & Traits & Category & Sensitivity \\
 \midrule
 \endfirsthead

 \toprule
 Stressors & Traits & Category & Sensitivity \\
 \midrule
 \endhead

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Ocean acidification & Environment & Bathydemersal & 1.00 \\
 & & Bathypelagic & 0.00 \\
 & & Benthic & 1.00 \\
 & & Benthopelagic & 0.50 \\
 & & Demersal & 1.00 \\
 & & Pelagic & 0.00 \\
 \cline{2-4}
 & Mobility & Burrower & 0.75 \\
 & & Crawler & 0.75 \\
 & & Mobile & 0.25 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \cline{2-4}
 & Body & Biogenic silica & 0.00 \\
 & composition & Bone & 0.00 \\
 & & Cartilaginous & 0.00 \\
 & & Chitinous & 0.00 \\
 & & Non-calcifying & 0.00 \\
 & & Soft-bodied, aragonite & 0.00 \\
 & & Soft-bodied, calcite & 0.00 \\
 & & Soft-bodied, calcium phosphate & 0.00 \\
 & & Soft-bodied, calcium sulfate & 0.00 \\
 & & Skeleton, aragonite & 0.90 \\
 & & Skeleton, calcite & 0.80 \\
 & & Skeleton, calcium phosphate & 0.80 \\
 & & Skeleton, high-magnesium calcite & 1.00 \\
 & & Skeleton, phosphatic & 0.00 \\
 \cline{2-4}
 & Phylum & Annelida & 0.00 \\
 & & Arthropoda & 0.50 \\
 & & Brachiopoda & 0.50 \\
 & & Bryozoa & 1.00 \\
 & & Chordata & 0.00 \\
 & & Cnidaria & 1.00 \\
 & & Ctenophora & 0.00 \\
 & & Echinodermata & 1.00 \\
 & & Echiura & 0.00 \\
 & & Mollusca & 1.00 \\
 & & Porifera & 0.00 \\
 & & Sipuncula & 0.00 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Bottom-water & Environment & Bathydemersal & 1.00 \\
 temperature & & Bathypelagic & 0.00 \\
 anomalies & & Benthic & 1.00 \\
 & & Benthopelagic & 0.50 \\
 & & Demersal & 1.00 \\
 & & Pelagic & 0.00 \\
 \cline{2-4}
 & Mobility & Burrower & 0.75 \\
 & & Crawler & 0.75 \\
 & & Mobile & 0.25 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \cline{2-4}
 & Feeding type & Deposit feeder & 1.00 \\
 & & Selective filter feeder & 0.00 \\
 & & Grazer & 1.00 \\
 & & Parasite & 0.00 \\
 & & Plankton & 0.50 \\
 & & Predator & 0.50 \\
 & & Scavenger & 0.50 \\
 & & Suspension feeder & 1.00 \\
 & & Xylophagous & 0.50 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Surface-water & Environment & Bathydemersal & 0.00 \\
 temperature & & Bathypelagic & 0.00 \\
 anomalies & & Benthic & 0.00 \\
 & & Benthopelagic & 0.50 \\
 & & Demersal & 0.00 \\
 & & Pelagic & 1.00 \\
 \cline{2-4}
 & Mobility & Burrower & 0.75 \\
 & & Crawler & 0.75 \\
 & & Mobile & 0.25 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \cline{2-4}
 & Feeding type & Deposit feeder & 1.00 \\
 & & Selective filter feeder & 0.00 \\
 & & Grazer & 1.00 \\
 & & Parasite & 0.00 \\
 & & Plankton & 0.50 \\
 & & Predator & 0.50 \\
 & & Scavenger & 0.50 \\
 & & Suspension feeder & 1.00 \\
 & & Xylophagous & 0.50 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Hypoxia & Environment & Bathydemersal & 1.00 \\
 & & Bathypelagic & 0.00 \\
 & & Benthic & 1.00 \\
 & & Benthopelagic & 0.50 \\
 & & Demersal & 1.00 \\
 & & Pelagic & 0.00 \\
 \cline{2-4}
 & Mobility & Burrower & 0.75 \\
 & & Crawler & 0.75 \\
 & & Mobile & 0.25 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \cline{2-4}
 & Feeding type & Deposit feeder & 1.00 \\
 & & Selective filter feeder & 0.00 \\
 & & Grazer & 1.00 \\
 & & Parasite & 0.00 \\
 & & Plankton & 0.50 \\
 & & Predator & 0.50 \\
 & & Scavenger & 0.50 \\
 & & Suspension feeder & 1.00 \\
 & & Xylophagous & 0.50 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Coastal & Mobility & Burrower & 0.75 \\
 development & & Crawler & 0.75 \\
 & & Mobile & 0.50 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \cline{2-4}
 & Size & 0-100 cm & 0.25 \\
 & & 100-200 cm & 0.50 \\
 & & 200-300 cm & 0.75 \\
 & & 300+ cm & 1.00 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Direct human & Mobility & Burrower & 0.75 \\
 impact & & Crawler & 0.75 \\
 & & Mobile & 0.50 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \cline{2-4}
 & Size & 0-100 cm & 0.25 \\
 & & 100-200 cm & 0.50 \\
 & & 200-300 cm & 0.75 \\
 & & 300+ cm & 1.00 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Inorganic & Feeding type & Deposit feeder & 0.75 \\
 pollution & & Selective filter feeder & 0.00 \\
 & & Grazer & 0.00 \\
 & & Parasite & 0.00 \\
 & & Plankton & 0.00 \\
 & & Predator & 0.00 \\
 & & Scavenger & 0.50 \\
 & & Suspension feeder & 1.00 \\
 & & Xylophagous & 0.00 \\
 \cline{2-4}
 & Mobility & Burrower & 0.75 \\
 & & Crawler & 0.75 \\
 & & Mobile & 0.25 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Nutrient & Feeding type & Deposit feeder & 0.75 \\
 input & & Selective filter feeder & 0.00 \\
 & & Grazer & 0.00 \\
 & & Parasite & 0.00 \\
 & & Plankton & 0.00 \\
 & & Predator & 0.00 \\
 & & Scavenger & 0.50 \\
 & & Suspension feeder & 1.00 \\
 & & Xylophagous & 0.00 \\
 \cline{2-4}
 & Mobility & Burrower & 0.75 \\
 & & Crawler & 0.75 \\
 & & Mobile & 0.25 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Organic & Feeding type & Deposit feeder & 0.75 \\
 pollution & & Selective filter feeder & 0.00 \\
 & & Grazer & 0.00 \\
 & & Parasite & 0.00 \\
 & & Plankton & 0.00 \\
 & & Predator & 0.00 \\
 & & Scavenger & 0.50 \\
 & & Suspension feeder & 1.00 \\
 & & Xylophagous & 0.00 \\
 \cline{2-4}
 & Mobility & Burrower & 0.75 \\
 & & Crawler & 0.75 \\
 & & Mobile & 0.25 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Demersal destructive & Landings & Targeted species & 1.00 \\
 fisheries & & Bycatch & 0.75 \\
 & & Others & 0.25 \\
 \cline{2-4}
 For \textit{Others} & Environment & Bathydemersal & 1.00 \\
 only& & Bathypelagic & 0.00 \\
 & & Benthic & 1.00 \\
 & & Benthopelagic & 0.50 \\
 & & Demersal & 1.00 \\
 & & Pelagic & 0.00 \\
 \cline{2-4}
 & Mobility & Burrower & 0.75 \\
 & & Crawler & 0.75 \\
 & & Mobile & 0.25 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\

 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Demersal non-destructive & Landings & Targeted species & 1.00 \\
 high-bycatch fisheries & & Bycatch & 0.75 \\
 & & Others & 0.00 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Demersal non-destructive & Landings & Targeted species & 1.00 \\
 low-bycatch fisheries & & Bycatch & 0.75 \\
 & & Others & 0.00 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Pelagic high-bycatch & Landings & Targeted species & 1.00 \\
 fisheries & & Bycatch & 0.75 \\
 & & Others & 0.00 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Pelagic low-bycatch & Landings & Targeted species & 1.00 \\
 fisheries & & Bycatch & 0.75 \\
 & & Others & 0.00 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Marine & Feeding type & Deposit feeder & 0.75 \\
 pollution & & Selective filter feeder & 0.00 \\
 & & Grazer & 0.00 \\
 & & Parasite & 0.00 \\
 & & Plankton & 0.00 \\
 & & Predator & 0.00 \\
 & & Scavenger & 0.50 \\
 & & Suspension feeder & 1.00 \\
 & & Xylophagous & 0.00 \\
 \cline{2-4}
 & Mobility & Burrower & 0.75 \\
 & & Crawler & 0.75 \\
 & & Mobile & 0.25 \\
 & & Sessile & 1.00 \\
 & & Swimmer & 0.75 \\
 \midrule

 % =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
 Shipping & Environment & Bathydemersal & 0.00 \\
 & & Bathypelagic & 0.00 \\
 & & Benthic & 0.00 \\
 & & Benthopelagic & 0.50 \\
 & & Demersal & 0.00 \\
 & & Pelagic & 1.00 \\
 \cline{2-4}
 & Size & 0-100 cm & 0.00 \\
 & & 100-200 cm & 0.50 \\
 & & 200-300 cm & 0.75 \\
 & & 300+ cm & 1.00 \\
 \midrule


\end{longtable}
\end{center}
}
\end{singlespace}

\newpage
<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->
<!-- ### R packages -->
<!-- -=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=- -->

<!--
library(magrittr)
pack <- c('rfishbase',
          'taxize',
          'worrms',
          'rglobi',
          'motifcensus',
          'randomForest',
          'raster',
          'sp',
          'sf',
          'eDrivers',
          'magrittr',
          'tidyverse',
          'kableExtra',
          'rmarkdown',
          'btb',
          'fasterize')

pack <- sort(pack)
vers <- lapply(pack, function(x) as.character(packageVersion(x))) %>% unlist()

cit <- lapply(pack, function(x) {
  citation(x) %>%
  toBibtex()
})

pack <- data.frame(Name = pack,
                   Version = vers)

kableExtra::kable(pack, "latex", booktabs = T)
-->

<!-- \begin{singlespace}
{\fontsize{9}{11}\selectfont
\begin{center}
\begin{longtable}{ l l l l }
\caption{List of R packages used listed in alphabetical order. All analyses were carried out with the R programming language version. All analyses were carried out with the R programming language version 3.6.0 (R Core Team, 2019).}
\label{t-rpackages}
\tabularnewline
 -->
 <!-- \toprule -->
 <!-- Name & Version & Repository & Source \\
 \midrule
 \endfirsthead

 \toprule
 Name & Version & Repository & Source \\
 \midrule
 \endhead

 btb & 0.1.30 & CRAN &  Dos Santos et al. (2018)\\
 eDrivers & 0.0.1 & GitHub & Beauchesne (2020a) \\
 fasterize & 1.0.0 & CRAN & Ross (2018) \\
 kableExtra & 1.1.0 & CRAN & Zhu (2019) \\
 magrittr & 1.5 & CRAN & Bache and Wickham (2014) \\
 motifcensus & 0.0.2.9000 & GitHub & Cazelles and Beauchesne (2020) \\
 randomForest & 4.6.14 & CRAN & Liaw and Wiener (2002) \\
 raster & 2.8.19 & CRAN & Hijmans (2019) \\
 rfishbase & 3.0.3 & CRAN & Boettiger et al. (2012) \\
 rglobi & 0.2.19 & CRAN & Poelen et al. (2019) \\
 rmarkdown & 2.1 & CRAN & Xie et al. (2018), Allaire et al. (2020) \\
 sf & 0.7.4 & CRAN & Pebesma (2018) \\
 sp & 1.3.1 & CRAN & Pebesma and Bivand (2005), Bivand et al. (2013) \\
 taxize & 0.9.8 & CRAN & Chamberlain and Szocs (2013), Chamberlain et al. (2019) \\
 tidyverse & 1.2.1 & CRAN & Wickham (2017) \\
 worrms & 0.4.0.98 & CRAN & Chamberlain (2020) \\
 \bottomrule

\end{longtable}
\end{center}
}
\end{singlespace} -->

