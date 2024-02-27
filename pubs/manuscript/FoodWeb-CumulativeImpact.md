---
title: Ecological interactions amplify cumulative effects in marine ecosystems
author: David Beauchesne^1,2\*^, Kevin Cazelles^3^, Rémi M. Daigle^4,5^, Dominique Gravel^6^, Philippe Archambault^2,7^
fontsize: 12pt
output:
  pdf_document:
    toc: false
    number_sections: false
header-includes:
   - \usepackage{lineno}
   - \linenumbers
   - \usepackage{listings}
   - \usepackage{float}
   - \usepackage{setspace}
   - \usepackage{longtable}
   - \usepackage{booktabs}
   - \usepackage{tabularx}
   - \righthyphenmin=62
   - \lefthyphenmin=62
   - \usepackage{natbib}
bibliography: FoodWeb-CumulativeImpact.bib
csl: science.csl
# csl: frontiers.csl
link-citations: yes
relativeurls: true
---

<!--
rmarkdown::render('./FoodWeb-CumulativeImpact.md')
rmarkdown::render('./FoodWeb-CumulativeImpact.md', output_format = "word_document")
-->

**Title:** **Ecological interactions amplify cumulative effects in marine ecosystems**

**Authors:** David Beauchesne^1,2\*^, Kevin Cazelles^3^, Rémi M. Daigle^4,5^, Dominique Gravel^6^, Philippe Archambault^2,7^

**Affiliations:**

^1^Department of Health and Society, University of Toronto; Toronto, Canada.

^2^Québec Océan, Département de biologie, Université Laval; Québec, Canada.

^3^Department of Integrative Biology, University Of Guelph; Guelph, Canada.

^4^Bedford Institute of Oceanography, Fisheries and Oceans Canada; Dartmouth, Canada.

^5^Marine Affairs Program, Dalhousie University; Halifax, Canada.

^6^Département de biologie, Université de Sherbrooke; Sherbrooke, Canada.

^7^Takuvik International Research Laboratory, CNRS/Université Laval, ArcticNet; Québec, Canada.

\doublespacing

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- # Abstract -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

<!-- The abstract should be 100-125 words and organized in this structure: an opening sentence that sets the question that you address and is comprehensible to the general reader, background content specific to this study, results, and a concluding sentence. It should be a single paragraph. -->

**Abstract:** Biodiversity is not a mere collection of species; it also includes the diversity of interactions driving ecological dynamics and ecosystem functioning. Still, management overwhelmingly operates in silos, focusing on single stressor and species. Here, we assess the cumulative effects of climate change and human activities on species of the St. Lawrence marine ecosystem in eastern Canada using a novel approach that explicitly considers the less obvious yet no less significant effects arising from species interactions in a multiple stressors framework. We uncover cumulative effects that would otherwise be overlooked, particularly for fishes and marine mammals, many of which are exploited or endangered. This suggests that management plans and recovery strategies may be ignoring significant threats by overlooking species interactions. 

<!-- Biodiversity is not a mere collection of species; it also includes the diversity of interactions driving ecological dynamics and ecosystem functioning. Still, management overwhelmingly operates in silos, focusing on single stressor and species. Here, we assess the cumulative effects of climate change and human activities on species of the St. Lawrence marine ecosystem in eastern Canada using a novel approach that explicitly considers the web of interactions structuring communities. We uncover cumulative effects that would otherwise be overlooked, particularly for fishes and marine mammals, many of which are exploited or endangered. This suggests that management plans and recovery strategies may be ignoring significant threats by overlooking species interactions. We propose, to our knowledge, the first ecosystem-based approach relevant to species management which can evaluate the less obvious yet no less significant effects arising from species interactions in a multiple stressors framework. -->
 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- # Main text -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

**Main text:** Species interactions are a double-edged sword [@gilarranz2017]: those connections that are so vital to the existence of the complex ecological communities that inspired Darwin’s famous “*tangled bank*” metaphor are also those that allow the cumulative effects of climate change and human activities to spread through communities in a domino-like effect [@estes1974; @paine1980; @wootton2002; @bascompte2009a; @estes2011]. Stressors can rewire -- that is, reconfigure -- entire communities and alter energy flow in a system; for example, warming waters cause a northward advance of generalist fish species capable of disrupting both the structure and the strength of species interactions in the Arctic [@blanchard2015; @kortsch2015; @bartley2019]. Properly evaluating the effects of stressors on species should therefore consider species-specific sensitivities, the structure of local communities, and effects spreading through ecological interactions, *i.e.* trophically-mediated indirect effects [@beauchesne2021]. Management actions considering community structure and species interactions can lead to efficient and cost-effective outcomes, such as the restoration of kelp forests through the recovery of sea otters (*Enhydra lutris*), an important keystone species [@power1996] on the Pacific Coast of North America [@estes1974; @estes2010].

There is a growing demand for the management of the structural properties of whole communities to preserve ecosystems [@mccann2007; @tylianakis2010; @mcdonald-madden2016; @heinen2020] and for a broader application of regional cumulative effects assessments [@jones2016; @hodgson2019]. These are integral to an ecosystem-based approach to environmental management [@christensen1996]; still, species interactions remain conspicuously absent from the environmental management literature, biodiversity monitoring programs, the Intergovernmental Science-Policy Platform on Biodiversity and Ecosystem Services (IPBES) reports, and environmental regulations in general [@kollmann2016; @heinen2020]. Further, none of the headline or complementary indicators currently identified by the Parties to the Convention on Biological Diversity include the structure of ecological networks or trophic interactions [@cbd2023]. Here, we expand conventional cumulative effects assessment approaches (hereafter, species-scale assessment)[@maxwell2013; @halpern2019; @ohara2021] with recent progress in theoretical ecology [@stouffer2007; @stouffer2012; @beauchesne2021] to propose a novel method to capture the indirect propagation of the effects of stressors through species interactions (hereafter, network-scale assessment). 

We focus our network-scale assessment on the St. Lawrence marine ecosystem, in eastern Canada (see methods). This ecosystem is formed by one of the largest estuaries in the world and a vast interior sea. Together, they host diverse and productive ecological communities and provide a wealth of ecosystem services benefiting the Canadian economy: a rich commercial fisheries industry, a seaway that grants access to one of the most densely populated regions in North-America and more than 40 ports, an expanding aquaculture production, and a thriving tourism industry [@beauchesne2016; @schloss2017]. 

We demonstrate our approach by assessing and mapping the cumulative effects of 18 stressors on 193 species between 2010 and 2015. We use data-based or theoretically-derived indicators to characterize the distribution and intensity of stressors [@beauchesne2020], the distribution of species, the network of species interactions, and species-specific sensitivities to stressors; these are then combined through a theoretical framework [@beauchesne2021] to predict a cumulative effect score for every species considered (see methods). 

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- ## Results -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

<!-- ### Network-scale cumulative effects assessment -->

***Network-scale cumulative effects assessment***

The network-scale assessment considers the direct effects of stressors on a focal species, the effects of stressors on species the focal species interacts with (indirect effects), and the susceptibility of the focal species to the propagation of the effects of stressors, *i.e* its trophic sensitivity [@beauchesne2021] (fig. 1). For example, high-trophic-level consumers tend to be more susceptible to the propagation of stressors going up through the food chain [@sydeman2015; @stier2016]. The network-scale assessment deconstructs ecological networks using sub-networks of 3-species interactions called motifs [@milo2002] to assess cumulative effects. Motifs are subsets of *n*-species that, when put together, construct whole food webs and provide a mesoscale characterization of the structural properties of ecological communities [@bascompte2005; @stouffer2007; @stouffer2010; @stouffer2011; @bramonmora2018]. The network-scale assessment averages the effects of stressors on a focal species across its motif census, *i.e.* the collection of 3-species interactions in which it is involved in the food web [@stouffer2012; @beauchesne2021]. A network-scale assessment of cumulative effects is then obtained by summing the score of all species in each grid cell across the study area. Cumulative effects scores are predictions of effects that can be interpreted as an assessment of cumulative risk to species at a given location (see fig. 1 and methods for more details). 

We find that no area of the St. Lawrence is free of cumulative effects, with coastal areas, the whole St. Lawrence Estuary, the heads of the deep Channels -- *i.e.* Laurentian and Esquiman Channels and the Jacques Cartier Strait -- and the Magdalen Islands most heavily affected by stressors (fig. 2A). These are essentially areas of the St. Lawrence with a rich biodiversity overlapping with numerous high-intensity stressors. Stressors with the strongest effects are shipping, demersal destructive fisheries, and climate stressors (fig. 2B and fig. S1). Species most at risk from cumulative effects, meanwhile, are echinoderms (*e.g.* sea cucumbers and sea stars) and mollusks (*e.g.* scallops and sea snails; fig. 2B and fig. S1).


<!-- ### Interactions amplify cumulative effects  -->
***Interactions amplify cumulative effects***

We find that the spatial distribution of cumulative effects is similar between the network and species-scale assessments ($p_s$ = 0.93, P < 0.001; fig. S2). This is expected, as the distribution of cumulative effects will necessarily be captured in areas of greatest overlap between species and stressors for both assessments. The network-scale assessment, however, modulates the local intensity of cumulative effects based on species interactions to better encapsulate potential risks to species in an ecosystem. For each species, we quantified effects per $km^2$ by averaging cumulative effects over its entire range. In the St. Lawrence, we find that interactions amplify the effects of stressors for almost all species considered (fig. 3). We also find that the amplification of cumulative effects is not captured by the number of interactions a species is involved in (fig. 3); this means that the degree to which a species is connected in a food web could not be used to predict the amplification of cumulative effects arising from species interactions. 

While these findings are relevant for all species, it may be of particular concern for species with relatively low levels of cumulative effects in the species-scale assessment, many of which are endangered or commercially exploited. For example, groundfish populations -- Atlantic halibut (*Hippoglossus hippoglossus*), American plaice (*Hippoglossoides platessoides*), witch flounder (*Glyptocephalus cynoglossus*), Atlantic cod (*Gadus morhua*), redfish (*Sebastes spp.*), and Greenland halibut (*Reinhardtius hippoglossoides*) -- are generally affected by all stressor types directly and indirectly, with indirect effects often larger than direct effects (figs. S3A and S4). The network-scale assessment shows that interactions and local community structure modulate the magnitude of cumulative effects over the range of groundfish species. The groundfish populations of the St. Lawrence collapsed in the early 1990s due to overfishing [@savenkoff2007; @morissette2009] and shifts in trophic structure have been cited as the likely cause for their slow recovery [@jackson2001; @frank2005; @pedersen2017]. For such species, the risks associated with cumulative effects are likely underestimated when species interactions are overlooked.

A species may also be indirectly affected by stressors in new locations that were either not captured by species-scale assessments, or where cumulative effects were very low. The beluga whale (*Delphinapterus leucas*), an endangered and emblematic predator population whose high-profile conservation efforts have yielded no apparent recovery, is an eye-catching example in the St. Lawrence marine ecosystem [@lesage2020; @lesage2021]. The population is mostly affected directly by marine traffic and coastal stressors along the coast and across vessel tracks. However, the beluga population is indirectly affected by all stressor types. Furthermore, new areas along the coast that would be overlooked if species interactions were not considered are highlighted through the network-scale assessment (figs. S3B ad S4). This supports recent reports discussing the importance of trophically-mediated effects in the lack of recovery of the beluga population, such as potential competition with other marine mammals for prey species [@lesage2020; @lesage2021]. Considering interactions may be most crucial for such species, and perhaps the only way to capture all sources of stress and identify gaps in management measures.


<!-- ### Contribution to cumulative effects -->
***Contribution to cumulative effects***

We further explore the details of the assessment by extracting the contribution of stressors and species to the cumulative effects of species combined into 6 broad taxonomic groups: invertebrates divided into arthropods (Arthropoda; *n = 30*), Cnidaria (*n = 20*), echinoderms (Echinodermata; *n = 21*) and mollusks (Mollusca; *n = 19*), and vertebrates divided into fishes (Actinopterygii; *n = 62*) and marine mammals (Mammalia; *n = 24*). Stressors are also combined into broader groups, *i.e.* climate (*n = 6*), coastal (*n = 5*), fisheries (*n = 5*) and marine traffic (*n = 2*). 

We find that direct effects are greatest on invertebrates and dominated by climate stressors (fig. 4 and fig. S4). Direct effects on vertebrates, meanwhile, are relatively milder and limited to fewer stressors; that is, predominantly fisheries for fishes and marine traffic for marine mammals (fig. 4 and fig. S4). These results reflect broad taxa-specific sensitivities to the effects of stressors. Invertebrates are predominantly benthic species with limited mobility, many of which feed by filtering the water column, have calcified exoskeletons, or both; these traits make them generally more susceptible to the effects of stressors affecting the physico-chemical properties of their environment, and particularly to the effects of climate stressors [@kroeker2010; @kroeker2013; @butt2022]. Vertebrates, on the other hand, are generally mobile and active species, and thus able to minimize their exposure to stressors. Fishes are ectothermic and more susceptible to the effects of physico-chemical variations in their environment when compared to endothermic mammals. Yet both fishes and marine mammals are much less sensitive than invertebrates to climate stressors [@kroeker2010; @sydeman2015].

This would conclude the interpretation of the species-scale assessment; the network-scale assessment, however, offers more insights. Cumulative effects spreading through species interactions -- that is, indirect effects -- are prevalent across all taxonomic groups and from all stressor types (fig. 4 and fig. S4); they also often equal or exceed direct effects for arthropods, marine mammals, and most noticeably for fish species. As with direct effects, this also reflects taxa-specific sensitivities, where higher trophic level species such as fishes and marine mammals tend to be most sensitive to the propagation of effects spreading through their ecological interactions [@sydeman2015; @stier2016]. These results widen our understanding of the breadth of environmental risks to which species may be exposed. For instance, without indirect effects, fisheries and climate stressors may be perceived as non-factors for marine mammals, and all management efforts be directed towards stressors originating from marine traffic. This does not mean that marine traffic is not a threat to marine mammals. It means, however, that the management of marine mammals may be a more multifaceted challenge than anticipated.

Through the network-scale assessment, we can also quantify the contribution of species to the propagation of cumulative effects through their interactions with other species. In the St. Lawrence, fishes as a group contribute the most to spreading cumulative effects through the food web (fig. 4 and fig. S5). Arthropods and marine mammals have roughly similar contributions, while the overall contribution of other invertebrate groups is minimal (fig. 4 and fig. S5). Broadly speaking, the total contribution of a species to indirect effects increases with the number of species it interacts with ($p_s$ = 0.57, P < 0.001) and with the relative intensity of direct effects affecting it ($p_s$ = 0.46, P < 0.001). 

Explored in finer details, the results of the network-scale assessment can reveal a wide array of potential threats to species of interest. For instance, the population of Northern shrimp (*Pandalus borealis*) has been steadily decreasing in the St. Lawrence for the past two decades. Here, we find that shrimp are predominantly affected by the same stressors directly and indirectly: bottom-water climate stressors (direct: 33%; indirect: 15%), demersal fisheries (direct: 26%; indirect: 11%), and shipping (direct: 0%; indirect: 10%). Environmental threats are thus likely underestimated for shrimp in the St. Lawrence. We can push this exploration further to find that ~29% of indirect effects for Northern shrimp originate from 5 species: Atlantic cod (9%), snow crab (*Chionoecetes opilio*; 7%), redfish (5%), Atlantic halibut (4%) and Greenland halibut (4%). The redfish population, for example, dramatically increased over the past decade and Northern shrimp is a major component of redfish diet [@brown-vuillemin2022]. Together with climate stressors, trophically-mediated indirect effects could pose significant risks to the Northern shrimp population of the St. Lawrence [@brosset2019]. Such predictions could have tremendous value to management and conservation by identifying a more comprehensive range of potential risks for species of interest. <!-- It could also help in identifying early-warning signals -->


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- ## Discussion -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

***Discussion***

After decades of accumulated empirical and theoretical knowledge on the significance of trophically-mediated indirect effects [@estes1974; @paine1980; @wootton2002; @estes2011], the importance of species interactions and community structure in understanding the effects of global changes is increasingly recognized: species cannot be considered in a vacuum [@tylianakis2008; @ogorman2009; @gilarranz2016; @gilarranz2017; @ogorman2019; @waller2020; @beauchesne2021]. We should strive to distance ourselves from species-centric approaches in favor of holistic approaches to monitor and preserve biostructures [@mccann2007]. 

Here, we quantitatively show the gains in ecological understanding we can achieve by considering the less obvious, yet no less damaging, effects of multiple stressors amplified through species interactions. For many species of the St. Lawrence, our approach uncovers novel sources of stress and reveals that the cumulative risks associated with indirect effects can equal and even outweigh risks from direct effects for certain species. For species particularly susceptible to indirect effects, like fishes and marine mammals, properly assessing cumulative effects seems achievable solely through the lens of species interactions. Most commercially exploited species and species at risk in the St. Lawrence fall in that category; this is worrisome, as it suggests that current management plans and recovery strategies may ignore significant threats by overlooking indirect effects. 

In pairing theory, environmental management and computational capabilities, our approach offers concrete tools to efficiently identify the most critical threats to an ecosystem of interest. It is modular, flexible, grounded in theory and data-oriented; the expanding corpus of openly available environmental knowledge and computational capabilities can thus be leveraged to independently and incrementally enhance the quality of cumulative effects assessments. It builds on existing knowledge -- without replacing it -- and can guide future research to optimize the acquisition of scientific evidence and establish a clear feedback loop between holistic and reductionist approaches. It is also a solid foundation to extend cumulative effects assessments to social-ecological networks and assess indirect risks to social and cultural concerns such as food security and health. We believe that combining such holistic capabilities with broad applicability is currently one of the most important challenges in fulfilling the unkept promise of ecosystem-based management.


<!-- =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= -->
# References
<!-- =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= -->

<div id="refs"></div>

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Acknowledgments
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

**Funding:** We thank the Fond de Recherche Québécois Nature et Technologie (FRQNT) and the Natural Science and Engineering Council of Canada (CRSNG) for financial support. This project is supported by Québec Océan, the Quebec Centre for Biodiversity Science (QCBS) and Takuvik networks. This research is also sponsored by the NSERC Canadian Healthy Oceans Network and its Partners: Department of Fisheries and Oceans Canada and INREST (representing the Port of Sept-Îles and City of Sept-Îles). This research was enabled in part by support provided by WestGrid (www.westgrid.ca) and Compute Canada (www.computecanada.ca). 

**Export support:** We thank L. Tréau de Coeli for support in curating biotic datasets, and P. Calosi and C. Carrier-Belleau for support in ascribing sensitivities to traits.

**Author contribution:** DB, DG, and PA conceived the manuscript and the underlying objectives. DB prepared/formatted the data, performed the analyses, oversaw technical developments and led the drafting of the manuscript. KC and RD contributed to technical developments and analyses for the manuscript. All co-authors contributed to the revision of the manuscript.

**Competing interest declaration:** The authors declare no competing interests.

**Data and materials availability:** The data used for the cumulative effects assessment of environmental stressors on the food webs of the St. Lawrence marine ecosystem are available through Zenodo (DOI: ) and available at the following link: ***add link***. The code used for this assessment is available through multiple GitHub repositories archived on Zenodo. The code to reproduce the cumulative effects assessment is available at https://github.com/Ecosystem-Assessments/nceastl (DOI: ). The code to produce the stressor layers is available through the *eDrivers* GitHub organization (https://github.com/eDrivers) and described in @beauchesne2020. The code used to extract species traits is available at https://github.com/eBiotic/Traits (DOI: ). The code used to predict taxa distribution is available at https://github.com/eBiotic/Biotic (DOI: ). The code used to predict species interactions is available at https://github.com/david-beauchesne/Predict_interactions (DOI: ) and https://github.com/davidbeauchesne/Interaction_catalog (DOI: ), and described in @beauchesne2016. The code used to predict the St. Lawrence metaweb is available at https://github.com/david-beauchesne/MetawebEGSL (DOI: ). The code to evaluate species-specific sensitivities is available at https://github.com/david-beauchesne/Species_Vulnerability (DOI: ). The code to evaluate trophic sensitivity is available at https://github.com/david-beauchesne/FoodWeb-MultiStressors (DOI: 10.5281/zenodo.5014237) and described in @beauchesne2021. ***Note:*** *DOIs will be added once review process for publication is completed*


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Supplementary Materials
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

Materials and Methods

Figs S1 to S5

Tables S1 to S7

<!-- =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= -->
<!-- # Figure legends -->
<!-- =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= -->

**Fig. 1. Network-scale cumulative effects assessment method.** The assessment relies on data-based knowledge on the distribution and relative intensity of environmental stressors (**A**), the distribution of species (B), the relative sensitivity of species to the effects of stressors (C), the metaweb of ecological interactions -- *i.e.* who eats whom -- and the susceptibility of species to the propagation of the effects of stressors through their interactions, *i.e* their trophic sensitivity. For a particular cell in a regular grid dividing an area of interest, the method extracts the local food web and the intensity of stressors (D). In this example, the focal cell includes 3 stressors (climate change-induced temperature anomalies, commercial shipping, and trawl fishing) affecting 5 species: krill (Euphausiacea), copepods (Copepoda), capelin (*Mallotus villosus*), Atlantic cod (*Gadus morhua*), and beluga whales (*Delphinapterus leucas*). For each species, cumulative effects are predicted across their collection of 3-species interactions, *i.e.* their motif census. Here, the beluga is involved in 3 motifs: 1 omnivory interaction (beluga-cod-capelin) and 2 tri-trophic food chains (beluga-capelin-krill; beluga-capelin-copepod; E). For each 3-species interaction, direct and indirect effects are those affecting the focal species and those affecting the species it interacts with, respectively. Effects are predicted as the sum of the product of the intensity of stressors, the sensitivity of species to the effects of stressors, and the trophic sensitivity of the focal species. A weight of relative importance is used to combine direct and indirect effects. The total effect is the combination of all predicted effects (F). Net effects on species are then evaluated as the average of total effects predicted across 3-species interactions (G). This process is performed for every cell in the study grid to obtain a map of predicted cumulative effects for every species considered (H). The sum of all species assessments provides the network-scale cumulative effects predictions (I).

**Fig. 2. Network-scale cumulative effects assessment in the St. Lawrence marine ecosystem.** (**A**) Network-scale cumulative effects assessment of 18 stressors on 193 species in the St. Lawrence explicitly considering the underlying structure of the ecological community. Grey points represent the cumulative effects scores / $km^2$ in bioregions of the St. Lawrence. (B) Multiplex network presenting the metaweb of interactions between the 193 species considered (grey connections), the presence of an effect of individual stressors on each species (colored connections), the overall cumulative effect on each species (species point size) and the mean effect of stressors (stressor point size).

**Fig. 3. Comparison of network-scale and species-scale cumulative effects for individual species.** Scatterplot of the mean network-scale cumulative effects as a function of the species-scale cumulative effect over a species distribution ($C / km^2$) for all 193 species considered. The size of the point is the degree of each species, *i.e* the number of interactions in which they are involved in the metaweb. The orange line represents the direct effects baseline, meaning that results from the network-scale assessment cannot fall below that line. Species along the orange line are those for which the network-scale and species-scale assessments are equal; that is species with no predicted interaction. The black line is the 1:1 identity line. Values above or below that line identify species for which the network-scale assessment is relatively greater or lower than the species-scale assessment, respectively.

**Fig. 4. Contribution of stressors and species to indirect effects in the network-scale cumulative effects assessment.** The upper half of the figure illustrates the mean contribution of climate (*n = 6*), coastal (*n = 5*), fisheries (*n = 5*), and marine traffic (*n = 2*) stressors to the regional cumulative effects assessment ($C / km^2$) on invertebrates grouped at the phyla taxonomic level (Arthropoda: *n = 30*; Cnidaria: *n = 20*; Echinodermata: *n = 21*; Mollusca: *n = 19*) and vertebrates grouped at the class taxonomic level (Actinopterygii: *n = 62*; Mammalia: *n = 24*). The contribution can be divided into the direct and indirect contributions of stressors to the cumulative effects of each taxonomic group. Direct effects are those attributable to the effect of stressors on a focal species, while indirect effects are the effects of stressors spreading through species interactions. The size of the point is proportional to the relative contribution of stressor groups to a taxonomic group. The grey gradient of boxes surrounding stressor points is proportional to the combined contribution of all stressors to direct or indirect effects, with low or high contributions presented as pale or dark grey, respectively. The lower half of the figure illustrates the mean contribution of taxonomic groups to the propagation of indirect effects to taxonomic groups. As with the stressors, the size of the points represents the relative contribution of taxonomic groups to indirect effects, and the grey gradient of boxes that of the combined contributions of invertebrates or vertebrates to indirect effects.


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
# Supplementary Materials
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

# Materials and Methods

## General model

Whole food webs can be decomposed into collections of $p$-species interactions called motifs [@milo2002] that provide a mesoscale characterization of the structural properties of ecological networks [@bascompte2005; @stouffer2007; @stouffer2010; @stouffer2011; @bramonmora2018]. In a $n$-species food web ($n \geq p$), the collection of $p$-species motifs ($p \leq n$) in which species $i$ is involved in ($M_i = \{m_{i,1},m_{i,2},...,m_{i,x}\}$) forms its motif census ($M_i$) [@stouffer2012; @beauchesne2021]. The motif census provides an overview of all the interactions and connected species likely to affect a species’ dynamics, and the propagation of disturbances through their interactions. Here, we focus exclusively on the most abundant 3-species motifs in empirical food webs (*i.e.* trophic food chain, omnivory, exploitative and apparent composition) [@camacho2007; @stouffer2010] to assess a species motif census, although the general model would be applicable to any $p$-species motifs.

Network-scale cumulative effects scores ($C_N$) were predicted in each grid cell as follows: 

$$C_{N_x} = \sum_i \frac{1}{|M_i|} \sum_{m_{i,x} \in M_i} \sum_j D_j * \overline{\mu_j} * T_{i_{m_{i,x}}}$$

where $i$ is the focal species, $M_i$ is the motif census of species $i$, $m_{i,x}$ are the 3-species motifs of interest forming species $i$'s motifs census, and $D_j$ is the log-transformed and scaled intensity of stressor $j$. 

$\overline{\mu_j}$ corresponds to the joint sensitivity to stressor $j$ of the species involved in motif $m_{i,x}$. Here, we explicitly consider that a species' response to stressors depends on its own response as well as the response of species it interacts with. The joint sensitivity is measured as:

$$\overline{\mu_j} = w_1 \mu_{i,j} + w_2 \sum_k^2 \mu_{k,j}$$

$\mu_{i,j}$ and $\mu_{k,j}$ are the sensitivities to stressor $j$ of focal species $i$ and of the two species interacting with focal species $i$ in motif $m_{i,x}$, respectively. $w_1$ and $w_2$ are weighting factors that give a relative importance to direct -- *i.e.* effects to species $i$ -- and indirect -- *i.e.* effects propagating through species $k$ to species $i$ -- effects in the assessment. $w_1 + 2 w_2 = 1$ to directly relate the weighting to a percent contribution to direct and indirect effects. For this assessment, we used $w1 = 0.5$ and $w2 = 0.25$ to give equal weight to direct and indirect effects. 

$T_{i_{m_{i,x}}}$ is the trophic sensitivity of species $i$ in motif $m_{i,x}$. It captures a species sensitivity to trophically-mediated effects, which depends on the structure of the community, the trophic position of focal species $i$ and the specific entry points of stressors in the system [@beauchesne2021]. 

## Stressors

We used the spatial distribution and intensity of 18 stressors available through an open-knowledge platform called *eDrivers* [@beauchesne2020]. Stressors are divided into 4 groups: land-based (*i.e.* inorganic pollution, organic pollution, nutrient input, coastal development, and direct human impact), climate (*i.e.* positive and negative bottom-water and surface-water temperature anomalies, ocean acidification, and hypoxia), fisheries (*i.e.* demersal destructive, demersal non-destructive high-bycatch, demersal non-destructive low-bycatch, pelagic high-bycatch, and pelagic low-bycatch) and marine traffic (*i.e.* shipping and marine pollution; table S1). Methods to characterize each stressor are described in @beauchesne2020. Stressors with non-normal frequency distributions were log-transformed to avoid underestimating intermediate stressor intensity values [@halpern2019]. All stressors were scaled between 0 and 1 to obtain relative intensities and allow comparisons between stressors. For each stressor, the 99th quantile of intensity distribution was used as the upper bound for scaling to control for extreme values that may or may not be real observations.

## Species distribution

### Biotic data

We used data from 4 monitoring programs conducted by Fisheries and Oceans Canada (DFO) to obtain a list of taxa with observed occurrences in the St. Lawrence marine ecosystem (table S2). We included a list of 30 known whale and seal species in the St. Lawrence and used distribution ranges available from the IUCN Species Red List of Threatened Species for 24 of the 30 marine mammal species (table S2) [@iucn2020]. We curated the list of taxa used for the analyses by grouping and removing taxa based on expert knowledge and bibliographic research. For example, species of the same genus and hard to distinguish were grouped; species that were identified as probable misidentifications or outliers were removed from the data. The curation process yielded 424,953 taxa occurrences and 434,851 taxa absences for 391 taxa between 2010 and 2015 (tables S2 and S3). The curation process was documented and is available on GitHub (https://github.com/eBiotic/Biotic). All species scientific names were resolved using the `taxize` R package [@chamberlain2013; @chamberlain2020a].

### Abiotic data

We used environmental data characterizing the bottom-water and surface-water salinity, temperature, oxygen, primary productivity, pH (surface) and aragonite (bottom) conditions in the St. Lawrence marine ecosystem. We also considered latitude, longitude and depth, for 13 environmental descriptors. The data was accessed through various regional [@dutil2011; @dutil2012; @galbraith2018; @starr2019; @blais2019] and global [@assis2018] environmental monitoring programs and public repositories (table S4).

### Spatial distribution

We extrapolated and mapped the distribution of taxa in the St. Lawrence marine ecosystem using the Random Forest ensemble learner [@breiman2001]. We used the default parameters proposed by the `randomForest` R package to classify the presence or absence of taxa: 500 trees and the number of variables in the random subset at each node set to the square root of the number of variables [@liaw2002]. We only considered taxa with at least 50 observations, yielding a total of 169 taxa (table S3). Each taxon was modelled using all 13 environmental descriptors (table S3). We generated pseudo-absences for taxa without absences in the dataset ($n = 5$) by randomly sampling the study area at least 5 km away from observed points; for these taxa, we generated the same number of pseudo-absences as observed occurrences [@barbet-massin2012]. We measured the performance of the models for each taxon using the sensitivity, specificity, accuracy and True Skilled Statistics (TSS; table S3) [@allouche2006]. For each taxon, we predicted spatial distribution within the same 1 $km^2$ resolution grid used for stressors. Individual taxa distributions were then smoothed using bisquare kernel smoothing [@dossantos2022] with a 5 km radius to avoid potentially granular distributions that would affect estimations of species co-occurrence. We used distribution ranges available from the IUCN Species Red List of Threatened Species for 24 species of marine mammals [@iucn2020]. Our dataset includes distribution maps for 193 taxa in the St. Lawrence marine ecosystem. We assumed that phytoplankton and zooplankton species were present throughout the St. Lawrence since these taxa are missing from our dataset and are required to consider trophic dynamics properly.

## Species-specific sensitivity 

### Traits data

We documented the body composition, the maximal body size, the type of marine environment in which species are found, the feeding mode, the mobility and the phylum (table S5) of all 391 taxa available in the biotic dataset. We extracted traits data from the World Register of Marine Species (WoRMS) [@wormseditorialboard2017], FishBase [@froese2019], SeaLifeBase [@palomares2019], the Encyclopedia of Life [@encyclopediaoflife2020] and the Global Biotic Interaction (GloBI) database [@poelen2014; @poelen2022a].  We used the `taxize` [@chamberlain2013; @chamberlain2020a], `worrms` [@chamberlain2020] and `rfishbase` [@boettiger2012] R packages to extract traits data. Any taxon for which traits were unavailable programmatically were searched manually on the WoRMS and Encyclopedia of Life web portals. We also documented whether a species was targeted by fisheries or caught as bycatch by local fisheries using data from DFO's Fisheries Logbook Program [@dfo2016].

### Species-specific sensitivity

We evaluated the species-specific sensitivity of all 391 taxa to each stressor using a trait-matching approach. For each stressor, we identified traits that were known or suspected to influence a species sensitivity to the effects of the stressor (table S6). For example, the feeding strategy of an organism affects its sensitivity to nutrient and metal loading [@ellis2017], whereas its body composition affects its sensitivity to ocean acidification [@kroeker2013]. Traits were categorized to reflect their relative contribution to the sensitivity of a taxa to the effects of a stressor. For example, suspension feeders are generally more affected by nutrients and metals than deposit feeders [@ellis2017], whereas calcifying organisms are more vulnerable to the effects of ocean acidification than non-calcifying organisms [@kroeker2013]. Traits were categorized by giving a weight between 0 and 1 that reflects their relative contribution to the sensitivity of a taxa to a stressor: a weight of 0 represents a trait rendering taxa insensitive to the effects of a stressor, whereas a weight of 1 represents a trait associated with the highest relative sensitivity of a taxon to the effects of the stressor. The maximal sensitivity weight was retained if a taxon had multiple traits in a single category (*e.g.* crawler and swimmer). This sensitivity assessment was informed by expert knowledge and bibliographic research. Trait-matching rules and relative sensitivity weights for each stressor are available in table S5. The relative sensitivity of each taxon to a stressor was then evaluated as the product of the relative sensitivity weight of all traits associated with taxa sensitivity to the effects of the stressor. For example, the relative sensitivity to ocean acidification was evaluated using environment, mobility, body composition and phylum traits (table S5). This process yielded a relative sensitivity assessment ranging between 0 and 1 for each taxa.

## Metaweb

We predicted the metaweb of the St. Lawrence marine ecosystem, *i.e.* the network of biotic interactions, using a recommender approach [@beauchesne2016]. Here, we provide a brief overview of the approach, but refer to @beauchesne2016 for more details. The approach consists of a series of logical steps that predict a candidate resource list for each taxon based on empirical data available and the similarity among consumers and resources. It uses the K-nearest neighbour algorithm (KNN) [@murphy2012] to predict pairwise interactions given taxonomic and dietary similarity between consumers and resources and is informed by a catalogue of empirically known biotic interactions worldwide [@beauchesne2016]. The interactions catalogue was built using food web data [@brose2005; @kortsch2015; @universityofcanberra2016], predator-prey interactions [@barnes2008] and pairwise interactions from the GloBI database [@poelen2014; @poelen2022a]. We limited the compendium to taxa found in marine and coastal ecosystems. Taxa similarity was evaluated from taxonomic classification and sets of consumers or resources using the Tanimoto similarity measure. A weight of 0.5 was given to taxonomy and consumers or resources to consider them simultaneously [@desjardins-proulx2016]. The taxonomy of all taxa considered was accessed and validated from WoRMS [@wormseditorialboard2017] using the `taxize` package [@chamberlain2013; @chamberlain2020a]. We included the main phytoplankton and zooplankton taxa found in the St. Lawrence marine ecosystem to predict the metaweb [@morissette2003; @savenkoff2004; @savenkoff2012]; we then grouped predictions under phytoplankton or zooplankton. This yielded a total of 393 taxa ($S$), considering all 391 taxa identified through the biotic data and the addition of phytoplankton and zooplankton. We predicted a metaweb structured by 4880 links ($L$), a link density ($L_{moy} = L/S$) of 12.42 and a connectance ($C = L/S^2$) of 0.03, which is within range of most reported food webs [@dunne2002a].

## Trophic sensitivity

We provide a brief overview of the approach used by @beauchesne2021 to evaluate a species' sensitivity to multiple stressors given its trophic position, *i.e* its trophic sensitivity. The effects of multiple stressors on the dynamics of the most empirically abundant 3-species motifs -- *i.e.* tri-trophic food chain, omnivory, exploitative competition and apparent competition -- were simulated using Lotka-Volterra models [@gellner2016]. The dynamics of a single species regulated by density-dependent growth was also simulated; this control was included to consider disconnected species in the metaweb, which may arise due to insufficient data or because a species consumes detritus, bacteria or particulate organic matter. It was also used for the species-scale cumulative effects assessment so that disconnected species had the same cumulative effects results in the network-scale and species-scale assessments.

Negative effects of stressors were simulated by modifying combinations of equilibria equation parameters of population resource growth, mortality, attack and conversion rates (*i.e.* up to 9 parameters and 511 distinct pathways of effect) [@beauchesne2021]. Modifications to parameters simulate effects of stressors on ecological processes; these represent the pathways through which stressors directly and indirectly affect ecological communities. The set of all ecological processes affected by stressors across species combine to collectively affect a community and form a pathway of effect. For each 3-species motif, all possible pathways of effects were simulated; this resulted in 127 unique pathways of effect for tri-trophic food chain, exploitative competition and apparent competition motifs through 7 parameters, 511 pathways of effect for the omnivory motif through 9 parameters, and 1 pathway of effect for disconnected species through 1 parameter. For each pathway of effect, a species's trophic sensitivity was defined as the difference in its equilibrium abundance before and after the permanent appearance of stressors in the system; this represents the net effect of stressors on species and integrates all direct and indirect effects propagating to a focal species [@abrams1996; @beauchesne2021].

Here, due to the challenge of empirically attributing effects to specific ecological processes, we simplified pathways of effects and broadly considered effects to a species density rather than effects to specific ecological processes as in @beauchesne2021. We used trophic sensitivities across possible pathways of effect simulated in @beauchesne2021 as heuristics to assess a species' trophic sensitivity to the effects of stressors given its position in 3-species motifs. We used the absolute values of simulated trophic sensitivities and considered that any effect to a species' population dynamics, whether negative or positive, can propagate and disturb the dynamics of an ecological community. We then simplified pathways of effects as a function of their effects to the density of each species. For example, a pathway of effect targeting the mortality of a consumer was considered to affect the density of that consumer, whereas a pathway of effect targeting the attack rate of a consumer was considered to affect the density of both the consumer and the resource. We averaged trophic sensitivities to pathways of effects according to their contribution to the effects on species density, resulting in 8 possible pathways of effect for all 3-species motifs and 2 pathways of effect for the single species, each with a pathway of effect where no effects are observed. Finally, we centered trophic sensitivities so that disconnected species had a value of 1, resulting in a trophic sensitivities ranging from 0 to 6.17, with an average of 1.67 $\pm$ 1.57 and a median of 1.31.

## Assessment and spatial data representation

The open-source software R 4.2.3 was used for all analyses [@rcoreteam2023] and the package *rcea* [@beauchesne2023] was used to perform the cumulative effects assessment. See table S7 for a list of all R packages used. All datasets are presented at a 1 $km^2$ resolution even though some source data had coarser resolutions (table S1). We resampled and reprojected data when necessary using nearest neighbour estimates, which preserves the values of the source data. By doing so, we assume that the coarser data are evenly distributed across finer-scale cells with which they overlap. We used the NAD83 / Quebec Lambert projection (EPSG: 32198), which is well suited to represent and preserve surface area within our study system. 


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- # Supplementary figures -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

**Fig. S1. Multiplex network of cumulative effects.** Multiplex network presenting the metaweb of interactions between the 193 taxa considered (grey connections), the presence of an effect of individual stressors on each taxa (colored connections), the overall cumulative effect on each taxon (taxa point size) and the mean effect of stressors (stressor point size).

**Fig. S2. Comparison between the network-scale and species-scale cumulative effects assessment.** (**A**) Network-scale relative cumulative effects assessment of 18 stressors on 193 taxa in the St. Lawrence marine ecosystem explicitly considering the underlying structure of the ecological community. (B) Species-scale relative cumulative effects assessment of 18 stressors on 193 taxa in the St. Lawrence ignoring the underlying structure of the ecological community, *i.e.* the environmental impact assessment *status quo* or individual approach to cumulative effects assessment. (C) Spatial distribution of transgressive properties arising from species interactions, measured as the sum of the log ratio of individual species network-scale assessment over species-scale assessment in each 1 $km^2$ grid cell. (D) Scatterplot of the network cumulative effects score as a function of the individual cumulative effect score for each 1 $km^2$ grid cell. The orange line is the 1:1 identity line.

**Fig. S3. Comparison of the network-scale and species-scale cumulative effects assessments for selected species.** (**A**) groundfish species (*i.e.* Atlantic halibut *Hippoglossus hippoglossus*; American plaice *Hippoglossoides platessoides*; witch flounder *Glyptocephalus cynoglossus*; Atlantic cod *Gadus morhua*; redfish *Sebastes spp.*; Greenland halibut *Reinhardtius hippoglossoides*) and (B) beluga whales (*Delphinapterus leucas*).

**Fig. S4. Contribution of stressors to cumulative effects of species.** Mean contribution of 18 stressors to the regional cumulative effects assessment ($C / km^2$) of 193 taxa in the St. Lawrence marine ecosystem classified into 8 taxonomic groups. Taxonomic groups for invertebrates and vertebrates are divided at the phyla and classes level, respectively. The contribution can be divided into the direct and indirect contributions of each stressor to the cumulative effects on each taxa. Direct effects are those attributable to the effect of a stressor on a focal species, while indirect effects are the mean effects of stressors spreading through all 3-species interactions a taxa is involved in. The total contribution to cumulative effect is the sum of the direct and indirect contributions. Commercially exploited taxa and endangered, threatened and species of concern under the Canadian species at risk act are identified on the figure next to taxa names. 

**Fig. S5. Contribution of species to indirect effects.** Mean contribution of species to the indirect propagation of regional cumulative effects ($C_i / km^2$) of 18 stressors through ecological interactions among 193 taxa in the St. Lawrence marine ecosystem classified into 8 taxonomic groups. Taxonomic groups for invertebrates and vertebrates are divided at the phyla and classes level, respectively. Indirect effects (rows) are represented by a different color for each taxonomic group.


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
<!-- # Supplementary tables -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

**Table S1.** List of stressors used to perform the cumulative effects assessment [@beauchesne2020].

**Table S2.** List of biotic data used to obtain species list and occurrences. Except for IUCN data, monitoring programs are all conducted or managed by the Government of Canada's Department of Fisheries and Oceans (DFO).

**Table S3.** List of taxa included in the assessment with occurrences and absences from biotic data, and assessment of sensitivity, specificity, accuracy and True Skilled Statitics (TSS) of species distribution modeling.

**Table S4.** List of abiotic descriptors used to extrapolate and map taxa distribution in the St. Lawrence marine ecosystem.

**Table S5.** List of traits used to characterize the relative species-specific sensitivity of taxa to stressors and description of each trait category.

**Table S6.** Trait matching rules between stressors and traits to assess the relative species-specific sensitivity scores for each stressor.

**Table S7.** List of R packages used listed in alphabetical order. All analyses were carried out with the R programming language version 4.2.3 [@rcoreteam2023].


R Packages [@dossantos2022; @beauchesne2020a; @ross2022; @bache2022; @cazelles2023; @liaw2002; @hijmans2023; @beauchesne2023; @boettiger2012; @poelen2022a; @xie2018; @xie2020; @allaire2023; @pebesma2018; @pebesma2023; @bivand2013; @pebesma2005; @pebesma2022; @chamberlain2013; @chamberlain2020a; @wickham2019; @chamberlain2020].