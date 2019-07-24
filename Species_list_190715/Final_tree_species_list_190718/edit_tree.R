library(ape)
setwd("Desktop/CNUSA/Species_list_190715/Final_tree_species_list_190718/")
origin.tree <- read.tree("CN_US_5277_speciesname_BR_addJul182019.tre")
Ntip(origin.tree)
originlabel <- as.data.frame(origin.tree$tip.label)
write.csv(originlabel,"originlabel_lookup.csv",quote = F)
# Ampelopsis_delavayana [1499] [1503] 
# Carex_atlantica_subsp._capillacea [4620] [4671]
# Eupatorium_mohrii [2388] [2390] [2400]
# Panicum_dichotomum [4118] [4119]
drop.tip(phy = origin.tree, 1503, 4671,2390,2400,4119)
