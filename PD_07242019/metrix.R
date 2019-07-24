rm(list=ls())
library("ape")
library("tidyverse")
library("dplyr")
tree <- read.tree("./data/CN_US_speciesname_BR_addJul192019_ad.tre")
###### generate 11 sites 1/0 matrix #######
site.species <- read.csv("./.RData/11_sites_final.csv")   
colnames(site.species) <- c("Species","Site")
View(site.species)

S.code <- unique(site.species$Site)
Site.names <- c("BART", "CBS", "CWHL", "DLS", "GTS", "HARV", "MLBS", "OSBS","SNJ","TALL" ,"TMS")
matrix <- list()
for(i in 1:length(S.code)){
  dd <- site.species %>% group_by(Site) %>% filter(Site==S.code[i])
  ifelse(sum(dd$Species %in% tree$tip.label)==length(dd$Species), print("YES"), print(paste(S.code[i], ",", "NO")))
  dat <- as.data.frame(tree$tip.label)
  names(dat) <- "Species"
  dat <- mutate(dat, Present=ifelse(Species %in% dd$Species, 1, 0)) %>% arrange(Species)
  
  matrix[[S.code[i]]] <- dat
}

site.mtrx <- plyr::join_all(matrix, by="Species", type="left")
names(site.mtrx) <- c("Species", Site.names)
site.mtrx <- t(site.mtrx)
write.table(site.mtrx, "site.mtrx_present_absent.csv", sep=",",col.names=F, quote = FALSE)
View(site.mtrx)
a <- read.csv("site.mtrx_present_absent.csv")
a <-t(a)
summary(a)
###### generate CN vs. US 1/0 matrix ######
CN.US <- read.csv("./data/CN.US.csv")
colnames(CN.US) <- c("Species","Site")
summary(CN.US)
CN.US <- unique(CN.US)
C.code <- unique(CN.US$Site)
Country.names <- c("CN", "US") 
matrix <- list()
for(i in 1:length(C.code)){
  dd <- CN.US %>% group_by(Site) %>% filter(Site==C.code[i])
  dat <- as.data.frame(tree$tip.label)
  names(dat) <- "Species"
  dat <- mutate(dat, Present=ifelse(Species %in% dd$Species, 1, 0)) %>% arrange(Species)
  
  matrix[[C.code[i]]] <- dat
}

Country.mtrx <- plyr::join_all(matrix, by="Species", type="left")
names(Country.mtrx ) <- c("Species", Country.names)
Country.mtrx <- t(Country.mtrx)
write.table(Country.mtrx, "./Country.mtrx_present_absent.csv", sep=",",col.names=F, quote = FALSE)
dim(Country.mtrx)

###### generate disjunction vs. non-disjunction 1/0 matrix ######
rowdis_nondis <- read.csv("data/dis_nondis_metrix.csv")
setdiff(rowdis_nondis$Species,tree$tip.label)
pattern_table <- mutate(rowdis_nondis, dis=ifelse(Pattern %in% "D", 1, 0), 
                   non_dis=ifelse(Pattern %in% "N", 1, 0)) %>% select(Species, dis, non_dis)
write.csv(pattern_table, "./data/Dis_nondis_coded.csv", row.names = FALSE, quote=FALSE)
###### generate woody vs. herb 1/0 matrix ######
WH <- read.csv("./data/WH_5306.csv")

setdiff(WH$Species,tree$tip.label)

WH_table <- mutate(WH, Woody=ifelse(Lifeform %in% "W", 1, 0), 
                   Herb=ifelse(Lifeform %in% "H", 1, 0)) %>% select(Species, Woody, Herb)
View(WH_table)
write.csv(WH_table, "./data/WH_5306_coded.csv", row.names = FALSE, quote=FALSE)