rm(list=ls())
getwd()
library("picante")
library("phytools")
library("ape")
library("tidyverse")

###### building function PD_caculator ###### 
PD_caculator <- function(phy, name, data){
  comm <- NULL
  if(is.ultrametric(phy)!=TRUE){
    phy <- force.ultrametric(phy, method="extend")
  }
  write.csv(data, paste0("./result/", name, "_checked_matrix.csv", sep=""))
  comm <- data
  combined <- match.phylo.comm(phy, comm)
  phy<- combined$phy
  comm <- combined$comm
  
  cat("calculate PD ...\n")
  
  PD <- ses.pd(combined$comm, combined$phy, null.model = "taxa.labels", runs = 999, iterations = 1000)
  
  # caculate Standardized effect size of mean pairwise distances (MPD) in communities
  phy.dist <- cophenetic(phy)
  
  cat("calculate MPD ...\n")
  
  MPD <- ses.mpd(combined$comm, phy.dist, null.model = "taxa.labels", 
                 abundance.weighted = FALSE, runs = 999, iterations = 1000)
  MPD["NRI"] <- -1*MPD$mpd.obs.z
  
  # caculate Standardized effect size of  mean nearest taxon distances (MNTD) in communities
  
  cat("calculate MNTD ...\n")
  
  MNTD <- ses.mntd(combined$comm, phy.dist, null.model = "taxa.labels", 
                   abundance.weighted = FALSE, runs = 999, iterations = 1000)
  MNTD["NTI"] <- -1*MNTD$mntd.obs.z
  
  #write out table
  write.csv(PD, paste0("./result/", name, "_PD.csv", sep=""))
  write.csv(MPD, paste0("./result/", name, "_MPD.csv", sep=""))
  write.csv(MNTD, paste0("./result/", name, "_MNTD.csv", sep=""))
}

###### import final tree and replace alien symbols in metrix #######
tree <- read.tree("./data/CN_US_speciesname_BR_addJul192019_ad.tre")
tree$tip.label <- gsub("-", ".", tree$tip.label)

###### PD for 11 sites ###### 
sites <- read.csv("./data/site.mtrx_present_absent.csv", header = TRUE)
row.names(sites) <- as.character(sites[,1])
name <- "CN_US_11_sites"
PD_caculator(tree, name, sites)

###### PD for 2 countries ###### 
countries <- read.csv("./data/Country.mtrx_present_absent.csv", header = TRUE)
row.names(countries) <- as.character(countries[,1])
name <- "CN_US_2_sites"
PD_caculator(tree, name, countries)

###### PD for W_H one-mega site  ###### 
W_H <- read.csv("./data/WH_5306_coded.csv",header = TRUE)
W_H <- t(W_H)
View(W_H)
colnames(W_H) <- W_H[1,]
name <- "W_H_1_site"
PD_caculator(tree, name, W_H)

###### PD for W_H 11 sites  ###### 
W_H <- read.csv("./data/WH_5306_coded.csv",header = TRUE)
rownames(W_H) <- W_H[,1]
W.species <- W_H %>% mutate(Species=row.names(W_H)) %>% filter(Woody==1) %>% 
  select(Species)
W.species <- W.species$Species
H.species <- W_H %>% mutate(Species=row.names(W_H)) %>% filter(Herb==1) %>% 
  select(Species)
H.species <- H.species$Species
Site.matrix <- read.csv("data/site.mtrx_present_absent.csv", header = T)
rownames(Site.matrix) <- Site.matrix[,1]
Woody <- Site.matrix[,colnames(Site.matrix) %in% W.species]
Herb <- Site.matrix[,colnames(Site.matrix) %in% H.species]


name <- "W_11_site"
PD_caculator(tree,name,Woody)

name <- "H_11_site"
PD_caculator(tree,name,Herb)

###### PD for W_H each country  ###### 

Country.matrix <- read.csv("data/Country.mtrx_present_absent.csv",header = T)
rownames(Country.matrix) <- Country.matrix[,1]
Woody_Country <- Country.matrix[,colnames(Country.matrix) %in% W.species]
Herb_Country <- Country.matrix[,colnames(Country.matrix) %in% H.species]

name <- "W_2_site"
PD_caculator(tree,name,Woody_Country)

name <- "H_2_site"
PD_caculator(tree,name,Herb_Country)

###### PD for dis_non one-mega site  ###### 
dis_nondis <- read.csv("data/Dis_nondis_coded.csv", header = T)
dis_nondis <- t(dis_nondis)

colnames(dis_nondis) <- dis_nondis[1,]
name <- "Dis_non_dis_1_site"
PD_caculator(tree, name, dis_nondis)

###### PD for dis_non 11 sites  ###### 
dis_nondis <- read.csv("data/Dis_nondis_coded.csv", header = T)
rownames(dis_nondis) <- dis_nondis[,1]
dis.species <- dis_nondis %>% mutate(Species=row.names(dis_nondis)) %>% filter(dis==1) %>% 
  select(Species)
dis.species <- dis.species$Species
non.dis.species <- dis_nondis %>% mutate(Species=row.names(dis_nondis)) %>% filter(non_dis==1) %>% 
  select(Species)
non.dis.species <- non.dis.species$Species
Site.matrix <- read.csv("data/site.mtrx_present_absent.csv", header = T)
rownames(Site.matrix) <- Site.matrix[,1]
dis <- Site.matrix[,colnames(Site.matrix) %in% dis.species]
non.dis <- Site.matrix[,colnames(Site.matrix) %in% non.dis.species]

name <- "Dis_11_sites"
PD_caculator(tree, name, dis)

name <- "Non_dis_11_sites"
PD_caculator(tree, name, non.dis)

###### PD for dis_non  2 countries  ###### 

Country.matrix <- read.csv("data/Country.mtrx_present_absent.csv",header = T)
rownames(Country.matrix) <- Country.matrix[,1]
Dis_Country <- Country.matrix[,colnames(Country.matrix) %in% dis.species]
Non_dis_Country <- Country.matrix[,colnames(Country.matrix) %in% non.dis.species]

name <- "Dis_2_site"
PD_caculator(tree,name,Dis_Country)

name <- "Non_dis_site"
PD_caculator(tree,name,Non_dis_Country)
