# Phylogentic Diversity (PD) caculation

rm(list=ls())
# install.package("picante"）
library("picante")
library("phytools")
library("ape")
library("tidyverse")

PD_caculator <- function(phy, name, data){
  comm <- NULL
  if(is.ultrametric(phy)!=TRUE){
    phy <- force.ultrametric(phy, method="extend")
  }
   # if(sum(!complete.cases(phy$tip.label %in% colnames(data))) > 0){
   #   for(i in grep("-", phy$tip.label)){
   #     phy$tip.label[i] <- gsub("-", "", phy$tip.label[i])
   #   }
   #   colnames(data) <- gsub("-", "", data[1,])
   #   data <- data[-1,]
   # }
  write.csv(data, paste0("./result/", name, "_checked_matrix.csv", sep=""))
  
  comm <- data
  combined <- match.phylo.comm(phy, comm)
  phy<- combined$phy
  comm <- combined$comm
  # Standardized effect size of phylogenetic diversity (Faith's PD) in communities.
  
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
  write.csv(MPD, paste0("result/", name, "_MPD.csv", sep=""))
  write.csv(MNTD, paste0("result/", name, "_MNTD.csv", sep=""))
}



################### PD for 11 sites ######################################
# need three file
# 1. phylogeny
# 2. species list in sampling plot
# 3. output name
# reading tree file
# dir.create("result")
tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
tt <- read.csv("./data/CN_US.site.mtrx_present_absent.csv", header = TRUE)
row.names(tt) <- as.character(tt[,1])
tree$tip.label <- gsub("-", ".", tree$tip.label)
data <- tt
name <- "CNUS11_NW"

#############apply the function######################
PD_caculator(tree, name, data)


#############PD_for_US######################

US.tree <- read.tree("./data/Whole_speciesname_BR_addMay52019.tre")
uu <- read.csv("./data/US.site.mtrx_present_absent.csv", header=TRUE)

row.names(uu) <- as.character(uu[,1])
uu <- uu[,-1]

US.tree$tip.label <- gsub("-", ".", US.tree$tip.label)
data <- uu
name <- "US"

PD_caculator(US.tree, name, data)

###################PD_for_life_form################################

tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
tt <- read.csv("./result/CN_US_LifeForm_checked_matrix.csv", header = TRUE)
tt<- tt[,-1]
tt <- t(tt)
colnames(tt) <- tt[3,]
tt<- tt[-3,]
name <- "CN_US_LifeForm"
tree$tip.label <- gsub("-", ".", tree$tip.label)

PD_caculator(tree, name, tt)

#################PD_for_GA###################
tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
dd <- read.csv("./result/CN_US_GA_matrix.csv", header = TRUE)
dd <- t(dd)
row.names(dd) <- c("Species", "Angiosperms", "Gymnosperm")
colnames(dd) <- dd[1,]
dd<- dd[-1,]
name2 <- "CN2US_GA"
# tree$tip.label <- gsub("-", ".", tree$tip.label)
PD_caculator(tree, name2, dd)

################################CN_US################################################

data <- read.csv("./data/CN_US.2cumtrx_present_absent.csv", header = TRUE)
tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
name <- "CN2USA"
tree$tip.label <- gsub("-", ".", tree$tip.label)
PD_caculator(tree, name, data)

#############PD_for_CN######################

CN.tree <- read.tree("./data/Whole_CN_190510_ed.tre")
uu <- read.csv("./data/CN.cnmtrx_present_absent.csv", header=TRUE)

row.names(uu) <- as.character(uu[,1])
uu <- uu[,-1]

CN.tree$tip.label <- gsub("-", ".", CN.tree$tip.label)
data <- uu
name <- "CN"

PD_caculator(CN.tree, name, data)

#############PD_for_disjunctCN######################
tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
data <- read.csv("./result/CN_US.dismatrx_present_absent.csv", header = TRUE)
name <- "CN2USA_dis"
row.names(data) <- as.character(data[,1])
data <- data[,-1]
tree$tip.label <- gsub("-", ".", tree$tip.label)

PD_caculator(tree, name, data)

#####################CN_dis##############################

CN.tree <- read.tree("./data/Whole_CN_190510_ed.tre")

CN.tree$tip.label <- gsub("-", ".", CN.tree$tip.label)
name <- "CN_dis"
PD_caculator(CN.tree, name, data)

#####################US_dis##############################

US.tree <- read.tree("./data/Whole_US_BR_addMay52019.tre")
US.tree$tip.label <- gsub("-", ".", US.tree$tip.label)
name <- "US_dis"
PD_caculator(US.tree, name, data)

########################Disjuct_11site_##################################
tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
tree$tip.label <- gsub("-", ".", tree$tip.label)

disjuct <- read.csv("./result/CN_US.dismatrx_present_absent.csv", header = TRUE)
disjuct <- t(disjuct)

colnames(disjuct) <- disjuct[1,]
disjuct <- disjuct[-1,]
disjuct <- as.data.frame(disjuct)
disjuct.species <- disjuct %>% mutate(species=row.names(disjuct)) %>% filter(disjun_sp==1) %>% 
  select(species)
disjuct.species <- disjuct.species$species

no.disjuct.species <- disjuct %>% mutate(species=row.names(disjuct)) %>% filter(Non_disjun==1) %>% 
  select(species)

no.disjuct.species <- no.disjuct.species$species

tt <- read.csv("./result/CNUS11_NW_checked_matrix.csv", header = TRUE)

row.names(tt) <- as.character(tt[,1])

tt <- tt[,-1]
tt <- tt[,-1]

aa <- tt[,colnames(tt) %in% disjuct.species]
bb <- tt[,colnames(tt) %in% no.disjuct.species]

name <- "CNUS_11disonly"
PD_caculator(tree, name, aa)

name <- "CNUS_11_NO_dis"
PD_caculator(tree, name, bb)
