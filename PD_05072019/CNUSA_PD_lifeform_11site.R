# Phylogentic Diversity (PD) caculation

rm(list=ls())
# install.package("picante"）
library("picante")
library("phytools")

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
  write.csv(data, paste0("result/lifeform_11site/", name, "_checked_matrix.csv", sep=""))
  
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
  write.csv(PD, paste0("result/lifeform_11site/", name, "_PD.csv", sep=""))
  write.csv(MPD, paste0("result/lifeform_11site/", name, "_MPD.csv", sep=""))
  write.csv(MNTD, paste0("result/lifeform_11site/", name, "_MNTD.csv", sep=""))
}


#######################life_form_11sites##################

tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
tree$tip.label <- gsub("-", ".", tree$tip.label)

lifeform <- readRDS("./result/CN_US_11site_WH.rds")

for (i in 1:length(lifeform)){
  xx <- lifeform[[i]]
  xx <- t(xx)
  colnames(xx) <- as.character(xx[1,])
  xx <- xx[-1,]
  
  name <- names(lifeform[i])
  
  print(name)
  
  PD_caculator(tree, name, xx)
}





