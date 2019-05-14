rm(list=ls())
library("picante")
library("phytools")
library("cluster")
library("phyr")
# install.packages("https://raw.githubusercontent.com/daijiang/phyr/master/phyr_0.1.5.zip", repos = NULL)
# devtools::install_github("daijiang/phyr")
# install.packages("https://raw.githubusercontent.com/daijiang/phyr/master/phyr_0.1.5.tgz", repos = NULL)
PD_caculator <- function(data, tree, name){
  comm <- NULL
  if(is.ultrametric(tree)!=TRUE){
    tree <- force.ultrametric(tree, method="extend")
  }
  phy <- tree
  comm <- data
  combined <- match.phylo.comm(phy, comm)
  phy<- combined$phy
  comm <- combined$comm
  phy.dist <- cophenetic(phy)
  
  # comdist
  #Calculates mean pairwise distance (MPD) separating taxa in two communities
  #a measure of phylogenetic beta diversity
  
  cat("Calculates inter-community mean pairwise distance --- comdist ...\n")
  comdist.result <- comdist(comm, phy.dist)
  comdist.clusters <- hclust(comdist.result)
  
  cat("Calculates inter-community mean nearest taxon distance --- comdistnt  ...\n")
  comdistnt.result <- comdistnt(comm, phy.dist)
  comdistnt.clusters <- hclust(comdistnt.result)
  
  cat("Calculates Phylogenetic Community Dissimilarity --- pcd  ...\n")
  pcd.result <- phyr::pcd(comm, tree)
  
  cat("Calculates Unweighted UniFrac distance between communities --- UniF ...\n")
  Unif <- unifrac(comm, tree)
  
  pdf(paste0("result/Beta_diversity/", name, "beta_diversity_plot.pdf"), height = 6, width = 4)
  # par(mfrow = c(1, 2))
  plot(comdist.clusters, main=paste0(name, " sites"), sub="MPD_beta", xlab="")
  # axis(2, at = c(0, axTicks(2)), cex.axis = 0.8, mgp = c(3, 0.7, 0.5))
  # plot(as.dendrogram(comdistnt.clusters), horiz=TRUE, main="CN-US 11 sites mean nearest taxon distance", sub="Beta Diversity", xlab="")
  plot(comdistnt.clusters, main=paste0(name," sites"), sub="MNTD_beta", xlab="")

dev.off()

  #write out table
  write.table(as.matrix(comdist.result), paste0("./result/Beta_diversity/", name, "_comdist.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(comdistnt.result), paste0("result/Beta_diversity/", name, "_comdistnt.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(pcd.result$PCD), paste0("result/Beta_diversity/", name, "_PCD_matrix.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(pcd.result$PCDc), paste0("result/Beta_diversity/", name, "_PCDc_matrix.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(pcd.result$PCDp), paste0("result/Beta_diversity/", name, "_PCDp_matrix.csv", sep=""), quote = FALSE, sep = ",")
  # write.table(as.matrix(pcd.result$PSVmncd), paste0("result/Beta_diversity/", name, "_PSVmncd_matrix.csv", sep=""), quote = FALSE, sep = ",")
  # write.table(pcd.result$PSVpool, paste0("result/Beta_diversity/", name, "_PSVpool_matrix.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(Unif), paste0("result/Beta_diversity/", name, "_UniFrac.csv", sep=""), quote = FALSE, sep = ",")
}

# dir.create("result/Beta_diversity")

tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
tree$tip.label <- gsub("-", ".", tree$tip.label)
data <- read.csv("./result/CNUS11_NW_checked_matrix.csv", header = TRUE)
row.names(data) <- data[,1]
data <- data[,-1]
name <- "CNUS_11"

PD_caculator(data, tree, name)

#US_whole
tree <- read.tree("./data/Whole_US_BR_addMay52019.tre")
tree$tip.label <- gsub("-", ".", tree$tip.label)
data <- read.csv("./result/US_checked_matrix.csv", header = TRUE)
row.names(data) <- data[,1]
data <- data[,-1]
name <- "US_6"
PD_caculator(data, tree, name)

#CN_whole
CN.tree <- read.tree("./data/Whole_CN_190510_ed.tre")
CN.tree$tip.label <- gsub("-", ".", CN.tree$tip.label)
uu <- read.csv("./data/CN.cnmtrx_present_absent.csv", header=TRUE)

row.names(uu) <- as.character(uu[,1])
uu <- uu[,-1]
data <- uu
name <- "CN_5"
tree <- CN.tree
PD_caculator(data,CN.tree, name)


