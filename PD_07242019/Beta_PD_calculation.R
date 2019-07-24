rm(list=ls())
library("picante")
library("phytools")
library("cluster")
library("phyr")
library("pheatmap")
# install.packages("https://raw.githubusercontent.com/daijiang/phyr/master/phyr_0.1.5.zip", repos = NULL)
# devtools::install_github("daijiang/phyr")
# install.packages("https://raw.githubusercontent.com/daijiang/phyr/master/phyr_0.1.5.tgz", repos = NULL)

PD_beta_caculator <- function(data, tree, name){
  comm <- NULL
  if(!is.ultrametric(tree)){
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
  # comdist.clusters <- hclust(comdist.result)
  
  cat("Calculates inter-community mean nearest taxon distance --- comdistnt  ...\n")
  comdistnt.result <- comdistnt(comm, phy.dist)
  # comdistnt.clusters <- hclust(comdistnt.result)
  
  cat("Calculates Phylogenetic Community Dissimilarity --- pcd  ...\n")
  pcd.result <- phyr::pcd(comm, tree)
  
  cat("Calculates Unweighted UniFrac distance between communities --- UniF ...\n")
  Unif <- unifrac(comm, tree)
  
  
  #write out table
  write.table(as.matrix(comdist.result), paste0("./result/Beta_diversity/", name, "_comdist.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(comdistnt.result), paste0("./result/Beta_diversity/", name, "_comdistnt.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(pcd.result$PCD), paste0("./result/Beta_diversity/", name, "_PCD_matrix.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(pcd.result$PCDc), paste0("./result/Beta_diversity/", name, "_PCDc_matrix.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(pcd.result$PCDp), paste0("./result/Beta_diversity/", name, "_PCDp_matrix.csv", sep=""), quote = FALSE, sep = ",")
  # write.table(as.matrix(pcd.result$PSVmncd), paste0("result/Beta_diversity/", name, "_PSVmncd_matrix.csv", sep=""), quote = FALSE, sep = ",")
  # write.table(pcd.result$PSVpool, paste0("result/Beta_diversity/", name, "_PSVpool_matrix.csv", sep=""), quote = FALSE, sep = ",")
  write.table(as.matrix(Unif), paste0("result/Beta_diversity/", name, "_UniFrac.csv", sep=""), quote = FALSE, sep = ",")
}

##### import tree ##### 
tree <- read.tree("./data/CN_US_speciesname_BR_addJul192019_ad.tre")
tree$tip.label <- gsub("-", ".", tree$tip.label)

##### 11 sites total  ##### 
data <- read.csv("./result/CN_US_11_sites_checked_matrix.csv", header = TRUE)
row.names(data) <- data[,1]
data <- data[,-1]
name <- "CN_US_11"

PD_beta_caculator(data, tree, name)

# plot
PCD <- read.csv(paste0("result/Beta_diversity/", name, "_PCD_matrix.csv", sep=""), header=T)
Unif <- read.csv(paste0("result/Beta_diversity/", name, "_UniFrac.csv", sep=""), header=T)

pdf(paste0("result/Beta_diversity/", name, "beta_diversity_plot.pdf"), height = 6, width = 6)

pheatmap(as.matrix(PCD), display_numbers = T, number_format = "%.4f", fontsize = 10, main="PCD")

pheatmap(as.matrix(Unif), display_numbers = T, number_format = "%.4f", fontsize = 10, main="Unif")

dev.off()

##### woody and herb ##### 
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

name <- "W_11_sites"
PD_beta_caculator(Woody, tree, name)

name <- "H_11_sites"
PD_beta_caculator(Herb, tree, name)






##### dis and non_dis ##### 
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
PD_beta_caculator(dis, tree, name)

name <- "Non_dis_11_sites"
PD_beta_caculator(non.dis, tree, name)



