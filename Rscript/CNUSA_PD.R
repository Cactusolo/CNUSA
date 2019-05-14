# Phylogentic Diversity (PD) caculation

rm(list=ls())
# install.package("picante"）
library(picante)

# need two file
# 1. phylogeny
# 2. species list in sampling plot

# reading tree file

phy<-read.tree("GTS.tre")

comm<-read.csv("GTSF.csv", header=T, row.names = 1) #the 1sst row of the csv file is header

# checking the name in the phylogeny match with community data sets
combined <- match.phylo.comm(phy, comm)
phy<- combined$phy
comm <- combined$comm

# Computes the cophenetic distances for a hierarchical clustering.

phy.dist <- cophenetic(phy)

# Standardized effect size of phylogenetic diversity (Faith's PD) in communities.
PD <- ses.pd(comm,phy.dist, null.model = "richness", 
                     abundance.weighted = FALSE, runs = 999)


# caculate Standardized effect size of mean pairwise distances (MPD) in communities

MPD <- ses.mpd(comm, phy.dist, null.model = "richness", 
                       abundance.weighted = FALSE, runs = 999)

# caculate Standardized effect size of  mean nearest taxon distances (MNTD) in communities
MNTD <- ses.mntd(comm, phy.dist, null.model = "richness", 
                         abundance.weighted = FALSE, runs = 999)

