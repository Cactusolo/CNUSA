
rm(list=ls())
library("ape")
library("tidyverse")
#species on the tree

tree.us <- read.tree("./AddTaxa_May52019/Whole_speciesname_BR_addMay52019.tre")


site.code <- read_csv("./Site_present_absent/Whole_total_May12019.csv", col_names = FALSE)
names(site.code) <- c("Code", "Species")

S.code <- unique(site.code$Code)
US.names <- c("Coweeta", "Havard", "Mountain_Lake", "Ordway", 
              "Talladega", "White_Mountain")

matrix <- list()
for(i in 1:length(S.code)){
  dd <- site.code %>% group_by(Code) %>% filter(Code==S.code[i])
  ifelse(sum(dd$Species %in% tree.us$tip.label)==length(dd$Species), print("YES"), print(paste(S.code[i], ",", "NO")))
  dat <- as.data.frame(tree.us$tip.label)
  names(dat) <- "Species"
  dat <- mutate(dat, Present=ifelse(Species %in% dd$Species, 1, 0)) %>% arrange(Species)
  
  matrix[[S.code[i]]] <- dat
}

US.site.mtrx <- plyr::join_all(matrix, by="Species", type="left")
names(US.site.mtrx) <- c("Species", US.names)
US.site.mtrx <- t(US.site.mtrx)
# dir.create("result_matrix")
write.csv(US.site.mtrx, "result_matrix/US.site.mtrx_present_absent.csv", col.names=FALSE, quote = FALSE)


####CN_US######

files <-list.files("./matrix/site_species", pattern = "_site.csv$", full.names=TRUE) 

CN.names <- c("Changbai", "Dongling", "Gutian", "Shennong", "Tianmu")

CN_site_list <- list()
for(i in 1:length(files)){
  tt <- read_csv(files[i], col_names = FALSE)
  names(tt) <- c("Species", "Present")
  qq <- tt %>% group_by(Present) %>% filter(Present=="1")
  CN_site_list[[CN.names[i]]] <- qq$Species
  print(paste0(CN.names[i], ",", length(qq$Species)))
}

#US
US.names <- c("Coweeta", "Havard", "Mountain_Lake", "Ordway", 
              "Talladega", "White_Mountain")

S.code <- unique(site.code$Code)
US_site_list <- list()
for(i in 1:length(S.code)){
  dd <- site.code %>% group_by(Code) %>% filter(Code==S.code[i])
  US_site_list[[US.names[i]]] <- dd$Species
  print(paste0(US.names[i], ",", length(dd$Species)))
}

#####CN_US

#reading tree
tree.cn_us <- read.tree("./CN_US_speciesname_BR_addMay52019.tre")

#species list for each site
sites.11 <- c(CN_site_list, US_site_list)

mm <- list()
for(i in 1:length(sites.11)){
  Species.s <- as.character(sites.11[[i]])
  name <- names(sites.11[i])
  ifelse(sum(Species.s %in% tree.cn_us$tip.label)==length(Species.s), print("YES"), print(paste(names(sites.11[i]), ",", "NO")))
  dat <- as.data.frame(tree.cn_us$tip.label)
  names(dat) <- "Species"
  dat <- mutate(dat, Present=ifelse(tree.cn_us$tip.label %in% Species.s, 1, 0)) %>% arrange(Species)
  
  mm[[name]] <- dat
}

CN_US.site.mtrx <- plyr::join_all(mm, by="Species", type="left")
names(CN_US.site.mtrx) <- c("Species", names(sites.11))
CN_US.site.mtrx <- t(CN_US.site.mtrx)
# dir.create("result_matrix")
write.csv(CN_US.site.mtrx, "result_matrix/CN_US.site.mtrx_present_absent.csv", col.names=FALSE, quote = FALSE)


####CN_US matrix
US_species <- tree.us$tip.label
CN_US_species <- tree.cn_us$tip.label
CN_species <- NULL

for(i in 1:length(CN_site_list)){
  CN_species <- c(CN_species, CN_site_list[[i]])
}

CN_species <- unique(CN_species)

site.cn_us <- list(China=CN_species, USA=US_species)

cumtrx <- list()
for(i in 1:length(site.cn_us)){
  Species.s <- as.character(site.cn_us[[i]])
  name <- names(site.cn_us[i])
  ifelse(sum(Species.s %in% CN_US_species)==length(Species.s), print("YES"), print(paste(name, ",", "NO")))
  dat <- as.data.frame(CN_US_species)
  names(dat) <- "Species"
  dat <- mutate(dat, Present=ifelse(CN_US_species %in% Species.s, 1, 0)) %>% arrange(Species)
  
  cumtrx[[name]] <- dat
}

CN_US.cumtrx <- plyr::join_all(cumtrx, by="Species", type="left")

names(CN_US.cumtrx) <- c("", names(site.cn_us))
CN_US.cumtrx <- t(CN_US.cumtrx)
colnames(CN_US.cumtrx) <- CN_US.cumtrx[1,]
CN_US.cumtrx <- CN_US.cumtrx[-1,]
# dir.create("result_matrix")
write.csv(CN_US.cumtrx, "result_matrix/CN_US.2cumtrx_present_absent.csv", quote = FALSE)


######################CN_matrix#############################################
CN.tree <- read.tree("../PD_05072019/data/Whole_CN_190510.tre")
CN.tree$tip.label[6] <-  "Taxodium_distichum_var._imbricatum"
CN.tree$tip.label[1727] <- "Erigeron_canadensis"
CN.tree$tip.label[1712] <- "Gnaphalium_pensylvanicum"
CN.tree$tip.label[2817] <- "Alisma_plantago-aquatica"
CN.tree$tip.label[2237] <-  "Orthilia_secunda"
write.tree(CN.tree, "../PD_05072019/data/Whole_CN_190510_ed.tre")

CN_site_list[[1]] <- unique(CN_site_list[[1]])

cnmtrx <- list()
for(i in 1:length(CN_site_list)){
  Species.s <- as.character(CN_site_list[[i]])
  name <- names(CN_site_list[i])
  ifelse(sum(Species.s %in% CN.tree$tip.label)==length(Species.s), print("YES"), print(paste(name, ",", "NO")))
  dat <- as.data.frame(CN.tree$tip.label)
  names(dat) <- "Species"
  dat <- mutate(dat, Present=ifelse(CN.tree$tip.label %in% Species.s, 1, 0)) %>% arrange(Species)
  
  cnmtrx[[name]] <- dat
}

CN.cnmtrx <- plyr::join_all(cnmtrx, by="Species", type="left")

names(CN.cnmtrx) <- c("", names(CN_site_list))
CN.cnmtrx <- t(CN.cnmtrx)
colnames(CN.cnmtrx) <- CN.cnmtrx[1,]
CN.cnmtrx <- CN.cnmtrx[-1,]
# dir.create("result_matrix")
write.csv(CN.cnmtrx, "result_matrix/CN.cnmtrx_present_absent.csv", quote = FALSE)
write.csv(CN.cnmtrx, "../PD_05072019/data/CN.cnmtrx_present_absent.csv", quote = FALSE)

#####################Disjunct_matrix##############################

tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
CN_US_species <- tree$tip.label

disjun_sp <- read_csv("./data/disju_sp_395.txt", col_names = F)
disjun_sp <- disjun_sp$X1
Non.disjun <- setdiff(CN_US_species, disjun_sp)

d.list <- list("disjun_sp"=disjun_sp, "Non_disjun"=Non.disjun)

cumtrx <- list()
for(i in 1:length(list)){
  Species.s <- as.character(d.list[[i]])
  name <- names(d.list[i])
  ifelse(sum(Species.s %in% CN_US_species)==length(Species.s), print("YES"), print(paste(name, ",", "NO")))
  dat <- as.data.frame(CN_US_species)
  names(dat) <- "Species"
  dat <- mutate(dat, Present=ifelse(CN_US_species %in% Species.s, 1, 0)) %>% arrange(Species)
  
  cumtrx[[name]] <- dat
}

CN_US.dismatrx <- plyr::join_all(cumtrx, by="Species", type="left")

names(CN_US.dismatrx) <- c("", names(d.list))

CN_US.dismatrx <- t(CN_US.dismatrx)
colnames(CN_US.dismatrx) <- CN_US.dismatrx[1,]
CN_US.dismatrx <- CN_US.dismatrx[-1,]
# dir.create("result_matrix")
write.csv(CN_US.dismatrx, "result/CN_US.dismatrx_present_absent.csv", quote = FALSE)

########################US_CN_11_site####################################
tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
CN_US_species <- tree$tip.label

site.cn_us <- read.csv("./result/CNUS11_NW_checked_matrix.csv", header = TRUE)
row.names(site.cn_us) <- site.cn_us[,2]
site.cn_us <- site.cn_us[,-2]
site.cn_us <- site.cn_us[,-1]

lifeform <- read.csv("./result/CN_US_LifeForm_checked_matrix.csv", header = TRUE)
lifeform <- t(lifeform)
lifeform  <- as.data.frame(lifeform)
names(lifeform) <- as.character(unlist(lifeform[1,]))
lifeform <- lifeform[-1,]
# woody <- names(lifeform[which(lifeform$Woody == 1),][,1])
# herb <- names(lifeform[which(lifeform$Herb == 1),][,2])

trouble <- lifeform %>% mutate(Species=row.names(lifeform)) %>%  filter(Woody==0 & Herb==0)
no.trouble <- lifeform %>% mutate(Species=row.names(lifeform)) %>%  filter(Woody==1 | Herb==1)
# write.csv(trouble, "./result/species_noherb_nowoody.csv", quote = F)
other.data <- read.csv("./data/species_noherb_nowoody_Hanyang.csv", header = TRUE, stringsAsFactors = F)
other.data <- other.data[,-1]

lifeform.new <- rbind.data.frame(no.trouble, other.data)

write.csv(lifeform.new, "./result/CN_US_LifeForm_checked_matrix.csv", quote = FALSE)

lifeform <- lifeform.new
woody <- lifeform[which(lifeform$Woody == 1),]$Species
herb <- lifeform[which(lifeform$Herb == 1),]$Species


lfmtrx <- list()
for(i in 1:length(row.names(site.cn_us))){
  Species.s <- names(site.cn_us[i,which(site.cn_us[i,]==1)])

  name <- row.names(site.cn_us)[i]
  ifelse(sum(c(Species.s %in% woody, Species.s %in% herb))==length(Species.s), print("YES"), print(paste(name, ",", "NO")))
  dat <- as.data.frame(Species.s)
  names(dat) <- "Species"
  dat <- mutate(dat, Woody=ifelse(Species.s %in% woody, 1, 0), Herb=ifelse(Species.s %in% herb, 1, 0)) %>% arrange(Species)
  
  lfmtrx[[name]] <- dat
}

saveRDS(lfmtrx, "./result/CN_US_11site_WH.rds")
