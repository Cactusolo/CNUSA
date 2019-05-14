rm(list=ls())

library("ape")
library("taxonlookup")
library("tidyverse")

#read tree
tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")

species <- tree$tip.label

#checking species taxonomic rank
table <- lookup_table(species, by_species = TRUE, missing_action="NA")
sum(is.na(table))
# 72
# check_name <- table[!complete.cases(table),]
# example <- rbind.data.frame(tail(table, 2), head(table, 2))
# 
# check_name <- rbind.data.frame(example, check_name)
# write.csv(check_name, "./result/Hanyang_check_name.csv", quote = FALSE)
# 

##good table
good.table <-  table[complete.cases(table),]
name_checked <- read.csv("./result/Hanyang_check_name_190508.csv", header = TRUE)
name_checked <- name_checked[5:28, 1:5]



good.table <- data.frame(Species=row.names(good.table), good.table)
good.table <- good.table[,-6]
row.names(good.table) <- NULL


colnames(name_checked) <- colnames(good.table)
combine_table <- rbind.data.frame(good.table, name_checked)
combine_table <- as.data.frame(combine_table)
# write.csv(combine_table, "./result/CN_US_taxonomy.csv", row.names = FALSE, quote=FALSE)
combine_table <- read_csv("./result/CN_US_taxonomy.csv", col_names = TRUE)
setdiff(species, combine_table$Species)

GA_table <- mutate(combine_table, Gymnosperms=ifelse(group %in% "Gymnosperms", 1, 0), 
                   Angiosperms=ifelse(group %in% "Angiosperms", 1, 0)) %>% select(Species, Gymnosperms, Angiosperms)


write.csv(GA_table, "./result/CN_US_GA_matrix.csv", row.names = FALSE, quote=FALSE)

####woody&herb
WH <- read_csv("../matrix_update/W_H/CN_US_species_WH_5608.csv", col_names = TRUE)

setdiff(WH$Species,tree$tip.label)

WH_table <- mutate(WH, Woody=ifelse(Life_Form %in% "W", 1, 0), 
                   Herb=ifelse(Life_Form %in% "H", 1, 0)) %>% select(Species, Woody, Herb)

write.csv(WH_table, "./result/CN_USA_WH_matrix.csv", row.names = FALSE, quote=FALSE)

######US_CN

tree <- read.tree("./data/CN_US_speciesname_BR_addMay52019.tre")
data <- read_csv("./data/CN_US.site.mtrx_present_absent.csv", col_names = TRUE)
data2 <- data %>% group_by(Species) %>% filter(Species=="Changbai")
Changbai.sp <- names(data2[which(data2[1,]==1)])

Changbai_list <- read.csv("./data/Changbai.txt", header=FALSE)
Changbai_list <- Changbai_list$V1
