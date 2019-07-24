setwd("Desktop/CNUSA/Species_list_190715/")
rm(list = ls())
library(dplyr)
CNUS_11 <- read.csv("./data/CNUS11_NW_checked_matrix.csv",header = T)
transformed <- t(CNUS_11)
Changbai <- subset(transformed,transformed[,1]==1)
Changbai <- as.data.frame(Changbai)
write.csv(rownames(Changbai),"./results/CBS_species.csv",row.names = F,quote = F)

Dongling <- subset(transformed,transformed[,2]==1)
Dongling <- as.data.frame(Dongling)
write.csv(rownames(Dongling),"./results/DLS_species.csv",row.names = F,quote = F)

Gutian <- subset(transformed,transformed[,3]==1)
Gutian <- as.data.frame(Gutian)
write.csv(rownames(Gutian),"./results/GTS_species.csv",row.names = F,quote = F)

Shennong <- subset(transformed,transformed[,4]==1)
Shennong <- as.data.frame(Shennong)
write.csv(rownames(Shennong),"./results/SNJ_species.csv",row.names = F,quote = F)

Tianmu <- subset(transformed,transformed[,5]==1)
Tianmu <- as.data.frame(Tianmu)
write.csv(rownames(Tianmu),"./results/TMS_species.csv",row.names = F,quote = F)