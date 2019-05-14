library(maps)
library(ggplot2)
library(tidyverse)
world_map <- map_data("world")
world_map <- subset(world_map, world_map$long< 150 & world_map$long >-150 & world_map$lat<55 & world_map$lat > 15)
site <- read.csv("./data/CN_US_sites_geo.csv", header = TRUE)
# site %>% glimpse()
site.map <- left_join( world_map, site, by = c('region' = 'country'))
site.map$Site <- as.character(site.map$Site)
#Creat a base plot with gpplot2
# p <- ggplot() + coord_fixed() +
#   xlab("") + ylab("")

p <- ggplot(site.map , aes( x = long, y = lat, group = group )) + 
  coord_fixed() + xlab("") + ylab("")
#Add map to base plot
base_world_messy <- p + geom_polygon(aes(fill=SES.PD))
  # scale_color_manual(values = c('1' = 'red', '0' = NA)) +
  # theme(legend.title = element_blank())
  # theme(legend.position = "bottom")


# base_world_messy <- p + geom_polygon(data=US_CN, aes(x=long, y=lat, group=region, colour=region, fill=region), 
#                                      colour="black") +
#   scale_color_manual(values = c("oragne", "blue"))


# base_world_messy

#Strip the map down so it looks super clean (and beautiful!)
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

# base_world

map_data_sized <- base_world +
  geom_point(data=site.map, aes(x=Longitude, y=Latitute, size=SES.PD, group=Site), colour="Deep Pink", 
             fill="light pink", pch=21, alpha=I(0.01)) +
  annotate("text", label = "White_Mountain", x =-61.29,y = 46.06,size =2.5, colour = "red") +
  annotate("text", label = "Havard", x =-65.17,y = 42.54,size =2.5, colour = "red") +
  annotate("text", label = "Mountain_Lake", x =-72.52,y = 38.38,size =2.5, colour = "red") +
  annotate("text", label = "Coweeta", x =-75.45,y = 35.05,size =2.5, colour = "red") +
  annotate("text", label = "Talladega", x =-80.42,y = 32.92,size =2.5, colour = "red") +
  annotate("text", label = "Ordway", x =-76.99,y = 29.69,size =2.5, colour = "red") +
  annotate("text", label = "Changbai", x =128,y = 41.06,size =2.5, colour = "red") +
  annotate("text", label = "Dongling", x =115.5,y = 38.93,size =2.5, colour = "red") +
  annotate("text", label = "Shennong", x =110.31,y = 30.49,size =2.5, colour = "red") +
  annotate("text", label = "Tianmu", x =119.44,y = 32.00,size =2.5, colour = "red") +
  annotate("text", label = "Gutian", x =118.13,y = 28.24,size =2.5, colour = "red") 
map_data_sized
########################species richness###########################################
map_species_sized <- base_world +
  geom_point(data=site.map, aes(x=Longitude, y=Latitute, size=Species, group=Site), colour="green", 
             fill="light green", pch=21, alpha=I(0.01)) #+
  # annotate("text", label = "White_Mountain", x =-61.29,y = 46.06,size =2.5, colour = "red") +
  # annotate("text", label = "Havard", x =-65.17,y = 42.54,size =2.5, colour = "red") +
  # annotate("text", label = "Mountain_Lake", x =-72.52,y = 38.38,size =2.5, colour = "red") +
  # annotate("text", label = "Coweeta", x =-75.45,y = 35.05,size =2.5, colour = "red") +
  # annotate("text", label = "Talladega", x =-80.42,y = 32.92,size =2.5, colour = "red") +
  # annotate("text", label = "Ordway", x =-76.99,y = 29.69,size =2.5, colour = "red") +
  # annotate("text", label = "Changbai", x =128,y = 41.06,size =2.5, colour = "red") +
  # annotate("text", label = "Dongling", x =115.5,y = 38.93,size =2.5, colour = "red") +
  # annotate("text", label = "Shennong", x =110.31,y = 30.49,size =2.5, colour = "red") +
  # annotate("text", label = "Tianmu", x =119.44,y = 32.00,size =2.5, colour = "red") +
  # annotate("text", label = "Gutian", x =118.13,y = 28.24,size =2.5, colour = "red") 
map_species_sized

########################NRI###########################################
map_NRI_sized <- base_world +
  geom_point(data=site.map, aes(x=Longitude, y=Latitute, size=NRI, group=Site), colour="orange", 
             fill="light yellow", pch=21, alpha=I(0.01)) #+
# annotate("text", label = "White_Mountain", x =-61.29,y = 46.06,size =2.5, colour = "red") +
# annotate("text", label = "Havard", x =-65.17,y = 42.54,size =2.5, colour = "red") +
# annotate("text", label = "Mountain_Lake", x =-72.52,y = 38.38,size =2.5, colour = "red") +
# annotate("text", label = "Coweeta", x =-75.45,y = 35.05,size =2.5, colour = "red") +
# annotate("text", label = "Talladega", x =-80.42,y = 32.92,size =2.5, colour = "red") +
# annotate("text", label = "Ordway", x =-76.99,y = 29.69,size =2.5, colour = "red") +
# annotate("text", label = "Changbai", x =128,y = 41.06,size =2.5, colour = "red") +
# annotate("text", label = "Dongling", x =115.5,y = 38.93,size =2.5, colour = "red") +
# annotate("text", label = "Shennong", x =110.31,y = 30.49,size =2.5, colour = "red") +
# annotate("text", label = "Tianmu", x =119.44,y = 32.00,size =2.5, colour = "red") +
# annotate("text", label = "Gutian", x =118.13,y = 28.24,size =2.5, colour = "red") 
map_NRI_sized

########################NTI###########################################
map_NTI_sized <- base_world +
  geom_point(data=site.map, aes(x=Longitude, y=Latitute, size=NTI, group=Site), colour="orange", 
             fill="orange", pch=21, alpha=I(0.01)) #+
# annotate("text", label = "White_Mountain", x =-61.29,y = 46.06,size =2.5, colour = "red") +
# annotate("text", label = "Havard", x =-65.17,y = 42.54,size =2.5, colour = "red") +
# annotate("text", label = "Mountain_Lake", x =-72.52,y = 38.38,size =2.5, colour = "red") +
# annotate("text", label = "Coweeta", x =-75.45,y = 35.05,size =2.5, colour = "red") +
# annotate("text", label = "Talladega", x =-80.42,y = 32.92,size =2.5, colour = "red") +
# annotate("text", label = "Ordway", x =-76.99,y = 29.69,size =2.5, colour = "red") +
# annotate("text", label = "Changbai", x =128,y = 41.06,size =2.5, colour = "red") +
# annotate("text", label = "Dongling", x =115.5,y = 38.93,size =2.5, colour = "red") +
# annotate("text", label = "Shennong", x =110.31,y = 30.49,size =2.5, colour = "red") +
# annotate("text", label = "Tianmu", x =119.44,y = 32.00,size =2.5, colour = "red") +
# annotate("text", label = "Gutian", x =118.13,y = 28.24,size =2.5, colour = "red") 
map_NTI_sized