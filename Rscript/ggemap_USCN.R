rm(list=ls())
library(maps)
library(ggplot2)
library(tidyverse)
library(gridExtra)
world_map <- map_data("world")

US <- subset(world_map, world_map$region == "USA")
US <-  map_data("usa")
CN <-  subset(world_map, world_map$region == "China")


base_US_messy <- p + geom_polygon(aes(x=long, y=lat, group=group), fill = 'blue', alpha=0.4, 
                                     color = 'blue', data = US)

base_CN_messy <- p + geom_polygon(aes(x=long, y=lat, group=group), fill = 'red', alpha=0.4, 
                                  color = 'red', data = CN)

grid.arrange(base_CN_messy, base_US_messy, widths = c(1,1.3), nrow=1, ncol=2)

site <- read.csv("./data/CN_US_sites_geo.csv", header = TRUE)
site.cn <- site %>% filter(country=="China")
# site %>% glimpse()
site.CN <- left_join(CN, site.cn, by = c('region' = 'country'))

map_cnpd_sized <- base_CN_messy +
  geom_point(data=site.CN, aes(x=Longitude, y=Latitute, size=SES.PD, group=Site), colour="blue", 
             fill="NA", pch=21) +
  theme(legend.position = "none")

site.us <- site %>% filter(country=="USA")
site.US <- left_join(US, site.us, by = c('region' = 'country'))

map_uspd_sized <- base_US_messy +
  geom_point(data=site.US, aes(x=Longitude, y=Latitute, size=SES.PD, group=Site), colour="red", 
             fill="NA", pch=21) +
  theme(legend.position = "none")

grid.arrange(map_cnpd_sized, map_uspd_sized, widths = c(1,1.3), nrow=1, ncol=2)
# US_CN <- map_data("world") %>% 
#   filter(region == c("China", "USA"))
# 
# US_CN.fort <- fortify(US_CN, region = c("China", "USA"))
# 
# p <- ggplot() + coord_fixed() + xlab("") + ylab("")
# 
# base_world_messy <- p + geom_polygon(aes(x=long, y=lat, group=region), fill = 'light green', alpha=0.4, 
#                color = 'light green', data = US_CN.fort) +
#   facet_wrap( ~ region, nrow = 2) 
# 
# base_world_messy
