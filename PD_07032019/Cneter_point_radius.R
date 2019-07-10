# remotes::install_github("valentinitnelav/geobuffer")
library("geobuffer")
library("sf")
library("rgeos")
library("wellknown")
library("leaflet")
# library("mapview")
# library("ggplot2")
# library("ggmap")
# library("maps")
# remotes::install_github("ropensci/spocc")
library("spocc")
# devtools::install_github("ropensci/spocc")
# WM = data.frame(lon = -71.29, lat = 44.06)
# 
# WM_10km <- geobuffer_pts(xy = data.frame(lon = -71.29, lat = 44.06),
#                                  dist_m = 10*10^3,
#                                  output = "sf")
# WM_10km
# 
# mapView(as(WM_10km, "Spatial"), alpha.regions = 0.2)


# WM_10km.wkt <- st_as_text(WM_10km$geometry[[1]])
# 
# wktview(WM_10km.wkt)
# wellknown::wktview(WM_10km.wkt, center=c(WM$lon, WM$lat), zoom = 10.4)


# WM_10km2 <- geobuffer_pts(xy = data.frame(lon = -71.29, lat = 44.06),
#                          dist_m = 10*10^3,
#                          output = "data.frame")

# New_Hampshire <- get_map("US", zoom=8)
# US <- map_data("usa")
# # New Hampshire
# WM <- map_data("state") %>% filter(region=="new hampshire")
# AIzaSyD8OgXje4n2jUhF7FiDoV5ZwZM1W9le1J8
# ggmap(US)+
#   geom_polygon(data=WM_10km2, aes(x=lon, y=lat), alpha=0.3,colour="red",fill="red")+
#   geom_path(data=WM_10km2, aes(x=lon, y=lat), colour="white",alpha=0.7,size=3)
# 
#   annotate("point",x=7.257885,y=46.79049,size=7)+
#   annotate("text", x=7.257885,y=46.79049,label="Golden Swiss Area",colour="white",size=3)

data<- read.csv("./data/CN_US_sites_geo.csv", header = TRUE)
Sites <- data[,2:4]
Sites <- cbind.data.frame(Sites$Site, Sites$Longitude, Sites$Latitute)
names(Sites) <- c("site", "lon", "lat")



# map_wkt <- function(site){
#   xy <- Sites[Sites$site==site,][,-1]
#   range_10km <- geobuffer_pts(xy, dist_m = 10*10^3, output = "sf")
#   range_10km.wkt <- sf::st_as_text(range_10km$geometry[[1]])
#   #result <- list(Center=xy, Range=range_10km.wkt)
#   # return(range_10km.wkt)
#   site_range_10mk[[site]] <- list(Center=xy, Range=range_10km.wkt)
#   # wktview(range_10km.wkt, center=c(xy$lon, xy$lat), zoom = 10.4)
# }
# map_wkt("Ordway")
# 
# wellknown::wktview(range_10km.wkt, center=c(xy$lon, xy$lat), zoom = 10.4)

site_range_10mk <- list()
for(i in 1:6){
  site <- as.character(Sites$site[i])
  xy <- Sites[i,][,-1]
  range_10km <- geobuffer_pts(xy, dist_m = 10*10^3, output = "sf")
  range_10km.wkt <- sf::st_as_text(range_10km$geometry[[1]])
  site_range_10mk[[site]] <- list(Center=xy, Range=range_10km.wkt)
}

wellknown::wktview(site_range_10mk$Havard$Range, center=c(site_range_10mk$Havard$Center$lon, site_range_10mk$Havard$Center$lat), zoom = 10.4)

wellknown::wktview(site_range_10mk$Mountain_Lake$Range, center=c(site_range_10mk$Mountain_Lake$Center$lon, site_range_10mk$Mountain_Lake$Center$lat), zoom = 10.4)

wellknown::wktview(site_range_10mk$Coweeta$Range, center=c(site_range_10mk$Coweeta$Center$lon, site_range_10mk$Coweeta$Center$lat), zoom = 10.4)

wellknown::wktview(site_range_10mk$Talladega$Range, center=c(site_range_10mk$Talladega$Center$lon, site_range_10mk$Talladega$Center$lat), zoom = 10.4)

saveRDS(site_range_10mk, "Site_range/site_range_10mk.rds")


