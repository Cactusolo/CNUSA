library(ggplot2)
library(ggmap)
library(maps)
library(tidyverse)

library("ratser")
library("rgdal")
library("rgeos")

library("sampSurf")
# install.packages("wicket")
library("sp")
library("wicket")
library("spocc")
devtools::install_github("valentinitnelav/geobuffer")

wm=data.frame(lat=44.06, lon=-71.29,ID=1)
coordinates(wm)=~lon+lat
proj4string(wm)=CRS("+init=epsg:4326")

buf = gBuffer(wm, width=10000, joinStyle="MITRE")
plot(buf)

pc <- spTransform(wm, CRS("+init=epsg:4326")) 


pc10km <- gBuffer( pc, width=10000, byid=TRUE )
# Add data, and write to shapefile
pc100km <- SpatialPolygonsDataFrame( pc100km, data=pc100km@data )
writeOGR( pc100km, "pc100km", "pc100km", driver="ESRI Shapefile" )


# data("wrld_simpl", package="maptools")

wm <- spCircle(10000, spUnits = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), centerPoint = c(x=-71.29, y=44.06), spID="WM")

wm_wkt <- sp_convert(wm$spCircle, group=TRUE)

wkt_vis(wm_wkt, zoom=10)


library("wellknown")
library("leaflet")
wktview(wm_wkt)


US <- map_data("usa")
# New Hampshire
WM <- map_data("state") %>% filter(region=="new hampshire")

ggmap(US)+
  geom_polygon(data=mydata,aes(x=Longitude,y=Latitude),alpha=0.3,colour="red",fill="red")+
  geom_path(data=mydata,aes(x=Longitude,y=Latitude),colour="white",alpha=0.7,size=3)+
  annotate("point",x=7.257885,y=46.79049,size=7)+
  annotate("text", x=7.257885,y=46.79049,label="Golden Swiss Area",colour="white",size=3)