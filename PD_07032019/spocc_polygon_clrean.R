rm(list=ls())

library("geobuffer")
library("sf")
library("rgeos")
library("wellknown")
library("leaflet")
library("spocc")
library("tidyverse")

dat<- read.csv("./data/CN_US_sites_geo.csv", header = TRUE, stringsAsFactors = F)
Sites <- dat[,2:4]
Sites <- cbind.data.frame(Sites$Site, Sites$Longitude, Sites$Latitute)
names(Sites) <- c("site", "lon", "lat")

# saveRDS(site_range_10mk, "Site_range/site_range_10mk.rds")

#read in wkt polygon exported from Qgis
White_Mountain <- "MULTIPOLYGON(((-71.29029932158805 44.07616377565827,-71.28856944099745 44.07665802725559,-71.28865181626367 44.074763396132546,-71.28799281413391 44.074763396132546,-71.28832231519878 44.07402201873658,-71.28659243460818 44.073692517671695,-71.28618055827708 44.070974133886466,-71.28823993993257 44.07064463282159,-71.28823993993257 44.070068005958056,-71.28412117662161 44.07039750702293,-71.2840388013554 44.0698208801594,-71.28288554762833 44.06998563069183,-71.28288554762833 44.068997127497205,-71.28033191437552 44.068997127497205,-71.28033191437552 44.06454886312137,-71.25388945391916 44.06446648785515,-71.25265382492587 44.0591120955509,-71.2561959613733 44.0591120955509,-71.2587495946261 44.05589946016835,-71.25866721935988 44.05276920005202,-71.2631154837357 44.049968441000566,-71.26122085261267 44.049309438870814,-71.2604794752167 44.04873281200728,-71.26204460527487 44.04650867981936,-71.2656691169885 44.04848568620862,-71.26706949651422 44.04782668407887,-71.26912887816971 44.04016578432048,-71.27184726195495 44.038024027398784,-71.27225913828605 44.03481139201623,-71.27341239201311 44.03431714041892,-71.27662502739565 44.03505851781489,-71.27728402952542 44.03654127260684,-71.28173229390124 44.035799895210864,-71.28057904017417 44.03398763935404,-71.28115566703771 44.03201063296478,-71.28634530880953 44.031598756633684,-71.2909583237178 44.03258725982831,-71.29334720643816 44.03093975450393,-71.29507708702876 44.031763507166126,-71.29985485246948 44.03629414680818,-71.31287014453211 44.037365025269025,-71.31698890784307 44.042225165975964,-71.32003679269317 44.04296654337193,-71.32242567541354 44.04568492715717,-71.31913066476477 44.05103931946142,-71.31130501447394 44.05145119579251,-71.31509427672003 44.05499333223994,-71.32110767115402 44.05598183543457,-71.32036629375806 44.06248948146589,-71.30932800808468 44.06669062004307,-71.30562112110482 44.064631238387584,-71.29244107850974 44.065043114718684,-71.29433570963279 44.072127387613534,-71.29128782478267 44.07146838548378,-71.2922763279773 44.07525764772986,-71.2901345710556 44.075010521931205,-71.29029932158805 44.07616377565827)))"

# dir.create("result")
# pdf("result/White_Mountain_polygon.pdf", height = 6.5, width = 6.5)
wellknown::wktview(White_Mountain, center=c(Sites$lon[1], Sites$lat[1]), zoom = 10.4)
# dev.off()

Mountain_Lake <- "MULTIPOLYGON(((-80.52762413623942 37.37913451685481,-80.51511425590691 37.37698227937825,-80.51760278048918 37.35991570876333,-80.52701881944914 37.35355988246536,-80.52785953721342 37.37087866840957,-80.525539156184 37.37284594797798,-80.52920468563627 37.376343333877394,-80.52762413623942 37.37913451685481)))"

wellknown::wktview(Mountain_Lake, center=c(Sites$lon[3], Sites$lat[3]), zoom = 12.4)


# the focal plant species 
# Tracheophyta
# 松柏门 Coniferophyta
# 苏铁门 Cycadophyta
# 银杏门 Ginkgophyta
# 买麻藤门 Gnetophyta
# 被子植物门 Magnoliophyta

# plants <- c("Coniferophyta", "Cycadophyta", "Ginkgophyta", "Gnetophyta", "Magnoliophyta")

#Define a function, using polygon to query gbif, and only return plants
# occurred inside the polygone
get_data_gbif <- function(site, N, polygone){
  data <- NULL
  # if the records >500 (limite),chop query by 500
  if(N >500){
    pool <- seq.int(1,N, 400)
    for(i in 1:round(N/400,0)){
      if(i==1){
        res <- occ(query="Tracheophyta", geometry = polygone, from = "gbif", limit = 400)
        data <- res$gbif$data[[1]]
        # next
      }else if(i>1){
        res <- occ(query="Tracheophyta", geometry = polygone, from = "gbif", limit = 400, start=pool[i])
        data <- bind_rows(data, res$gbif$data[[1]])
        # print(pool[i])
      }
    }  
    
  } else{ #if the records <500 (limite),direct query
    res <- occ(query="Tracheophyta", geometry = polygone, from = "gbif")
    data <- res$gbif$data[[1]]
  }

  #save R data
  saveRDS(data, paste0("result/", site, "_gbif_raw_Tracheophyta.rds", sep=""))
  #all records
  data.res <- data %>% select(name,longitude,latitude,scientificName,order,family,genus,species,genericName,specificEpithet,country)
  write.csv(data.res, paste0("result/", site, "_gbif_occdata.csv", sep=""), row.names = FALSE)
  #just species list
  species_list <- data %>% select(genericName,specificEpithet) %>% filter(!is.na(specificEpithet)) %>% 
    mutate(species_name=paste0(genericName, "_", specificEpithet, sep="")) %>% select(species_name)
  write.csv(unique(species_list), paste0("result/", site, "_gbif_specieslist.csv", sep=""), row.names = FALSE, quote=FALSE)
}


####runing panel##########
#list of "White_Mountain", "Mountain_Lake" ploygon
sites <- c(dat$Site[1], dat$Site[3])
site.range <- list(White_Mountain, Mountain_Lake)
names(site.range) <- sites

#the total records were obtained by
file <- "result/gbif_occ_outputlog.txt"
for(i in 1:length(sites)){
  print(names(site.range)[i])
  cat(sites[i],"\n", file=file, append = TRUE)
  capture.output(occ(query="Tracheophyta", geometry = site.range[[i]][1], from = "gbif"), file=file, append = TRUE)
}

# got records from gbif
# White_Mountain 73
# Mountain_Lake 302

# apply the function to get records from gbif
records <- c(73, 302)
for(i in 1:length(names(site.range))){
  site <- names(site.range)[i]
  polygon <- site.range[[i]][1]
  N <- records[i]
  result <- get_data_gbif(site, N, polygon)
}

#########get_data_from_idigbio###################
#combination not working
tt <- occ(geometry = site.range[[1]][1], from = c("gbif", "idigbio", "inat"), has_coords=TRUE)
# Warning messages:
#   1: idigbio:  
#   2: idigbio: HTTP failure: 400
# Error message from API server: {"error":"Terms not found in index for type records","statusCode":400,"name":"TermNotFoundError","terms":["geopoint.1"]} 


