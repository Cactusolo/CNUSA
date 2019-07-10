rm(list=ls())

library("tidyverse")
library("ridigbio")
library("stringr")
# devtools::install_github("tidyverse/stringr")
# devtools::install_github("pieterprovoost/rwkt")
library("rwkt")
#reading polygone
site.range <- readRDS("New_analysese/Site_range_gbif/site_range_10mk.rds")

#first letter Cap
cap.let <- function(x){
  x <- str_to_sentence(as.character(x))
}

#parsing geocode
json_cl <- function(y){
  y <- as.numeric(gsub("\\{\"lat\": |\"lon\": |\\}", "", y))
  return(y)
}

#make a loop go throught 6 sites

for (site in names(site.range)){
  #read in Magnoliopsida data
  a <- read_csv(paste0("New_analysese/iDigBio_6sites/", site, "_Magnoliopsida.csv", sep=""))
  aa <- a %>% select("dwc:scientificName", "idigbio:geoPoint", "dwc:genus", "dwc:specificEpithet", "dwc:family", "dwc:order", "dwc:country")
  #read in Pinopsida data
  b <- read_csv(paste0("New_analysese/iDigBio_6sites/", site, "_Pinopsida.csv", sep=""))
  bb <- b %>% select("dwc:scientificName", "idigbio:geoPoint", "dwc:genus", "dwc:specificEpithet", "dwc:family", "dwc:order", "dwc:country")
  #combine and clean
  data <- bind_rows(aa,bb) %>% na.omit() %>% mutate_at(vars("dwc:scientificName", "dwc:genus", "dwc:family", "dwc:order", "dwc:country"), cap.let) %>% group_by("idigbio:geoPoint") %>% separate("idigbio:geoPoint", c("latitude", "longitude"), ",") %>% 
    mutate_at(vars(latitude), json_cl) %>% mutate_at(vars(longitude), json_cl)
  if(dim(data)[2] >8){
    data <- data[,-9]
  }
  
  names(data) <- c("species", "latitude", "longitude", "genus", "epithet", "family", "order", "country")
  #output data
  write.csv(data, paste0("New_analysese/iDigBio_6sites/", site, "_idigbio_occ.csv", sep=""), row.names = FALSE, quote=FALSE)
  #output species list
  
  write.csv(unique(paste0(data$genus, "_", data$epithet, sep="")), paste0("New_analysese/Site_range_idigbio/", site, "_idigbio_specieslist.csv", sep=""), col.names=FALSE, row.names = FALSE, quote=FALSE)
}

gml <- wkt2geojson(Mountain_Lake)

idig_search_records(rq=list(genus="acer"), limit=10)
idig_count_records(rq=list(genus="acer", geopoint=aa))


tt <- wkt2bbox(White_Mountain)

ll <- bbox2wkt(tt)

aa <- toGeoJSON(ll, pretty = TRUE, data = list("geopoint", "geo_bounding_box"))
