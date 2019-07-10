library(tidyverse)
library(ridigbio)
library(spocc)
#first letter Cap
cap.let <- function(x){
  x <- str_to_sentence(as.character(x))
}

#parsing geocode
json_cl <- function(y){
  y <- as.numeric(gsub("\\{\"lat\": |\"lon\": |\\}", "", y))
  return(y)
}

polygon_filter <- function(site,polygon){
  dat <- read_csv(paste0("./data/",site, ".csv"), col_names = T)
  a <-  dat %>% select("dwc:scientificName", "idigbio:geoPoint", "dwc:genus", "dwc:specificEpithet", "dwc:family", "dwc:order", "dwc:country")
  aa <- a %>% na.omit() %>% mutate_at(vars("dwc:scientificName", "dwc:genus", "dwc:family", "dwc:order", "dwc:country"), cap.let) %>% group_by("idigbio:geoPoint") %>% separate("idigbio:geoPoint", c("latitude", "longitude"), ",") %>% 
    mutate_at(vars(latitude), json_cl) %>% mutate_at(vars(longitude), json_cl)
  aa <- aa[,-9]
  names(aa) <- c("species", "latitude", "longitude", "genus", "epithet", "family", "order", "country")
  write.csv(aa,paste0("./results/",site, "_idigbio_records.csv"),row.names = F, quote = F)
  # qq <- wkt2bbox(polygon)
  # print(qq)
  # dat2 <- aa[(aa$longitude <= qq$max_x) & (aa$longitude >= qq$min_x) & (aa$latitude <= qq$max_y) & (aa$latitude >= qq$min_y), ]
  # print(dat2)
  # write.csv(dat2, paste0("./results/",site, "_filtered.csv"), row.names = F, quote = F)
  # idigbio_species <- unique(dat2$species)
  # idigbio_species_replaced <- gsub(" ", "_", idigbio_species)
  # idigbio_species_replaced <- as.data.frame(idigbio_species_replaced)
  # names(idigbio_species_replaced) <- "species"
  # write.csv(idigbio_species_replaced,paste0("./results/",site, "_species_list.csv"), row.names = F, quote = F)
}

##############################################################################################################################

site <- "Mountain_lake"
polygon <- "MULTIPOLYGON(((-80.52762413623942 37.37913451685481,-80.51511425590691 37.37698227937825,-80.51760278048918 37.35991570876333,-80.52701881944914 37.35355988246536,-80.52785953721342 37.37087866840957,-80.525539156184 37.37284594797798,-80.52920468563627 37.376343333877394,-80.52762413623942 37.37913451685481)))"
polygon_filter(site,polygon)
