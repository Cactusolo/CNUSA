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

idigbio_records <- function(site){
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

site <- "Mountain_Lake"
idigbio_records(site)

site <- "White_Mountain"
idigbio_records(site)

########### checking ##########
# Records have been imported into QGIS, and fit focal polygons perfectly after clipping. Snapshots have been saved under the ./results directory.

########### removing duplicates ########### 
## Site White Mountain
dup_White_Mountain <- read.csv("results/White_Mountain_clipped.csv")
uniq_White_Mountain <- unique(dup_White_Mountain$species)
# manual examine found alien species name 'Acer spicatum lam.'
uniq_White_Mountain <- uniq_White_Mountain[-17]
uniq_White_Mountain_replaced <- gsub(" ", "_", uniq_White_Mountain)
uniq_White_Mountain_replaced  <- as.data.frame(uniq_White_Mountain_replaced)
names(uniq_White_Mountain_replaced) <- "species"
write.csv(uniq_White_Mountain_replaced, "./results/uniq_White_Mountain_species_list.csv",row.names = F, quote = F)
## Site Mountain Lake
dup_Mountain_Lake <- read.csv("results/Mountain_Lake_clipped.csv")
uniq_Mountain_Lake <- unique(dup_Mountain_Lake$species)
# manual examine found no alien species name
uniq_Mountain_Lake_replaced <- gsub(" ", "_", uniq_Mountain_Lake)
uniq_Mountain_Lake_replaced  <- as.data.frame(uniq_Mountain_Lake_replaced)
names(uniq_Mountain_Lake_replaced) <- "species"
write.csv(uniq_Mountain_Lake_replaced, "./results/uniq_Mountain_Lake_species_list.csv",row.names = F, quote = F)
