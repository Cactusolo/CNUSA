rm(list=ls())

library("spocc")
library("tidyverse")
library("rinat")
library("sp")
library("wicket")
# Tracheophyta
# 松柏门 Coniferophyta
# 苏铁门 Cycadophyta
# 银杏门 Ginkgophyta
# 买麻藤门 Gnetophyta
# 被子植物门 Magnoliophyta

# plants <- c("Coniferophyta", "Cycadophyta", "Ginkgophyta", "Gnetophyta", "Magnoliophyta")
# 
# # res <- occ(geometry = site_range_10mk$Talladega$Range, from = c("gbif", "idigbio", "inat"), has_coords=TRUE)
# res <- occ(query="Tracheophyta", geometry = site_range_10mk$Talladega$Range, from = c("gbif", "inat"))
# # site_range_10mk <- readRDS("Site_range/site_range_10mk.rds")
# library(rgbif)
# name_lookup(query='Tracheophyta')
# 
# test <- occ(query="Tracheophyta", geometry = site_range_10mk$Talladega$Range, from = "gbif")
# test2$gbif$data[[1]] %>% group_by(phylum) %>% filter(phylum=="Tracheophyta")
# 

# a function that combined result of reach query for missing values add "NA"
# rbind.all.columns <- function(x, y) {
#   
#   x.diff <- setdiff(colnames(x), colnames(y))
#   y.diff <- setdiff(colnames(y), colnames(x))
#   
#   x[, c(as.character(y.diff))] <- NA
#   
#   y[, c(as.character(x.diff))] <- NA
#   
#   return(rbind(x, y))
# }

get_data_gbif <- function(site, N, polygone){
  data <- NULL
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
    
  } else{
    res <- occ(query="Tracheophyta", geometry = polygone, from = "gbif")
    data <- res$gbif$data[[1]]
  }
  saveRDS(data, paste0("Site_range/", site, "_gbif_raw_Tracheophyta.rds", sep=""))
  data.res <- data %>% select(name,longitude,latitude,scientificName,order,family,genus,species,genericName,specificEpithet,country)
  write.csv(data.res, paste0("Site_range/", site, "_gbif_occdata.csv", sep=""), row.names = FALSE)
  #just species list
  species_list <- data %>% select(genericName,specificEpithet) %>% filter(!is.na(specificEpithet)) %>% 
    mutate(species_name=paste0(genericName, "_", specificEpithet, sep="")) %>% select(species_name)
  write.csv(unique(species_list), paste0("Site_range/", site, "_gbif_specieslist.csv", sep=""), row.names = FALSE, quote=FALSE)
}

# occ(query="Tracheophyta", geometry = site.range$Talladega$Range, from = "gbif")

# get_data_gbif("Talladega", 1058, site.range$Talladega$Range)
# 
# file <- "Site_range/outputlog.txt"
# for(i in 1:length(names(site.range))){
#   # print(names(site.range)[i])
#   cat(names(site.range)[i],"\n", file=file, append = TRUE)
#   capture.output(occ(query="Tracheophyta", geometry = site.range[[i]]$Range, from = "gbif"), file=file, append = TRUE)
# }

####runing panel##########
site.range <- readRDS("Site_range/site_range_10mk.rds")

records <- c(415, 677, 806, 750, 1058, 1457)
for(i in 1:length(names(site.range))){
  site <- names(site.range)[i]
  polygon <- site.range[[i]]$Range
  N <- records[i]
  get_data_gbif(site, N, polygon)
}

site.range <- readRDS("New_analysese/Site_range_gbif/site_range_10mk.rds")
polygon <- site.range[[1]]$Range

get_inat_obs(query="vascular plants", bounds = wkt_bounding(polygon))


get_inat_obs(taxon_name="Tracheophyta", bounds = wkt_bounding(polygon),maxresults = 1000)
