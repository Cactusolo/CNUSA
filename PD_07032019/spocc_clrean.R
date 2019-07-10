rm(list=ls())

library("spocc")
library("tidyverse")
# Tracheophyta
# 松柏门 Coniferophyta
# 苏铁门 Cycadophyta
# 银杏门 Ginkgophyta
# 买麻藤门 Gnetophyta
# 被子植物门 Magnoliophyta

# plants <- c("Coniferophyta", "Cycadophyta", "Ginkgophyta", "Gnetophyta", "Magnoliophyta")
# 
# # res <- occ(geometry = site_range_10mk$Talladega$Range, from = c("gbif", "idigbio", "inat"), has_coords=TRUE)

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
  saveRDS(data, paste0("Site_range/", site, "_gbif_raw_Tracheophyta.rds", sep=""))
  #all records
  data.res <- data %>% select(name,longitude,latitude,scientificName,order,family,genus,species,genericName,specificEpithet,country)
  write.csv(data.res, paste0("Site_range/", site, "_gbif_occdata.csv", sep=""), row.names = FALSE)
  #just species list
  species_list <- data %>% select(genericName,specificEpithet) %>% filter(!is.na(specificEpithet)) %>% 
    mutate(species_name=paste0(genericName, "_", specificEpithet, sep="")) %>% select(species_name)
  write.csv(unique(species_list), paste0("Site_range/", site, "_gbif_specieslist.csv", sep=""), row.names = FALSE, quote=FALSE)
}


####runing panel##########
#reading polygone
site.range <- readRDS("Site_range/site_range_10mk.rds")

#the total records were obtained by
# file <- "Site_range/outputlog.txt"
# for(i in 1:length(names(site.range))){
#   # print(names(site.range)[i])
#   cat(names(site.range)[i],"\n", file=file, append = TRUE)
#   capture.output(occ(query="Tracheophyta", geometry = site.range[[i]]$Range, from = "gbif"), file=file, append = TRUE)
# }

records <- c(415, 677, 806, 750, 1058, 1457)
for(i in 1:length(names(site.range))){
  site <- names(site.range)[i]
  polygon <- site.range[[i]]$Range
  N <- records[i]
  get_data_gbif(site, N, polygon)
}

