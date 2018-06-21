library(tidyverse)
library(magrittr)
library(httr)

data = read_csv("real_estate_ready.CSV")
geo_miss = data %>% filter(is.na(lat)|is.na(lng)) 

# google map api
address = geo_miss %>% select(id,address)

urlAPI = "https://maps.googleapis.com/maps/api/geocode/json"
key = "AIzaSyDX7VCsQlVau9VlZ9yKAvIad8EvQjWwav4"

lookUpGIS = function(address,key){
  urlAPI = "https://maps.googleapis.com/maps/api/geocode/json"
  urlAPI %>% GET(query = list(address=address,key=key)) %>%
    content()
}  

res = lookUpGIS(address = address,key=key) 
res %>% .$results %>% .[[1]] %>% .$geometry %>% .$location

res_all = map(address$address,lookUpGIS,key=key) 
address_fix = res_all %>% 
  map(function(content) {content$results[[1]]$geometry$location}) %>% 
  do.call(rbind,.) %>% cbind(id = address$id)
address_fix %<>% transform(lat = as.numeric(lat), 
                           lng = as.numeric(lng),
                           id = as.character(id))

# join 
data[data$id %in% address_fix$id,c("lat","lng")] = 
  address_fix[,c("lat","lng")]

write_csv(data,"real_estate_ready.CSV")
