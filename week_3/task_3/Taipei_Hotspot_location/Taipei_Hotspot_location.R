require("RCurl")
library("jsonlite")
library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
library(mapproj)

data <- fromJSON("http://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=15a8dc36-3173-4d94-8144-a390d17ac34a")
lon = sapply(data$result$results$經度, as.numeric)
lat = sapply(data$result$results$緯度, as.numeric)
# Different type
#map <- get_map(location = c(lon = 121.533937, lat = 25.03933), zoom = 13, language = "zh-TW", maptype = "roadmap")
# Great to show data on the map
map <- get_map(location = c(lon = 121.533937, lat = 25.03933), zoom = 13, language = "zh-TW", maptype = "toner-lite")
# ggmap(map)

ggmap(map) + geom_point(aes(x = lon, y = lat, color = "red"), data = data$result$results)