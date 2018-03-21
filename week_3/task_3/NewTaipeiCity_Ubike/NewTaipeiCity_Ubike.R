library("rjson")
library("jsonlite")
library("htmlwidgets")
library("leaflet")

json_data <- fromJSON("http://data.ntpc.gov.tw/api/v1/rest/datastore/382000000A-000352-001")
lon <- sapply(json_data$result$records$lng, as.numeric)
lat <- sapply(json_data$result$records$lat, as.numeric)
name <- json_data$result$records$sna
number <- json_data$result$records$sbi

m <- leaflet(data = json_data$result$records) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lon, lat, popup=paste("地點：", name, "<br>",
                           "數量：", number, "<br>")) %>% 
  addProviderTiles(providers$OpenStreetMap)
m  # Print the map

saveWidget(m, file="/Users/Kuan-Hao/Desktop/Ubike.html")