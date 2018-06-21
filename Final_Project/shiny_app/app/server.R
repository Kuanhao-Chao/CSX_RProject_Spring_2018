library(magrittr)
library(tidyverse)
library(reticulate)

path_to_python <- "/anaconda3/bin/python"
use_python(path_to_python)


real.estate.data <- read.csv("real_estate_ready.CSV")
mrt.data <- read.table("final_dataset/MRT_station_data.csv")
bus.station.data <- read.table("final_dataset/bus_station_data.csv")
park.data <- read.table("final_dataset/park.final.csv")
store.data <- read.table("final_dataset/store_all.csv")
fields_1 <- c("region", "usage", "land", "building", "car_park", "select.function", "low_age", "high_age", "low_area", "high_area", "low_price", "high_price")
fields_2 <- c("pre_region", "pre_usage", "pre_area", "pre_age", "pre_type", "pre_n_room", "pre_n_hall", "pre_n_bath", "pre_is_comp", "pre_is_manage", "pre_is_furn")
fileName_1 <- "user_search.csv"
fileName_2 <- "user_predict.csv"



outputDir <- "responses"
saveData_1 <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses_1")) {
    responses_1 <<- rbind(responses_1, data)
    # Create a unique file name
    # fileName <- "user_inputs.csv"
    write.csv(
      x = responses_1,
      file = file.path(outputDir, fileName_1), 
      row.names = FALSE, quote = TRUE
    )
  } else {
    responses_1 <<- data
    # fileName <- "user_inputs.csv"
    write.csv(
      x = responses_1,
      file = file.path(outputDir, fileName_1), 
      row.names = FALSE, quote = TRUE
    )
  }
}

saveData_2 <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses_2")) {
    responses_2 <<- rbind(responses_2, data)
    # Create a unique file name
    # fileName <- "user_inputs.csv"
    write.csv(
      x = responses_2,
      file = file.path(outputDir, fileName_2), 
      row.names = FALSE, quote = TRUE
    )
  } else {
    responses_2 <<- data
    # fileName <- "user_inputs.csv"
    write.csv(
      x = responses_2,
      file = file.path(outputDir, fileName_2), 
      row.names = FALSE, quote = TRUE
    )
  }
}

loadData_1 <- function() {
  if (exists("responses_1")) {
    responses_1
  }
}

loadData_2 <- function() {
  if (exists("responses_2")) {
    responses_2
  }
}

function(input, output, session) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  #convenient.store <- read.csv("")
  
  # Whenever a field is filled, aggregate all form data
  
  fromData_1 <- reactive({
    data <- sapply(fields_1, function(x) input[[x]])
    data
  })
  
  fromData_2 <- reactive({
    data <- sapply(fields_2, function(x) input[[x]]) %>% 
      c(lat = input$premap_click[[1]], lng = input$premap_click[[2]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData_1(fromData_1())
  })
  
  observeEvent(input$pre_submit, {
    saveData_2(fromData_2())
  })
  # show the previous response
  output$responses <- DT::renderDataTable({
    input$submit
    loadData_1()
  })
  
  #######################################################
  ################ History Finding ######################
  
  select.data <- eventReactive(input$submit,
                               {real.estate.data %>% filter(district_id == input$region,
                                                            use == input$usage,
                                                            is.land == input$land,
                                                            is.building == input$building,
                                                            is.park == input$car_park,
                                                            build_state == input$select.function,
                                                            house_age %in% input$low_age:input$high_age,
                                                            total_size >= input$low_area & total_size <= input$high_area,
                                                            PRICE >= input$low_price & PRICE <= input$high_price) %>% 
                                   select(lat,lng,PRICE,total_size,house_age)
                               })
  
  houseLeafIcon <- makeIcon(
    iconUrl = "images/house-xxl.png",
    iconWidth = 38, iconHeight = 38
  )
  
  mrtLeafIcon <- makeIcon(
    iconUrl = "images/mrt.png",
    iconWidth = 38, iconHeight = 38
  )
  
  busLeafIcon <- makeIcon(
    iconUrl = "images/bus.png",
    iconWidth = 38, iconHeight = 38
  )
  
  parkLeafIcon <- makeIcon(
    iconUrl = "images/park.png",
    iconWidth = 38, iconHeight = 38
  )
  
  storeLeafIcon <- makeIcon(
    iconUrl = "images/store.png",
    iconWidth = 38, iconHeight = 38
  )
  
  predict.marker <- makeIcon(
    iconUrl = "images/predict.png",
    iconWidth = 38, iconHeight = 38
  )
  
  output$mymap <- renderLeaflet({
    leaflet(data = select.data()) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~lng,~lat, icon = houseLeafIcon,  label = ~{sprintf("價格:%s<br/>面積:%s<br/>屋齡:%s",PRICE, total_size, house_age) %>% 
          lapply(htmltools::HTML)}) %>% 
      addMarkers(group = "mrt.layer", data = cbind(mrt.data$lat, lng = mrt.data$lng), layerId = mrt.data$station_name, popup = mrt.data$station_name, icon = mrtLeafIcon, clusterOptions = markerClusterOptions()) %>%
      addMarkers(group = "park.layer", data = cbind(park.data$Longitude, lng = park.data$Latitude), layerId = park.data$Name, popup = park.data$Name, icon = parkLeafIcon, clusterOptions = markerClusterOptions()) %>% 
      addMarkers(group = "bus.layer", data = cbind(bus.station.data$longitude, lng = bus.station.data$latitude), layerId = bus.station.data$nameZh, popup = bus.station.data$nameZh, icon = busLeafIcon, clusterOptions = markerClusterOptions()) %>%
      addMarkers(group = "store.layer", data = cbind(store.data$lng, lng = store.data$lat), layerId = as.character(store.data$lng), icon = storeLeafIcon, clusterOptions = markerClusterOptions()) %>%
      addLayersControl(
        overlayGroups = c("mrt.layer", "park.layer", "bus.layer", "store.layer"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup("mrt.layer") %>%
      hideGroup("park.layer") %>%
      hideGroup("bus.layer") %>% 
      hideGroup("store.layer")
    }
  )  
  
  # observeEvent(input$mrt, {
  #   proxy <- leafletProxy("mymap", session)
  #   if (!isTRUE(input$mrt)){
  #     # proxy %>% removeMarker(layerId = mrt.data$station_name)
  #     proxy%>% hideGroup("mrt.layer")
  #   } else {
  #     # proxy %>% addMarkers(group = "mrt.layer", data = cbind(mrt.data$lat, lng = mrt.data$lng), layerId = mrt.data$station_name, popup = mrt.data$station_name, icon = mrtLeafIcon, clusterOptions = markerClusterOptions())
  #     proxy %>% showGroup("mrt.layer")
  #   }
  # }, ignoreNULL = FALSE)
  # 
  # observeEvent(input$park, {
  #   proxy <- leafletProxy("mymap", session)
  #   if (!isTRUE(input$park)){
  #     # proxy %>% removeMarker(layerId = park.data$Name)
  #     proxy%>% hideGroup("park.layer")
  #   } else {
  #     # proxy %>% addMarkers(group = "park.layer", data = cbind(park.data$Longitude, lng = park.data$Latitude), layerId = park.data$Name, popup = park.data$Name, icon = parkLeafIcon, clusterOptions = markerClusterOptions())
  #     proxy %>% showGroup("park.layer")
  #   }
  # }, ignoreNULL = FALSE)
  # 
  # 
  # observeEvent(input$bus, {
  #   proxy <- leafletProxy("mymap", session)
  #   if (!isTRUE(input$bus)){
  #     # proxy %>% removeMarker(layerId = bus.station.data$nameZh)
  #     proxy%>% hideGroup("bus.layer")
  #   } else {
  #     # proxy %>% addMarkers(group = "bus.layer", data = cbind(bus.station.data$longitude, lng = bus.station.data$latitude), layerId = bus.station.data$nameZh, popup = bus.station.data$nameZh, icon = busLeafIcon, clusterOptions = markerClusterOptions())
  #     proxy %>% showGroup("bus.layer")
  #   }
  # }, ignoreNULL = FALSE)
  # 
  # observeEvent(input$store, {
  #   proxy <- leafletProxy("mymap", session)
  #   if (!isTRUE(input$store)){
  #     # proxy %>% removeMarker(layerId = as.character(store.data$lng))
  #     proxy %>% showGroup("store.layer")
  #   } else {
  #     # proxy %>% addMarkers(group = "store.layer", data = cbind(store.data$lng, lng = store.data$lat), layerId = as.character(store.data$lng), icon = storeLeafIcon, clusterOptions = markerClusterOptions())
  #     proxy %>% showGroup("store.layer")
  #   }
  # }, ignoreNULL = FALSE)
  
  ########################################################
  ################### Prediction #########################

  output$premap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = {cbind(rnorm(1) * .003 + 121.537, rnorm(1) * .003 + 25.038)}, icon = predict.marker, layerId = "predict.marker.init"
      )
  })

  # observeEvent(input$pre_submit, {
  #   proxy <- leafletProxy("mymap", session)
  #   proxy %>% removeMarker(layerId = "predict.marker")
  #   # proxy %>% addMarkers(group = "mrt.layer", data = cbind(mrt.data$lat, lng = mrt.data$lng), layerId = mrt.data$station_name, popup = mrt.data$station_name, icon = mrtLeafIcon, clusterOptions = markerClusterOptions())
  # }, ignoreNULL = FALSE)

  
  output$lng <- reactive(
    sprintf("經度:%.3f", input$premap_click[1])
  )
  
  output$lat <- reactive(
    sprintf("緯度:%.3f", input$premap_click[2])
  )
  
  output$cursor <- renderText(cursor())
  
  observeEvent(input$pre_submit,{
    proxy2 <- leafletProxy("premap", session)
    proxy2 %>% removeMarker(layerId = "predict.marker.init")
    user.input <- read.csv("./responses/user_predict.csv")
    # length(row.names(user.input))
    user.input.last.row<- user.input[length(row.names(user.input)),]
    lat <- user.input.last.row$lat
    lng <- user.input.last.row$lng
    cat("lat : ", lat, "lng : ", lng)
    proxy2 %>% addMarkers(data = cbind(lat = lng, lng = lat), layerId = "predict.marker.init", icon = predict.marker)
  })
  
  price <- ""
  res <- eventReactive(input$pre_submit,{
    path = "/Users/Kuan-Hao/anaconda3/envs/py36/bin/python"
    use_python(path, required = FALSE)
    cat(py_config()$version)
    setwd("./python_code/")
    source_python("predict.py")
    Sys.sleep(1)
    setwd("../")
    price <<- scan("responses/return.txt", what="character")
    # proxy <- leafletProxy("mymap", session)
    # proxy %>% removeMarker(layerId = "predict.marker")
  })
  output$pre_return <- renderText(res())
}








