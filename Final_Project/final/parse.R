library(tidyverse)
library(magrittr)


# big5 2 utf8
parser = function(file_path)
{ data <- readLines(file_path, encoding="big5") %>% 
    iconv("big5", "utf8") %>% map(function(i) strsplit(i,","))
  data %<>% do.call(rbind,.) %>% do.call(rbind,.) %>% data.frame(stringsAsFactors=FALSE)
  colnames(data) <- data[1,]   
  data <- data[-1,-c((ncol(data)-1):ncol(data))]
  data
}

# generate file path
section = sprintf("%sQ%s",
                  rep(2015:2017,each=4),
                  rep(1:4,3))

# function
convert = function(section){
  # load in 
  file_path = sprintf("~/Desktop/R /data/real_estate/%s.csv",section)
  tmp_data = parser(file_path)
  
  # select proper columns
  tmp_data$section = rep(section,nrow(tmp_data))
  data = select(tmp_data,section,address,lat,lng,unit_PRICE,PRICE,room,hall,health,total_size,land_size)
  
  # transform $pen to features
  tmp_pen = tmp_data$pen %>% 
    sapply(function(chr) strsplit(chr,c("土地|建物|車位")),USE.NAMES = F) %>% 
    do.call(rbind,.) %>% .[,-1]
  data %<>% mutate(n_land = tmp_pen[,1], n_build = tmp_pen[,2], n_park = tmp_pen[,3])
  
  # generate district_id
  district = list(district = c("北投區", "大安區", "大同區", "南港區", "內湖區", "士林區", "松山區", "萬華區", "文山區", "信義區", "中山區", "中正區"),
                  district_id = 1:12)
  data$district_id = tmp_data$district %>% 
    factor(levels = district$district, labels = district$district_id)
  
  # date convert 
  data$date = tmp_data$date %>% as.numeric %>% 
    lapply(function(i) `+`(i,19110000)) %>% 
    as.character %>% as.Date("%Y%m%d")
  
  data$built_date = tmp_data$complete_yr %>% as.numeric() %>% 
    lapply(function(i) `+`(i,19110000)) %>% 
    as.character %>% as.Date("%Y%m%d")
  
  # floor convert
  number_list = list(c = c("一二三四五六七八九十"), n = "1234567891")
  
  data$floor = tmp_data$level %>% gsub("層", "",.) %>% 
    gsub("[^一二三四五六七八九十]", "",.) %>% 
    chartr(number_list$c, number_list$n,.) %>% 
    gsub("^$", "-1",.)   #非數字樓層都以-1表示
  
  data$total_floor = tmp_data$floor_num 
  
  # build_state convert
  build_state = list(state = c("辦公商業大樓","廠辦","店面(店鋪)","工廠","公寓(5樓含以下無電梯)",
                               "華廈(10層含以下有電梯)","其他","套房(1房1廳1衛)","透天厝","住宅大樓(11層含以上有電梯)"),
                     state_id = 1:10)
  
  data$build_state = tmp_data$build_state %>% 
    factor(levels = build_state$state, labels = build_state$state_id)
  
  # usage convert
  use = list(use = c("住","商","工","住商","住工"), use_id = 1:5)
  
  tmp_use = tmp_data$use %>% gsub("[^工|商|住]","",.) %>% 
    factor(levels = use$use, labels = use$use_id) %>% 
    as.character()
  
  tmp_use[which(is.na(tmp_use))] = "-1"
  data$use = tmp_use
  
  # 有無 convert
  data$is.comp = tmp_data$compartmented %>% chartr("有無","10",.) 
  data$is.manage = tmp_data$manage %>% chartr("有無","10",.) 
  data$is.furn = tmp_data$furniture %>% chartr("有無","10",.) 
  
  return(data)
}

# test

# mapping & fix type problem
result = map(section,convert) %>% do.call(rbind,.)
result %<>% transform(lat = as.numeric(lat),
                      lng = as.numeric(lng),
                      unit_PRICE = as.numeric(unit_PRICE),
                      PRICE = as.numeric(PRICE),
                      n_room = as.integer(room),
                      n_hall = as.integer(hall),
                      n_bath = as.integer(health),
                      total_size = as.numeric(total_size),
                      land_size = as.numeric(land_size),
                      n_land = as.integer(n_land),
                      n_build = as.integer(n_build),
                      n_park = as.integer(n_park),
                      district_id = as.integer(district_id),
                      floor = as.integer(floor),
                      total_floor = as.integer(total_floor),
                      build_state = as.integer(build_state),
                      use = as.integer(use),        
                      is.comp = as.logical(as.numeric(is.comp)),
                      is.manage = as.logical(as.numeric(is.manage)),
                      is.furn = as.logical(as.numeric(is.furn))) 

result %<>% select(-room,-hall,health)
result$unit_PRICE = result$PRICE/result$total_size
result$id = 1:nrow(result)

# output
write_csv(result,"real_estate_ready.CSV")

