library(httr)
library(XML)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(RSelenium)


############ try crawling #################
## setting headers ##
c = '_ga=GA1.2.1315615721.1521026860; _gid=GA1.2.1209046500.1521026860; cto_lwid=e1035138-2292-4880-a2dd-2d50aa166e6b; SPC_IA=-1; SPC_U=-; SPC_EC=-; csrftoken=oPOScYYlEyQW2GeJBP3jjeYL1jNX5DGa; SPC_SC_TK=; UYOMAPJWEMDGJ=; SPC_SC_UD=; SPC_F=wtsbgZwwNb7QUExBCsnIlXdMj5X07AjH; REC_T_ID=b6ff7872-277a-11e8-b94c-90b11c11fed6; SPC_T_ID="m7Q3wJsonJpF0sZ1rH3/dAi1b9rgu+zkfG+e8O2qy1VjALGelzl6wWsoMmuEgSYV+m1LcmdUFY1DjqIamUiF0IFXfMkmreC+0/R7N4wSZQM="; SPC_SI=3zz3xwx9dmj3fru7flguiuiokju0wby5; SPC_T_IV="9VwVgOmRlwXBMWYkwxWERg=="; __BWfp=c1521026866998x5c5c94e88; _gat=1'
js = '{"item_shop_ids":[{"itemid":19326926,"shopid":2066131},{"itemid":13195441,"shopid":2086246},{"itemid":18538404,"shopid":5239262},{"itemid":30539920,"shopid":3685075},{"itemid":11750718,"shopid":2098889},{"itemid":13233815,"shopid":1909162},{"itemid":13691272,"shopid":4124460},{"itemid":18922241,"shopid":511332},{"itemid":19116141,"shopid":5332726},{"itemid":19279720,"shopid":2946989},{"itemid":9231440,"shopid":2003759},{"itemid":16009533,"shopid":3618482},{"itemid":9869116,"shopid":3468681},{"itemid":16552213,"shopid":3500639},{"itemid":6040505,"shopid":329794},{"itemid":28099349,"shopid":3283880},{"itemid":15196039,"shopid":3689051},{"itemid":8877054,"shopid":2750206},{"itemid":5237618,"shopid":2397251},{"itemid":28245201,"shopid":955087},{"itemid":13477956,"shopid":4358834},{"itemid":25600695,"shopid":1596517},{"itemid":18783261,"shopid":3418161},{"itemid":24127771,"shopid":2738276},{"itemid":8362902,"shopid":3207532},{"itemid":24842770,"shopid":4337292},{"itemid":31328628,"shopid":1653643},{"itemid":22260204,"shopid":2706612},{"itemid":8706400,"shopid":2158716},{"itemid":31415900,"shopid":5095795},{"itemid":20555483,"shopid":3586266},{"itemid":14705568,"shopid":433566},{"itemid":30659989,"shopid":580176},{"itemid":29784468,"shopid":3202564},{"itemid":14644588,"shopid":2543452},{"itemid":13060669,"shopid":2337548},{"itemid":24000939,"shopid":3370408},{"itemid":13090218,"shopid":1056620},{"itemid":16901649,"shopid":1124030},{"itemid":15547492,"shopid":651859},{"itemid":12521443,"shopid":2623231},{"itemid":14897721,"shopid":3971988},{"itemid":23224888,"shopid":2501476},{"itemid":17102772,"shopid":1909162},{"itemid":29356327,"shopid":1241507},{"itemid":17754142,"shopid":3618482},{"itemid":1213966,"shopid":603184},{"itemid":14517258,"shopid":1587663},{"itemid":5179396,"shopid":2016909},{"itemid":15157869,"shopid":2354707}]}'


## POST ##
res = POST(url = 'https://shopee.tw/api/v1/items/',
          add_headers(referer = "https://shopee.tw/%E5%A5%B3%E7%94%9F%E8%A1%A3%E8%91%97-cat.62?categoryName=%E5%A5%B3%E7%94%9F%E8%A1%A3%E8%91%97&page=0",
                      `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36",
                      `x-csrftoken` = "oPOScYYlEyQW2GeJBP3jjeYL1jNX5DGa",
                      Cookie = c),
          body = js,encode = "json")


## JSON parsing ##
res %>% content(as = 'text') %>% fromJSON() %>% View


############### simulate explorer ######################
## prepare server ##
system('docker run -d -p 4445:4444 selenium/standalone-firefox')  # requir installed docker 
rD = rsDriver(port = 4445L,
              browser = "firefox")
remDr = rD$client


### [test] sending text & key to elements ##
# remDr$navigate("http://www.google.com/ncr")
# webElem <- remDr$findElement(using = "css", "[name = 'q']")
# webElem$sendKeysToElement(list("shopee", key = "enter"))
# remDr$getCurrentUrl()


## connect to the target site ##
url = "https://shopee.tw/%E5%A5%B3%E7%94%9F%E8%A1%A3%E8%91%97-cat.62"
remDr$navigate(url)


## get cookies ##
cookies = remDr$getAllCookies() %>% 
  do.call(rbind,.) %>% data.frame() %>% 
  transform(name = as.character(name), value = as.character(value)) 

# cookies 2 str function & mapping
cookies_str = cookies %>% select(name,value) %>%
  pmap(.f = function(name,value) {
    if(value == ""){
      paste(name)
    }
    else{
      paste(name,value,sep = "=")
    }
  }) %>% 
  do.call(function(...) paste(...,sep = "; "),args = .)


## request information ##
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.13; rv:58.0) Gecko/20100101 Firefox/58.0"
csrftoken = cookies %>% filter(name == "csrftoken") %>% .$value
api_url = "https://shopee.tw/api/v1/items/"


## get item list ##
# generate api url
item_number = 5000
item_order = (0:(item_number/50-1))*50
json_url = sprintf('https://shopee.tw/api/v1/search_items/?by=pop&order=desc&newest=%s&limit=50&categoryids=62',
                   as.character(item_order))

# json crawler function 
get_items = function(json_url){
  res_json = GET(url = json_url,
                 `user-agent` = userAgent,
                 Cookie = cookies_str)
  item_json = res_json %>% content(as = 'text') %>% fromJSON() %>% 
    .$item %>% toJSON() %>% as.character() %>% 
    sprintf('{"item_shop_ids":%s}',.)
  item_json
}

# mapping function
item_json = json_url %>% map(.f = get_items)


## get your data! ##
# crawler function
get_data = function(item_json){
  res = POST(url = api_url,
             add_headers(referer = url,
                         `user-agent` = userAgent,
                         `x-csrftoken` = csrftoken,
                         Cookie = cookies_str),
             body = item_json,
             encode = "json")
  res
}

# mapping function
res_list = item_json %>% map(.f = get_data)

## JSON parsing ##
# parsing function
shopeeDF = function(res){
  res %>% content(as = 'text') %>% fromJSON() %>% 
    select(c('itemid','shopid','name','brand','rating_star','rating_count','liked_count',
             'price_min','price_max','price','discount','wholesale_tier_list','sold','stock',
             'show_free_shipping','estimated_days','cmt_count','is_pre_order')) 
}

# mapping function
item_df = res_list %>% map(.f = shopeeDF) %>% 
  do.call(rbind,.)

## save & load ##
save(item_df,file = './shopee_girl_items.CSV')
load('./shopee_girl_items.CSV')

## visualization ##
# fix the value of price
item_df %<>% 
  transform(price_min = price_min/1e+5,
            price_max = price_max/1e+5,
            price = price/1e+5)

# [histogram] top 20th brand (number of items)  
item_df %>% 
  filter(brand != '') %>% 
  group_by(brand) %>% summarise(m=n()) %>% 
  arrange(m) %>% tail(20) %>% 
  ggplot(mapping = aes(brand,m)) +
  geom_histogram(stat = 'identity') +
  theme(text=element_text(family="黑體-繁 中黑", size=10),
        axis.text.x = element_text(angle = 60, hjust = 1)) 

# [scatter]&[density] rating_star ~ sold
item_df %>% 
  ggplot(mapping = aes(rating_star,sold)) + 
  geom_density(aes(y = ..count..*.01),
               colour = "#a0a0a0", fill = "#999999", alpha = .6) + 
  geom_point(size = .7, alpha = .4) +
  coord_cartesian(ylim = c(0, 40))

# [density] show_free_shipping ~ price
item_df %>% filter(price != 0) %>% 
  ggplot(mapping = aes(price,fill = show_free_shipping)) + 
  geom_density(alpha = .4) +
  coord_cartesian(xlim = c(0, 3000))


