
library(httr)

url <- "http://24h.pchome.com.tw/region/DHAA"

res <- GET(url)

re_json = content(res)

re_json

do.call(rbind,res_json$prods)
View(data.frame(do.call(rbind,res_json$prods)))
