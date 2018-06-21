library(tidyverse)
library(magrittr)

data = read_csv("real_estate_ready.CSV")
data$house_age = data$built_date %>% 
  format("%Y") %>% as.numeric %>% `*`(-1) %>% `+`(2018) 

write_csv(data, "real_estate_ready.CSV")

