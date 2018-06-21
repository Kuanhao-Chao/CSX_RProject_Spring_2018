library(tidyverse)
library(magrittr)


data = read_csv("real_estate_ready.CSV")

# total_size
data %>% ggplot(mapping = aes(total_size/3.3)) + 
  geom_density() + scale_x_continuous(limits = c(0,2e+2)) +
  xlab("坪數") + theme(text=element_text(family="黑體-繁 中黑"))

# location
data %>% filter(lat!=0|lng!=0) %>% 
  ggplot(mapping = aes(lat, lng)) + geom_density_2d()
  
