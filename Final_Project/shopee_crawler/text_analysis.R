library(wordcloud)
library(tidyverse)
library(magrittr)
library(tm)
library(jiebaR)
library(SnowballC)
library(colorspace)
library(NLP)

load("shopee_girl_items.CSV")

doc = item_df$name
toSpace = content_transformer(function(x, pattern) 
  gsub(pattern, " ", x))


doc_temp = doc %>% VectorSource %>% Corpus %>% 
  tm_map(toSpace, "[A-Za-z0-9]") %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace)


mixseg = worker()

jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(doc_temp, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame %<>% arrange(Freq) %>% tail(100)
freqFrame %>% View


par(family = 'STHeiti') 
  
wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=3,random.color=TRUE,
          colors=rainbow_hcl(nrow(freqFrame))
)
        



