---
title: "TFID_Visualization"
author: "Kuan-Hao, Chao"
date: "2018/4/10"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## 問題設定: 分析美國大選期間（2016/1/1 ~ 2016/11/8)，川普和希拉蕊在臉書上的動態消息分析
```{r}
library(httr)
library(rjson)
library(httpuv)
library(Rfacebook)
library(plyr)
library(wordcloud)
library(wordcloud2)
library(corpus)
library(knitr)
library(tm)
library(jiebaRD)
library(jiebaR)
library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)
library(forcats)
library(rlist)
```

### 先獲取川普（2016/1/1 ~ 2016/11/8)的貼文！

```{r}
token <- "EAACEdEose0cBAN0rMFbgMfdI723608lJN5R0ZBJalUZCnGMZA2LQj9jipD06VtkSHs8i2azPqjs4SjFG1xbkR6oUqXgJ57PfxLwluHP9ZAwTeMP9ZCdCHIt69ZCkEqYZCiCozcwDi68v2u3gSmZBGcQFqupaQGHUlazySG8v0DIqsZADhNTfbyBQYXSb9BpLENaIZD"
trump_prefex_post <- "https://graph.facebook.com/v2.12/153080620724/posts?limit=1&until=2016-11-8&since=2016-1-1"
prefix_access_token = "&access_token="
trump_url <- paste0(trump_prefex_post, prefix_access_token, token)
```

```{r}
trump_counter <- 0
trump_next_url <- trump_url
trump_posts_id <- list()
while(trump_next_url == 'NULL'){
    # url <- paste0(prefex_post, next_page, "&access_token=", token)
    # Get the response of the url (ex. posts)
    res <- httr::GET(trump_next_url)
    # Get the content of the url
    posts <- httr::content(res)
    trump_counter <<- trump_counter + 1
    trump_post <- posts$data[[1]]$message
    trump_post_date <- posts$data[[1]]$created_time
    trump_post_id <- posts$data[[1]]$id
    # list.append(trump_posts_id, trump_post_id)
    trump_posts_id[[trump_counter]] <- trump_post_id 
    trump_next_url <<- posts$paging$'next'
    # trump_url_check <- posts$paging$'next'
    trump_filename <- paste0("data/Donald_Trump/", trump_counter,"_", trump_post_date, ".txt")
    #write.table(trump_post, trump_filename)
}
```

### 再來獲取希拉蕊（2016/1/1 ~ 2016/11/8)的貼文！

```{r}
hillary_prefex_post <- "https://graph.facebook.com/v2.12/889307941125736/posts?limit=1&until=2016-11-8&since=2016-1-1"
prefix_access_token = "&access_token="
hillary_url <- paste0(hillary_prefex_post, prefix_access_token, token)
```
```{r}
hillary_counter <- 0
hillary_next_url <- hillary_url
hillary_posts_id <- list()
hillary_posts_id
while(hillary_next_url == 'NULL'){
    # url <- paste0(prefex_post, next_page, "&access_token=", token)
    # Get the response of the url (ex. posts)
    res <- httr::GET(hillary_next_url)
    # Get the content of the url
    posts <- httr::content(res)
    hillary_counter <<- hillary_counter + 1
    hillary_post <- posts$data[[1]]$message
    hillary_post_date <- posts$data[[1]]$created_time
    hillary_post_id <- posts$data[[1]]$id
    hillary_posts_id[[hillary_counter]] <- hillary_post_id 
    hillary_next_url <<- posts$paging$'next'
    # trump_url_check <- posts$paging$'next'
    hillary_filename <- paste0("data/Hillary_Clinton/", hillary_counter,"_", hillary_post_date, ".txt")
#    write.table(hillary_post, hillary_filename)
}
```
## 繪製文字雲

### 接下來將剛剛寫入的檔案都讀下來，並且存成data frame！ 
### 首先處理川普的！ 

```{r}
trump_file_path <- "/Users/Kuan-Hao/Documents/大二下/資料科學程式設計-蔡芸琤 /106-2RSampleCode/Project_1/data/Donald_Trump"
setwd(trump_file_path)
getwd()
trump_filenames <- list.files(getwd(), pattern="*.txt")
trump_files <- lapply(trump_filenames, readLines)
typeof(trump_files)
```
```{r}
# VectorSource: A vector giving the texts.
trump_sentence = unlist(trump_files)
#for (j in seq(trump_sentence))
#{
#  trump_sentence[[j]] <- gsub("Donold Trump", "Donold_Trump", trump_sentence[[j]])
#}
#trump_sentence
#trump_sentence <- tm_map(trump_sentence, removePunctuation)
#trump_sentence

trump_dataframe <- data_frame(file = 1:length(trump_sentence), text = trump_sentence)
trump_dataframe_tidy <- trump_dataframe %>% 
              unnest_tokens(word, text)
trump_dataframe_tidy_2 <- trump_dataframe_tidy %>%
        anti_join(stop_words) %>%
        count(word, sort = TRUE)
trump_dataframe_self_check = subset(trump_dataframe_tidy_2, word != 1 )
trump_dataframe_self_check = subset(trump_dataframe_self_check)
trump_dataframe_head_10 <- head(trump_dataframe_self_check, 10)
# trump_doc <- Corpus(VectorSource(trump_files))
# trump_doc
```
```{r}
pal = colorRampPalette(brewer.pal(9,"Blues"))(32)[seq(8,32,6)]
# wordcloud2(data = trump_dataframe_self_check, size=0.5)
wordcloud(word = trump_dataframe_self_check$word, freq = trump_dataframe_self_check$n, scale = c(4, 0.3), min.freq = 10, random.order = F,
          random.color = F, colors = rev(pal))
```

### 再來處理希拉蕊的！ 
```{r}
hillary_file_path <- paste0(getwd(), "/data/Hillary_Clinton/")
setwd(hillary_file_path)
getwd()
hillary_filenames <- list.files(getwd(), pattern="*.txt")
hillary_files <- lapply(hillary_filenames, readLines)
typeof(hillary_files)
```
```{r}
# VectorSource: A vector giving the texts.
hillary_sentence = unlist(hillary_files)
hillary_dataframe <- data_frame(file = 1:length(hillary_sentence), text = hillary_sentence)
hillary_dataframe_tidy <- hillary_dataframe %>% 
              unnest_tokens(word, text)
hillary_dataframe_tidy

hillary_dataframe_tidy_2 <- hillary_dataframe_tidy %>%
        anti_join(stop_words) %>%
        count(word, sort = TRUE)
hillary_dataframe_tidy_2
hillary_dataframe_self_check = subset(hillary_dataframe_tidy_2, word != 1 )
hillary_dataframe_self_check = subset(hillary_dataframe_self_check, word != "de" )
hillary_dataframe_self_check[hillary_dataframe_self_check$n == "hillary's"] <- "hillary"
hillary_dataframe_self_check
hillary_dataframe_self_check = subset(hillary_dataframe_self_check )
hillary_dataframe_self_check
hillary_dataframe_head_10 <- head(hillary_dataframe_self_check, 10)
```
```{r}
wordcloud(word = hillary_dataframe_self_check$word, freq = hillary_dataframe_self_check$n, scale = c(5, 0.3), random.order = F,
          random.color = F, min.freq = 10, colors = rev(pal))
# wordcloud2(data = hillary_dataframe_self_check, size=1)
```
```{r}
trump_dataframe_self_check
```
```{r}
hillary_dataframe_self_check
```
## 繪製頻率長條圖

```{r}
p <- ggplot(trump_dataframe_head_10, aes( x = fct_inorder(word), y = n)) 
p +geom_bar(stat = "identity") + coord_flip()
```

```{r}
#hillary_dataframe_head_10 <- within(hillary_dataframe_head_10, word <- factor(word, levels = names(sort(table(word), decreasing = TRUE))))
p <- ggplot(hillary_dataframe_head_10,aes( x = fct_inorder(word), y = n)) 
p +geom_bar(stat = "identity") + coord_flip()
```

# 經過TFID後得到的常用字分析
1. 我們可以看到，川普的臉書粉絲專頁中最常提到的自是America，而希拉蕊最常提到的字是Hillary(自己的名字)。
2. 以川普而言，他第二個常提及的字是自己的名字，第六個常提及的是Hillary(對手的名字，約70幾次);以希拉蕊來說，她最常提及的自己的名字(其中第十個常提及的單詞是cliton)，第二常提及Trump(對手的名字，約150幾次)。可以看出希拉蕊比川普更長在自己的粉專上提及對手。
3. 在前十大常用的單詞中，都有出現Trump以及Hillary的個人網站網址。
4. 彼此都常用的單字包括america, vote, people, live, 自己的網站，這些都是選舉必用的單字、宣傳手段(網站)。
5. 並沒有特意把Donald Trump 和 Hillary Clinton 以一個單詞統計，因為發現很多時候並不會同時出現，不過可以發現自己的名字和對手的名字是高頻單字。
6. 從文字雲看，可以看到一些競選標語 ex. Trump: makeamericagreatagain 、 americanfirst、draintheswap(改變華府生態圈的口號)、crookedhillary、imwithyou(為facebook 常用hashtag)，希拉蕊在競選標語上明顯少於川普。
7. 發現川普常用的的自為crooked(不正直的)、rigged(被操弄的)，推測和川普的選舉醜聞、攻擊希拉蕊的私人信箱事件有關等。

## 再來我們來看兩位候選人每篇貼文的回應數
### 先來看看川普的！
```{r}
# the posts id from（2016/1/1 ~ 2016/11/8)
trump_prefex_comment <- "https://graph.facebook.com/v2.12/"
trump_prefix_comment = "/comments?summary=true"
prefix_access_token = "&access_token="
trump_comments_numbers = list()
trump_comment_count <- 0

for(id in trump_posts_id){
  trump_comment_url <- paste0(trump_prefex_comment, id, trump_prefix_comment, prefix_access_token, token)
  trump_comment_res <- httr::GET(trump_comment_url)
  trump_comment <- httr::content(trump_comment_res)
  trump_comment_count <- trump_comment_count + 1
  trump_comment_number <- trump_comment$summary$total_count
  trump_comments_numbers[[trump_comment_count]] <- trump_comment_number 
}
```

### 再來看看希拉蕊的！
```{r}
# the posts id from（2016/1/1 ~ 2016/11/8)
hillary_prefex_comment <- "https://graph.facebook.com/v2.12/"
hillary_prefix_comment = "/comments?summary=true"
prefix_access_token = "&access_token="
hillary_comments_numbers = list()
hillary_comment_count <- 0

for(id in hillary_posts_id){
  hillary_comment_url <- paste0(hillary_prefex_comment, id, hillary_prefix_comment, prefix_access_token, token)
  hillary_comment_res <- httr::GET(hillary_comment_url)
  hillary_comment <- httr::content(hillary_comment_res)
  hillary_comment_count <- hillary_comment_count + 1
  hillary_comment_number <- hillary_comment$summary$total_count
  hillary_comments_numbers[[hillary_comment_count]] <- hillary_comment_number 
}
```

## 來看一下川普和希拉蕊（2016/1/1 ~ 2016/11/8)貼文的平均comment數（代表人氣
### 先看川普的
```{r}
trump_total_comments_number <- 0
for(comment_number in trump_comments_numbers){
  trump_total_comments_number <- trump_total_comments_number + comment_number
}
trump_average_comments_number <- trump_total_comments_number / length(trump_comments_numbers)
trump_average_comments_number
```
### 川普（2016/1/1 ~ 2016/11/8)貼文的平均comment數： 24719.92!!!
### 再來看希拉蕊的
```{r}
hillary_total_comments_number <- 0
for(comment_number in hillary_comments_numbers){
  hillary_total_comments_number <- hillary_total_comments_number + comment_number
}
hillary_average_comments_number <- hillary_total_comments_number / length(hillary_comments_numbers)
hillary_average_comments_number
```
### 希拉蕊（2016/1/1 ~ 2016/11/8)貼文的平均comment數： 5879.44!!!
## 川普 >> 希拉蕊
# 兩位候選人每篇貼文的回應數結果： 我們可以看出川普的留言數高於希拉蕊，在臉書上的聲望高於希拉蕊
