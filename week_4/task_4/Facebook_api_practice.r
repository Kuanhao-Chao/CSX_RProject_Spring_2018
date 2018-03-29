
#先把需要的package執行看存不存在
library(httr)
library(rjson)
library(httpuv)
library(Rfacebook)

library(plyr)
library(nlr)

token <- "EAACEdEose0cBAOyMh4jDcwy0qxO2mKP4CriYzYoZAjJQm37C22WiZC1eQlhiCvyjjCfdZCZAGZAMGFHMlUneSxaBhdj03TCdy35ZCQoWbG9kypLZC9qiA99B47VJEKZCVr4xzzpLSlS9IZAOxQIXSkmycjpTGbkM36ughHZCKD6QJWvVSz4BUefrtt7pMLmv8nY5ouHvkA2kUDMAZDZD"
prefex_post <- "https://graph.facebook.com/v2.12/me/?fields=posts&access_token="
url <- paste0(prefex_post, token)
# Get the response of the url (ex. posts)
res <- httr::GET(url)
# Get the content of the url
posts <- content(res)

res

for( x in 1:16){
    print(posts$posts$data[[x]]$message)
}

me <- getUsers("me", token, private_info = TRUE)
me$name

prefex_me <- "https://graph.facebook.com/v2.12/dXNlcl9jb250ZAXh0OgGQNe5zws7pL4orKZAUj0IozaP2PZBPencXCNLENCnWXnhavvE0BZBI7yRH8keorFJRghjVfeePOzljewhIqICEeD1byr9snTuZAmGuqtj93DzuJ9AZD/mutual_likes?pretty=0&limit=25&after=MTM5MDU4NTE5NDU3NDAxNwZDZD&access_token="
url_me <- paste0(prefex_me, token)
# Get the response of the url (ex. posts)
res_me <- httr::GET(url_me)
# Get the content of the url
posts_me <- content(res_me)

data_list <- vector()

for(x in 1:25){
    data_list <- c(data_list, posts_me$data[[x]]$name)
}
a <- data.frame(data_list)
colnames(a) <- c("Liked paged")
url_next <- posts_me$paging$`next`
res_next <- httr::GET(url_next)
posts_me <- content(res_next)

a

token <- "EAACEdEose0cBAOyMh4jDcwy0qxO2mKP4CriYzYoZAjJQm37C22WiZC1eQlhiCvyjjCfdZCZAGZAMGFHMlUneSxaBhdj03TCdy35ZCQoWbG9kypLZC9qiA99B47VJEKZCVr4xzzpLSlS9IZAOxQIXSkmycjpTGbkM36ughHZCKD6QJWvVSz4BUefrtt7pMLmv8nY5ouHvkA2kUDMAZDZD"
prefex_post <- "https://graph.facebook.com/v2.12/10150145806225128/posts?limit=1&until=2016-1-17&since=2015-11-01&access_token="
url <- paste0(prefex_post, token)
# Get the response of the url (ex. posts)
res <- httr::GET(url)
# Get the content of the url
posts <- httr::content(res)

# to check url
url

# to check whether the link is valid
res

# convert posts data into
# unlist(given a list structure, simplifies it to produce a vector contains 
#   all the atomic components with occurs in x)
groups <- matrix(unlist(posts$data))

groups

# save the first post into 1.txt
count <- 1
filename <- paste0("data/", count, ".txt")
write.table(groups, filename)

next_page = posts$paging$cursors$after

next_page

next_flg <- posts$paging$`next`

next_flg

# iteratively save the posts into number.txt
while(next_flg != 'NULL'){
#     token <<- "EAACEdEose0cBAP29G74zpR497rG3dxRqZCMvWMh9R4QKgW3s0M2MjneBpHu2ibg5WmhWEZA90qTJNirjZBZBpus9NP3F0CUfO4VSinJJkBjqCiuM56DQVeTxezlRlMeF4fBbhAE2zMOrua06qJshw6UfiB78yVJZCAY8mXPNftrJz6FVYlRyKdHhMu92SJ25T7yZAIMcYZCTwZDZD"
    url <- paste0("https://graph.facebook.com/v2.12/10150145806225128/posts?limit=1&until=2016-1-17&since=2015-11-01&after=", next_page, "&access_token=", token)
    # Get the response of the url (ex. posts)
    res <- httr::GET(url)
    # Get the content of the url
    posts <- httr::content(res)
    count <- count + 1
    next_page_data <- matrix(unlist(posts$data))
    next_page <- posts$paging$cursors$after
    next_flg <- posts$paging$`next`
    filename <- paste0("data/", count, ".txt")
    write.table(next_page_data, filename)
}

library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
library("corpus")

paste0(getwd(), '/data')

setwd(paste0(getwd(), '/data'))

filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)

docs <- tm_map(docs,toSpace,"V1")
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "1")
docs <- tm_map(docs,toSpace, "的")
docs <- tm_map(docs,toSpace, "及")
docs <- tm_map(docs,toSpace, "為")
docs <- tm_map(docs,toSpace, "是")
docs <- tm_map(docs,toSpace, "在")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

mixseg = worker()
# segment <- c("陳菊","布里斯本","高雄","重劃區","合作會","後勁溪")
# new_user_word(mixseg,segment)

jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}

font_family <- par("family") # the previous font family
par(family = "STSong") # change to a nice Chinese font
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
#畫出文字雲
wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=3,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

mlb <-table(cc[groups])

ans = data.frame(mlb)

ans

head(ans[order(ans$Freq, decreasing = TRUE),])

library(wordcloud2)

library("corpus")

set.seed(100)

cstops <- "https://raw.githubusercontent.com/ropensci/textworkshop17/master/demos/chineseDemo/ChineseStopWords.txt"
csw <- paste(readLines(cstops, encoding = "UTF-8"), collapse = "\n") # download
csw <- gsub("\\s", "", csw)           # remove whitespace
stop_words <- strsplit(csw, ",")[[1]] # extract the comma-separated words

# Create volatile corpora.
docs <- VCorpus(DataframeSource(groups[2]))

inspect(docs)
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removePunctuation)
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "年")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "很")
docs <- tm_map(docs, toSpace, "都")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "和")
docs <- tm_map(docs, toSpace, "他")
docs <- tm_map(docs, toSpace, "為")
docs <- tm_map(docs, toSpace, "跟")
docs <- tm_map(docs, toSpace, "就")
docs <- tm_map(docs, toSpace, "也")
docs <- tm_map(docs, toSpace, "與")
docs <- tm_map(docs, toSpace, "以")
docs <- tm_map(docs, toSpace, "讓")
docs <- tm_map(docs, toSpace, "日")
docs <- tm_map(docs, toSpace, "月")
docs <- tm_map(docs, toSpace, "到")
for(i in seq(docs)){
  docs[[i]]<-gsub('[[:punct:]]', '', docs[[i]])
  docs[[i]]<-gsub("，"," ",docs[[i]])
  docs[[i]]<-gsub("-"," ",docs[[i]])
}

par(family="Didot")#讓文字顯示成中文
wordcloud(ans$Var1, ans$Freq, min.freq=3, random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(ans))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
