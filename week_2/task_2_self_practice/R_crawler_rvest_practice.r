
# install showtext
library(rvest)
library(xml2)
library(igraph)
library(ggplot2)
library(showtext)

IMDb_movie_url <- read_html("http://www.imdb.com/list/ls058982125/?sort=user_rating,desc&st_dt=&mode=detail&page=1")

movie_name_list <- IMDb_movie_url %>%
  html_nodes(".lister-item-header a") %>%
  html_text()

length(movie_name_list)

movie_rate_list <- IMDb_movie_url %>% 
  html_nodes(".ratings-imdb-rating strong") %>%
  html_text() %>%
  as.numeric()

length(movie_rate_list)

movie_vote_list <- IMDb_movie_url %>%
  html_nodes(".text-muted+ span:nth-child(2)") %>%
  html_text() 
  as.numeric()

movie_vote_list_revise <- movie_vote_list[4:103]

length(movie_vote_list_revise)

movie_time_list <- IMDb_movie_url %>% 
  html_nodes(".runtime") %>%
  html_text()

length(movie_time_list)

df_movie = data.frame(movie_name_list, movie_rate_list, movie_vote_list_revise, movie_time_list)

df_movie

df_rate_only = data.frame(movie_name_list, movie_rate_list)

df_rate_select = df_rate_only[df_rate_only["movie_rate_list"] > 7.8, ]

df_rate_select

p <-ggplot(df_rate_select, aes(movie_name_list, movie_rate_list))
p +geom_bar(stat = "identity")

library("RSQLite")

drv <- dbDriver("SQLite")

conn <- dbConnect(drv,"movie.sqlite")

dbListTables(conn)

dbWriteTable(conn, "Movies_2017", df_movie,row.names=FALSE,append=TRUE) 

dbDisconnect(conn)

drv <- dbDriver("SQLite")

conn <- dbConnect(drv,"movie.sqlite")

dbListTables(conn)

dbReadTable(conn,"Movies_2017")

dbDisconnect(conn)

## normal usage
url.main <- 'https://www.ptt.cc/bbs/studyabroad/index.html'
href.title <- html_nodes(read_html(url.main), ".title a")
# store title urls for further searching
R.hrefs <- html_attr(href.title, 'href')
# store the title
R.title.txt <- html_text(href.title)

## using pipes
# url.main <- 'https://www.ptt.cc/bbs/studyabroad/index.html'
# R.hrefs <- url.main %>% read_html() %>% html_nodes(".title a") %>% html_attr('href')

for( i in R.title.txt){
    print(i)
}

# form a empty vector
R.article.data <- c()

for(i in 1:length(R.hrefs)){
    # paste0() ==> without any white space
    article.url <- paste0('https://www.ptt.cc', R.hrefs[i])
    article <- html_nodes(read_html(article.url), "#main-content")
    article.content <- html_text(article)
    #  to convert a character vector between encodings: the ‘i’ stands for ‘internationalization’.
    article.utf8 <- iconv(article.content, 'utf8')
    # vector start counting from 1!!
    R.article.data <- c(R.article.data, article.utf8)
    # Suspend execution of R expressions for a specified time interval.
    # Sys.sleep(sample(3:5, 1))
}

R.article.data[1]

# create a session
gmail.url <- "https://accounts.google.com/signin/v2/identifier?hl=zh-TW&passive=true&continue=https%3A%2F%2Fwww.google.com.tw%2F%3Fgfe_rd%3Dcr%26dcr%3D0%26ei%3DC1upWvaXF8WiX7G7q9gD%26gws_rd%3Dssl&flowName=GlifWebSignIn&flowEntry=ServiceLogin"
gmail.session <- html_session(gmail.url)

gmail.session

gmail.form <- html_form(gmail.session)[[1]]

gmail.form

filled_form <- set_values(gmail.form, Email = "ntueeb05howard@gmail.com")

filled_form

# subnit form !!
submit_email_gmail <- submit_form(session = gmail.session, form = filled_form, submit='signIn')

submit_email_gmail

github.url <- "https://github.com/login"
github.session <- read_html(github.url)

github.form <- html_form(github.session)[[1]]

github.form

github_filled_form <- set_values(github.form, login = "ck1021051@gmail.com", password = "")

github_filled_form

github.session$url = "https://github.com/login"
github_filled_form$url = ""
submit_github <- submit_form(session = github.session, form = github_filled_form)


