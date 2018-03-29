
vignette("selectorgadget")

library(rvest)
library(magrittr)

lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

lego_movie

##%>% is defined by package magrittr(CRAN). Heavily used by dplyr(CRAN)
rating <- lego_movie %>%
  html_nodes("strong span")
rating

cast <- lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast

poster <- lego_movie %>%
  html_nodes(".poster img") %>%
  html_attr("src")
poster

#titleImageStrip .loadlate
Photos <- lego_movie %>% 
  html_nodes("#titleImageStrip .loadlate") %>%
  html_attr("loadlate")

Photos

# http://stackoverflow.com/questions/15853204
s <- html_session("http://hadley.nz")
s %>% jump_to("hadley-wickham.jpg") %>% jump_to("/") %>% session_history()
s %>% jump_to("hadley-wickham.jpg") %>% back() %>% session_history()
s %>% follow_link(css = "#about a:nth-child(2)")

a <- html_session("https://zh.wikipedia.org/wiki/ONE_PIECE%E6%B5%B7%E8%B3%8A%E5%88%97%E8%A1%A8")

a %>% jump_to("https://zh.wikipedia.org/wiki/%E8%92%99%E5%85%B6%C2%B7D%C2%B7%E9%AD%AF%E5%A4%AB") %>% back() %>% session_history()

a %>% follow_link(css = "dt a")

onepiece.main.url <- read_html("https://zh.wikipedia.org/wiki/ONE_PIECE%E6%B5%B7%E8%B3%8A%E5%88%97%E8%A1%A8#%E8%8D%89%E5%B8%BD%E5%A4%A7%E8%88%B9%E5%9C%98")

onepiece.chracters.url <- onepiece.main.url %>% html_nodes("dt a") %>% html_attr("href")
onepiece.charaters.txt <- onepiece.main.url %>% html_nodes("dt a") %>% html_text()

onepiece.chracters.url

onepiece.charaters.txt

# create target_website_form :
search <- html_form(read_html("http://www.google.com"))[[1]]
# create target_website_session
google_session <- html_session("http://www.google.com")
a <- set_values(search, q = "My little pony")
# set_values(search, hl = "fr")
## Not run: set_values(search, btnI = "blah")

search

result_session = submit_form(session = google_session, form = a)

result_session
