# Import libraries
library(NLP)
library(readtext)
library(tm)
library(jiebaR)         # Chinese text segmentation, keyword extraction and speech tagging
library(jiebaRD)        # Chinese Text Segmentation Data for jiebaR Package.
library(tidyverse)      # opinionated collection of R packages designed for data science
library(stringr)        # text cleaning and regular expression
library(harrypotter)     # provide full texts of the first seven Harry Potter books.
library(dplyr)
library(janeaustenr)
library(tidytext)
library(wordcloud2)

# Text tidying!
# Tibbles are a modern take on data frames. Keep the features that have stood the test of time, and drop the features that used to be convenient but are now frustrating
text_data_frame <- tibble(chapter = seq_along(philosophers_stone),
                          text = philosophers_stone)              # smart way to take data frame(x and y)

text_data_frame %>% 
  unnest_tokens(word, text)

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

# store the content of the book
books_content <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
                      goblet_of_fire, order_of_the_phoenix, half_blood_prince,
                      deathly_hallows)
# now series is an empty data frame
series <- tibble()

# seq_along : generate a sequence that is as long as the vector
# difference between sequence and sequence along: https://stackoverflow.com/questions/13732062/what-are-examples-of-when-seq-along-works-but-seq-produces-unintended-results
for(i in seq_along(titles)) { # 1 ~ 7
  # create from column vectors
  clean <- tibble(chapter = seq_along(books_content[[i]]),
                  text = books_content[[i]]) %>%
    unnest_tokens(word, text) %>%      # seperate the content into words
    # add a new column
    mutate(books_content = titles[i]) %>%
    # choose the column
    select(books_content, everything())
  # add together! to a single data frame
  series <- rbind(series, clean)
}
colnames(series) <- c("books_title", "chapter", "word")
series$books_title <- factor(series$books_title, levels = rev(titles))
# now ~ find the word frequency
# smart~ it has stio_words build in~ just remove those word
# the first aproach!
series_word_freq <- series %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 20)
series_word_freq

wordcloud2(data = series_word_freq, size=0.5)

# Find the most frequently used word in each book
series_word_freq_each_book <- series %>%
  anti_join(stop_words) %>%
  group_by(books_title) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(book = factor(books_title),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")
series_word_freq_each_book

# calculate percent of word use across all novels
potter_pct <- series %>%
  anti_join(stop_words) %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

# calculate percent of word use within each novel
frequency <- series %>%
  anti_join(stop_words) %>%
  count(books_title, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(potter_pct) %>%
  arrange(desc(book_words)) %>%
  ungroup()
