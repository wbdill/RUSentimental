#-------------------------------------------------------------------------------
# Personal cheatsheet: http://briandill.com/R/
#-------------------------------------------------------------------------------

install.packages("tidyverse")
#install.packages("tidytext")    # text mining
install.packages("gutenbergr")  # project Gutenberg book downloads
install.packages("textdata")

library(tidyverse)
library(tidytext)    # text mining
library(gutenbergr)  # Project Gutenberg downloadable books.  gutenberg_metadata to view index
library(textdata)
#-------------------------------------------------------------------------------
rm(list = ls())

#----- How do we rate sentiment? -----
# https://www.tidytextmining.com/
?get_sentiments

get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("loughran")

gutenberg_metadata # listing of all gutenberg books to get their gutenberg_id

View(gutenberg_metadata)


# ----- Mark Twain books -----
twain_books <- gutenberg_metadata %>%
  filter(author == "Twain, Mark", has_text == TRUE) %>%
  select(gutenberg_id, author, title)

View(twain_books)

# Top 10 books list: https://www.publishersweekly.com/pw/by-topic/industry-news/tip-sheet/article/64432-the-10-best-mark-twain-books.html
# pick 6 popular books identified by gutenberg_id
gutenberg_metadata %>%
  filter(gutenberg_id %in% c(74, 76, 86, 245, 1837, 3177)) %>%
  select(gutenberg_id, title, author)

# ----- download from gutenberg -----
twain_book_data <- gutenberg_download(c(74, 76, 86, 245, 1837, 3177))


# save locally so we don't have to re-download it in future R sessions
saveRDS(twain_book_data, "~/Documents/R/twain_book_data.rds")
twain_book_data <- readRDS("~/Documents/R/twain_book_data.rds")

head(twain_book_data, 15)

# add line numbers 1-N for each book
twain_book_data <- twain_book_data %>%
  group_by(gutenberg_id) %>%
  mutate(linenum = row_number()) %>% 
  ungroup()

# tokenize to one word per row (tidytext package)
twain_tokens <- twain_book_data %>%
  unnest_tokens(word, text, drop = TRUE)    # by word, colname = "text"
#----- Get Twain sentiments and graph net sentiment over time -----
# inner_join sentiment to score each word
# create index for every 80 lines, spread to + & - cols and get net sentiment
get_sentiments("bing") %>% group_by(word) %>% tally() %>% filter(n > 1)
get_sentiments("bing") %>% filter(word %in% c('envious', 'enviously'))
sentiments_bing <- get_sentiments("bing")
sentiments_bing <- sentiments_bing %>% distinct(word, .keep_all = TRUE)

twain_tokens %>%
  inner_join(sentiments_bing, by = "word")

twain_tokens %>%
  inner_join(sentiments_bing, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment)

twain_tokens %>%
  inner_join(sentiments_bing, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0)

twain_tokens %>%
  inner_join(sentiments_bing, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative)

twain_tokens %>%
  inner_join(sentiments_bing, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative) %>%
  inner_join(twain_books, by = "gutenberg_id")
  
# Do all the things and assign to twain_sentiment_by_index
twain_sentiment_by_index <- twain_tokens %>%
  inner_join(sentiments_bing, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative) %>%
  inner_join(twain_books, by = "gutenberg_id")

library(ggplot2)

#graph net sentiment over time (index) for each book
ggplot(twain_sentiment_by_index, aes(x = index, y = net_sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  labs(title = "Net Sentiment Over Time", x = "1X = 80 lines of text", y = "Net Sentiment")



# ----- top 10 sentiment words (by freq) for each book -----
top_10_each_book <- twain_tokens %>%
  inner_join(sentiments_bing, by = "word") %>%
  group_by(gutenberg_id, word) %>%
  count(word, sentiment) %>%
  arrange(gutenberg_id, desc(n)) %>%
  group_by(gutenberg_id) %>%
  top_n(10)



#-------------------------------------------------------------------------------
#----- Dickens books -----
rm(list = ls())

gutenberg_metadata %>%
  #filter(str_detect(author, "Dickens,"), has_text == TRUE, language == "en") %>%
  filter(str_detect(author, "Poe,"), has_text == TRUE, language == "en") %>%
  select(gutenberg_id, author, title) %>%
  View()

# ----- download from gutenberg -----
dickens_book_data <- gutenberg_download(c(46, 98, 730, 766, 1400))

# save locally so we don't have to re-download it in future R sessions
saveRDS(dickens_book_data, "~/Documents/R/dickens_book_data.rds")
dickens_book_data <- readRDS("~/Documents/R/dickens_book_data.rds")

# add line numbers by book and tokenize to one word per row
dickens_tokens <- dickens_book_data %>%
  group_by(gutenberg_id) %>%
  mutate(linenum = row_number()) %>%
  ungroup() %>% 
  unnest_tokens(word, text)    # by word, colname = "text"
  
#----- Get sentiments and graph net sentiment over time -----
# inner_join sentiment to score each word
# create index for every 80 lines, spread to + & - cols and get net sentiment
sentiments_bing <- get_sentiments("bing")
sentiments_bing <- sentiments_bing %>% distinct(word, .keep_all = TRUE)


dickens_sentiment_by_index <- dickens_tokens %>%
  inner_join(sentiments_bing, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative) %>%
  inner_join(gutenberg_metadata, by = "gutenberg_id")

library(ggplot2)

#graph net sentiment over time (index) for each book
ggplot(dickens_sentiment_by_index, aes(index, net_sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  labs(title = "Net Sentiment Over Time", x = "1X = 80 lines of text", y = "Net Sentiment")

#----- Edgar Allen Poe -----
poe_book_data <- gutenberg_download(c(932,1063,1065))

saveRDS(poe_book_data, "~/Documents/R/poe_book_data.rds")
poe_book_data <- readRDS("~/Documents/R/poe_book_data.rds")

poe_tokens <- poe_book_data %>%
  group_by(gutenberg_id) %>%
  mutate(linenum = row_number()) %>%
  ungroup() %>% 
  unnest_tokens(word, text)    # by word, colname = "text"

poe_sentiment_by_index <- poe_tokens %>%
  inner_join(sentiments_bing, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 20, sentiment) %>% # fewer lines per index
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative) %>%
  inner_join(gutenberg_metadata, by = "gutenberg_id")


ggplot(poe_sentiment_by_index, aes(index, net_sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  labs(title = "Net Sentiment Over Time", x = "1X = 20 lines of text", y = "Net Sentiment")
