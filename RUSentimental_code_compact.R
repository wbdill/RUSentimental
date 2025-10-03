#-------------------------------------------------------------------------------
# Personal cheatsheet: http://briandill.com/R/
#-------------------------------------------------------------------------------
#install.packages("tidyverse")
#install.packages("tidytext")    # text mining
#install.packages("gutenbergr")  # project Gutenberg book downloads

library(tidyverse)   # includes ggplot2 and dplyr
library(tidytext)    # text mining
library(gutenbergr)  # Project Gutenberg downloadable books.  gutenberg_metadata to view index
#-------------------------------------------------------------------------------
rm(list = ls())

# View(gutenberg_metadata)  # listing of all gutenberg books to get their gutenberg_id

# ----- Mark Twain books -----
twain_books <- gutenberg_metadata %>%
  filter(author == "Twain, Mark", has_text == TRUE) %>%
  select(gutenberg_id, author, title)

# View(twain_books)

# ----- download from gutenberg -----
twain_book_data <- gutenberg_download(c(74, 76, 86, 245, 1837, 3177))

# save locally so we don't have to re-download it in future R sessions
#saveRDS(twain_book_data, "C:/Temp/R/twain_book_data.rds")
#twain_book_data <- readRDS("C:/Temp/R/twain_book_data.rds")

# add line numbers 1-N for each book
twain_book_data <- twain_book_data %>%
  group_by(gutenberg_id) %>%
  mutate(linenum = row_number())

# tokenize to one word per row (tidytext package)
twain_tokens <- twain_book_data %>%
  as_tibble() %>%                        # important: ensures tibble structure
  mutate(text = as.character(text)) %>%  # ensure text is character
  unnest_tokens(word, text, drop = FALSE) %>%     # by word, colname = "text"
  filter(!is.na(word))

# if the bing corpus has multiple entries for a word, only keep one
bing_deduped <- get_sentiments("bing") %>%
  group_by(word) %>%
  filter(!(n() > 1 & sentiment == "positive")) %>%
  ungroup()


#----- Get Twain sentiments and graph net sentiment over time -----
# inner_join sentiment to score each word
# create index for every 80 lines, spread to + & - cols and get net sentiment
twain_sentiment_by_index <- twain_tokens %>%
  inner_join(bing_deduped, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative) %>%
  inner_join(twain_books, by = "gutenberg_id")

#graph net sentiment over time (index) for each book
ggplot(twain_sentiment_by_index, aes(x = index, y = net_sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  labs(title = "Net Sentiment Over Time", x = "1X = 80 lines of text", y = "Net Sentiment")

# ----- top 10 sentiment words (by freq) for each book -----
top_10_each_book <- twain_tokens %>%
  inner_join(bing_deduped, by = "word") %>%
  group_by(gutenberg_id, word) %>%
  count(word, sentiment) %>%
  arrange(gutenberg_id, desc(n)) %>%
  group_by(gutenberg_id) %>%
  top_n(10)

#-------------------------------------------------------------------------------
#----- Dickens books -----
rm(list = ls())

gutenberg_metadata %>%
  filter(str_detect(author, "Dickens,"), has_text == TRUE, language == "en") %>%
  select(gutenberg_id, author, title) %>%
  View()

# ----- download from gutenberg -----
dickens_book_data <- gutenberg_download(c(46, 98, 730, 766, 1400))

# save locally so we don't have to re-download it in future R sessions
#saveRDS(dickens_book_data, "C:/temp/R/dickens_book_data.rds")
dickens_book_data <- readRDS("C:/temp/R/dickens_book_data.rds")

# add line numbers by book and tokenize to one word per row
dickens_book_data <- dickens_book_data %>%
  group_by(gutenberg_id) %>%
  mutate(linenum = row_number())


dickens_tokens <- dickens_book_data %>%
  as_tibble() %>%                        # important: ensures tibble structure
  mutate(text = as.character(text)) %>%  # ensure text is character
  unnest_tokens(word, text, drop = FALSE) %>%     # by word, colname = "text"
  select(-text) %>% 
  filter(!is.na(word))

# if the bing corpus has multiple entries for a word, only keep one
bing_deduped <- get_sentiments("bing") %>%
  group_by(word) %>%
  filter(!(n() > 1 & sentiment == "positive")) %>%
  ungroup()

#----- Get sentiments and graph net sentiment over time -----
# inner_join sentiment to score each word
# create index for every 80 lines, spread to + & - cols and get net sentiment
dickens_sentiment_by_index <- dickens_tokens %>%
  inner_join(bing_deduped, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative) %>%
  inner_join(gutenberg_metadata, by = "gutenberg_id")

#graph net sentiment over time (index) for each book
ggplot(dickens_sentiment_by_index, aes(index, net_sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  labs(title = "Net Sentiment Over Time", x = "1X = 80 lines of text", y = "Net Sentiment")
