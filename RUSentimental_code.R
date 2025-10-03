#-------------------------------------------------------------------------------
# Personal cheatsheet: http://briandill.com/R/
#-------------------------------------------------------------------------------

#install.packages("tidyverse")
#install.packages("tidytext")    # text mining
#install.packages("gutenbergr")  # project Gutenberg book downloads
#install.packages("textdata")

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
saveRDS(twain_book_data, "C:/Temp/R/twain_book_data.rds")
twain_book_data <- readRDS("C:/Temp/R/twain_book_data.rds")

head(twain_book_data, 15)

# add line numbers 1-N for each book
twain_book_data <- twain_book_data %>%
  group_by(gutenberg_id) %>%
  mutate(linenum = row_number())

# tokenize to one word per row (tidytext package)
twain_tokens <- twain_book_data %>%
  as_tibble() %>%                        # important: ensures tibble structure
  mutate(text = as.character(text)) %>%  # ensure text is character
  unnest_tokens(word, text, drop = TRUE) # by word, colname = "text"


# if the bing corpus has multiple entries for a word, only keep one
bing_deduped <- get_sentiments("bing") %>%
  group_by(word) %>%
  filter(!(n() > 1 & sentiment == "positive")) %>%
  ungroup()

#----- Get Twain sentiments and graph net sentiment over time -----
# inner_join sentiment to score each word
# create index for every 80 lines, spread to + & - cols and get net sentiment

# for every word, join to the sentiment list and determin its sentiment
twain_tokens %>%
  inner_join(bing_deduped, by = "word")

# using linenum in chunks of 80 as an index, count how many positive and negative words occur in each index (chunk of 80 lines)
twain_tokens %>%
  inner_join(bing_deduped, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment)  # count() lets you quickly count the unique values of one or more variables

# pivot the positive and negative counts for each index into its own column - now just one row per index
twain_tokens %>%
  inner_join(bing_deduped, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n)
  # spread(sentiment, n, fill = 0)  # deprecated function - use pivot_wider()

# create a new column "net_sentiment"
twain_tokens %>%
  inner_join(bing_deduped, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(net_sentiment = positive - negative)

# join back to twain_books data frame to get the book title
twain_tokens %>%
  inner_join(bing_deduped, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(net_sentiment = positive - negative) %>%
  inner_join(twain_books, by = "gutenberg_id")
  
# Do all the things and assign to twain_sentiment_by_index
twain_sentiment_by_index <- twain_tokens %>%
  inner_join(bing_deduped, by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(net_sentiment = positive - negative) %>%
  inner_join(twain_books, by = "gutenberg_id")

library(ggplot2)

#graph net sentiment over time (index) for each book
ggplot(twain_sentiment_by_index, aes(x = index, y = net_sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  labs(title = "Net Sentiment Over Time"
       , x = "1X = 80 lines of text"
       , y = "Net Sentiment")



# ----- top 10 sentiment words (by freq) for each book -----
twain_tokens %>%
  inner_join(bing_deduped, by = "word") %>%
  inner_join(twain_books) %>% 
  group_by(title, word) %>%
  count(word, sentiment) %>%
  arrange(title, desc(n)) %>%
  group_by(title) %>%
  top_n(10) %>% 
  View()

