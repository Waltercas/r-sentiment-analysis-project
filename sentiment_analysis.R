library(tidytext)
library(tidyverse)
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text_df<- tibble(line= 1:4, text=text)
text_df %>%
  unnest_tokens(word, text)

library(janeaustenr)
library(tidyverse)
library(tidytext)
library(wordcloud2)
austen_books()
austen_books() %>% 
  distinct(book)
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number()) %>%         # identify line numbers
  ungroup()

original_books
tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

matchwords_books <- tidy_books %>%
  anti_join(get_stopwords())

stopwords::stopwords_getsources()
stopwords::stopwords_getlanguages("snowball")
stopwords_custom <- tribble(~word, ~lexicon,
                            "farfegnugen", "custom")
get_stopwords(source = "snowball")
bind_rows(get_stopwords(), stopwords_custom)    # The default is "snowball"

matchwords_books %>% 
  # distinct(word)
  count(word, sort = TRUE) 

matchwords_books %>%
  count(word, sort = TRUE) %>%
  head(100) %>% 
  wordcloud2(size = .4, shape = 'triangle-forward', 
             color = c("steelblue", "firebrick", "darkorchid"), 
             backgroundColor = "salmon")

matchwords_books %>%
  count(word) %>%
  with(wordcloud::wordcloud(word, n, max.words = 100))

pride_prej_novel <- tibble(text = prideprejudice) %>% 
  mutate(line = row_number())
tidy_pride_prej <-pride_prej_novel %>%
  unnest_tokens(word, text)
tidy_pride_prej

matchwords_pride <- tidy_pride_prej %>%
  anti_join(get_stopwords())

matchwords_pride %>% 
  # distinct(word)
  count(word, sort = TRUE) 

matchwords_books %>%
  count(word) %>%
  with(wordcloud::wordcloud(word, n, max.words = 100))

positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")   

tidy_books %>%
  filter(book == "Emma") %>%                        # only the book _emma_
  semi_join(positive) %>%                           # semi_join()
  count(word, sort = TRUE)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book)









