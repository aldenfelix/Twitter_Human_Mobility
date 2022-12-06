library(tidytext)
library(ggpubr)
library(tidyverse)

rm(list = ls())

tweet_data<- read.csv(file ='/Users/zeyusun/Downloads/tweet_data_20_mar.csv')
tidy_tweets<- tweet_data%>% 
      filter(Tweet.Retweet.Count=="0") %>% 
    select(Tweet.Id, Text) %>% 
  unnest_tokens(word, Text)

no_numbers<- tidy_tweets %>% 
  filter(is.na(as.numeric(word)))

no_stop_words<- no_numbers %>%
  anti_join(stop_words, by = "word")

nrc<- get_sentiments("nrc")

nrc_words <- no_stop_words %>%
  inner_join(nrc, by = "word")

pie_word <- nrc_words %>%
  group_by(sentiment) %>%
  tally %>%
  arrange(desc(n))

pie_word$perc<- round(pie_word$n/sum(pie_word$n) *100)
labs<- paste0(pie_word$sentiment, "(", pie_word$perc, "%)")

ggpubr::ggpie(pie_word, "n", label = labs, fill = "sentiment", color = "white", palette = "Spectral", title = "2020-May Sentiment Analysis")
