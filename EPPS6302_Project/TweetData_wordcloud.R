library(wordcloud2)
library(tm)
library(dplyr)

rm(list = ls())

exportWordCloud<- function(tweet_category){
#tweet_data<- read.csv(file ='/Users/zeyusun/Downloads/tweet_data_22_may.csv')
tweet_data<- tweets_topT_p3  
tweets<- tweet_data
tweets <- tweet_data %>% select("Text")

tweets$Text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "",tweets$Text) # remove alphanumeric characters 
tweets$Text <- gsub("https\\S*", "",tweets$Text) # remove hyperlinks
tweets$Text <- gsub("amp", "",tweets$Text) # amp just keeps showing up, remove it!!
tweets.corpus <- Corpus(VectorSource(tweets$Text))

tweets.corpus <- tweets.corpus %>%
  tm_map(removeNumbers) %>% # removes numbers from text
  tm_map(removePunctuation) %>% # removes punctuation from text
  tm_map(stripWhitespace) %>% # trims the text of whitespace
  tm_map(content_transformer(tolower)) %>% # convert text to lowercase
  tm_map(removeWords,stopwords("english")) %>% # remove stopwords
  tm_map(removeWords,stopwords("SMART")) # remove stopwords not removed from previous line

tdm <- TermDocumentMatrix(tweets.corpus) %>% # create a term document matrix
  as.matrix()

words <- sort(rowSums(tdm), decreasing = TRUE) # count all occurences of each word and group them
df <- data.frame(word = names(words), freq = words) # convert it to a dataframe
head(df)

set.seed(1234) # for reproducibility, sorta
wcloud <- wordcloud2(df,   # generate word cloud
                     size = 1.5,
                     color= 'random-dark', # set colors
                     shape = 'pentagon',
                     rotateRatio = 0) #horizontal looks better, but what do you think?
wcloud
}                                   