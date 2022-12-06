library(dplyr)

#data cleaning to get the toptravellers and the least travelers
toptraveler_data<- read.csv(file ='/Users/zeyusun/Downloads/toptravelers.csv')
tweet_data<- read.csv(file ='/Users/zeyusun/Downloads/paper_data/combined_full.csv')


toptraveler_data_most_travel<- subset(toptraveler_data, toptraveler_data$uniqueplaces >120)
least_traveler<- subset(toptraveler_data, toptraveler_data$uniqueplaces <5 & toptraveler_data$uniqueplaces>1)

tweets_topT<- tweet_data[tweet_data$Username %in% toptraveler_data_most_travel$Username,]
tweets_topT$date<- strptime(tweets_topT$Datetime, "%Y-%m-%d")
tweets_topT$year<- format(tweets_topT$date, format = "%Y")
tweets_topT$month<- format(tweets_topT$date, format = "%m")

tweets_topT_p1<- subset(tweets_topT, year == 2018)
tweets_topT_p2<- subset(tweets_topT, year>2018 & year<2021)
tweets_topT_p3<- subset(tweets_topT, year >= 2021)

tweets_leastT<- tweet_data[tweet_data$Username %in% least_traveler$Username,]
tweets_leastT$date<- strptime(tweets_leastT$Datetime, "%Y-%m-%d")
tweets_leastT$year<- format(tweets_leastT$date, format = "%Y")
tweets_leastT$month<- format(tweets_leastT$date, format = "%m")

tweets_leastT_p1<- subset(tweets_leastT, year == 2018)
tweets_leastT_p2<- subset(tweets_leastT, year>2018 & year<2021)
tweets_leastT_p3<- subset(tweets_leastT, year >= 2021)

#rank the most posted tweets by username
df<- as.data.frame(table(tweet_data$Username))
names(df)<- c("Username", "times")
df$rank<- rank(-df$times, ties.method="min")
df <- df[order(df$rank,decreasing = F),]


