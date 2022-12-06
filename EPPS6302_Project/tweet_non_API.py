import snscrape.modules.twitter as sntwitter
import pandas as pd
import itertools

# Setting variables to be used below
maxTweets = 100000

# Creating list to append tweet data to
# sntwitter.TwitterSearchScraper('country_life since:2018-07-01 until:2019-01-30 near: "Dallas"').get_items()):
# sntwitter.TwitterSearchScraper('recent_move geocode:"{}" since:2018-10-01 until:2019-04-30'.format(loc)).get_items()):
tweets_list2 = []

# Using TwitterSearchScraper to scrape data and append tweets to list
#loc = '32.7767, -96.7970, 30km'

#users_name = ['Lizb3thJ', 'crimsonkillxr']

for i, tweet in enumerate(
        sntwitter.TwitterSearchScraper('from:tx_tsb since:2019-09-01 until:2020-05-31').get_items()):
    if i > maxTweets:
        break
    tweets_list2.append([tweet.date, tweet.id,tweet.user, tweet.coordinates, tweet.place, tweet.retweetCount, tweet.replyCount, tweet.likeCount, tweet.content, tweet.user.username])

# Creating a dataframe from the tweets list above
tweets_df2 = pd.DataFrame(tweets_list2, columns=['Datetime', 'Tweet Id', 'UserID','Coordinates', 'Place', 'Tweet Retweet Count', 'Tweet Reply Count', 'Tweet LikeCount', 'Text', 'Username'])

tweets_df2.to_csv("4450329345_2.csv", index=False)

import pandas as pd

data = pd.read_csv('4450329345_2.csv')


