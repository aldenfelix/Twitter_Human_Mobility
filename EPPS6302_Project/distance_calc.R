library(tidyverse)
library(lubridate)
library(SDMTools)
library(RColorBrewer)
tweets <- read.csv("combined_full.csv")

# extract lat and long
tweets <- tweets %>% filter(Coordinates != "")
tweets <- subset(tweets, select = c("Datetime", "Tweet.Id", "Coordinates", 
                                    "Username"))

tweets <- subset(tweets, tweets$Datetime < "2022-07-01", select = Datetime:Username)

tweets$Coordinates <- gsub("Coordinates", "",
                           gsub("\\(", "",
                                gsub("\\)", "",
                                     gsub("longitude", "",
                                          gsub("latitude", "",
                                               gsub("=", "", tweets$Coordinates))))))

tweets <- separate(tweets, col = Coordinates,
                   into = c("longitude", "latitude"),
                   sep = c(","))
tweets$longitude <- as.numeric(tweets$longitude)
tweets$latitude <- as.numeric(tweets$latitude)

tweets$Datetime <- as_date(tweets$Datetime)

# rearrange tweets by username and date
tweets <- tweets %>% arrange(Username, desc(Datetime))

# break tweets into the three data periods
period1 <- tweets %>% filter(Datetime < "2018-11-01")
period2 <- tweets %>% filter(Datetime > "2019-08-31" & 
                               Datetime < "2020-06-01")
period3 <- tweets %>% filter(Datetime > "2021-09-30" & 
                               Datetime < "2022-07-01")


tweets$period <- c('') # takes forever to run omg
for (i in 1:nrow(tweets)) {
  date <- tweets$Datetime[i]
  if (date < "2018-11-01") {
    tweets$period[i] <- "Period 1"   # min date is 2018-03-01
  } else if (date > "2019-08-31" & 
             date < "2020-06-01") {
    tweets$period[i] <- "Period 2"
  } else if (date > "2021-09-30"  & 
             date < "2022-07-01") {
    tweets$period[i] <- "Period 3"
  } else {
    tweets$period[i] <- "between"
  }
}


options(scipen=999) # turn off scientific notation its annoying

# function #1:
# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

# function #2:
# Calculates the geodesic distance between two points specified by radian
# latitude/longitude using the Haversine formula
# from: https://www.r-bloggers.com/2010/11/great-circle-distance-calculations-in-r/
distance_hav <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

# function 3: actually calculates it
# distance function : accepts dataframe as argument
dist <- function(pd) {
  # creates list the same size as dataframe
  list <- rep(0, each = nrow(pd))
  # iterates through the rows of the dataframe
  for (j in 1:(nrow(pd)-1)) {
    # check if neighboring tweets are from the same user
    if (isTRUE(pd$Username[[j]] == pd$Username[[j+1]])) {
      long1 <- deg2rad(pd$longitude[[j]])
      long2 <- deg2rad(pd$longitude[[j+1]])
      lat1 <- deg2rad(pd$latitude[[j]])
      lat2 <- deg2rad(pd$latitude[[j+1]])
      # check if neighboring tweets have different locations
      if (isTRUE(long2-long1 == 0 & lat2-lat1 == 0)) {
        # if the locations are the same just feed out a 0
        # running the same locations through distance_hav makes it break because r sucks
        out <- 0
      } else {
        out <- distance_hav(long1, lat1, long2, lat2)
      }
      list[j] <- out #output is in kilometers
    }
  }
  return(list)
}

# the value represents the distance between the current and previous
# tweet from the same user in kilometers
period1$Traveled <- dist(period1)
period2$Traveled <- dist(period2)
period3$Traveled <- dist(period3)
tweets$Traveled <- dist(tweets)


### adds the cumulative distance for each user for each period
# for test period
sumdist <- function (pd) {
  sumlist <- rep(0, each = (nrow(pd)))
  for (i in (nrow(pd)-1):1) {
    if (pd$Username[i] == pd$Username[i+1]) {
      sumlist[i] <- (pd$Traveled[i] + sumlist[i+1])
    }
  }
  return(sumlist)
}


period1$SumTraveled <- sumdist(period1)  
period2$SumTraveled <- sumdist(period2)
period3$SumTraveled <- sumdist(period3)
tweets$SumTraveled <- sumdist(tweets)


# save the dataframes after manipulating them
saveRDS(period1, "period1.rds")
saveRDS(period2, "period2.rds")
saveRDS(period3, "period3.rds")
saveRDS(tweets, "tweets.rds")


# unique usernames in each period:
length(unique(tweets$Username)) # 296
length(unique(period1$Username)) # 147
length(unique(period2$Username)) # 182
length(unique(period3$Username)) # 258

# list of users that tweeted in all periods:
unique1 <- unique(period1$Username)
unique2 <- unique(period2$Username)
unique3 <- unique(period3$Username)

m1 <- match(unique1, unique2) # m1[i] is index of unique2 that matched value of unique1[i]
m2 <- match(unique2, unique3) # m1 is list of indexes from unique2

lista <- rep('', each=length(m1)) # users who tweeted in period 1 + 2
for (i in 1:length(m1)) {         # but maybe not 3
  lista[i] <- unique2[m1[i]]
}
lista <- lista[!is.na(lista)]

listb <- rep('', each=length(m2)) # users who tweeted in period 2 + 3
for (i in 1:length(m2)) {         # but maybe not 1
  listb[i] <- unique3[m2[i]]
}
listb <- listb[!is.na(listb)]

listab <- match(lista, listb)     # users in both list a and list b
uniquename <- rep('', each=length(listab)) # tweeted in all periods
for (i in 1:length(listab)) {
  uniquename[i] <- listb[listab[i]]
}
uniquename <- uniquename[!is.na(uniquename)] # 112 people
uniquename                                  # tweeted in all periods

saveRDS(uniquename, "uniquename.rds")


##### some good old data analysis
# gets greatest distance traveled in one tweet for each uniquename
travfunc <- function (pd) {
  numlist <- rep(0, each = nrow(uniquename))
  for (i in 1:length(uniquename)) {
    numlist[i] <- max(pd$Traveled[pd$Username == uniquename[i]])
  }
  return(numlist)
}
# gets sum of distance traveled over all time for each uniquename
travcum <- function (pd) {
  numlist <- rep(0, each = nrow(uniquename))
  for (i in 1:length(pd)) {
    numlist[i] <- max(pd$SumTraveled[pd$Username == uniquename[i]])
  }
  return(numlist)
}


compare <- data.frame(
  Username = uniquename,
  AvgTraveled1 = ,
  AvgTraveled2 = ,
  AvgTraveled3 = ,
  MedTraveled1 = ,
  MedTraveled2 = ,
  MedTraveled3 = ,
  MaxTraveled1 = travfunc(period1),
  MaxTraveled2 = travfunc(period2),
  Maxtraveled3 = travfunc(period3),
  SumTraveled1 = travcum(period1),
  SumTraveled2 = travcum(period2),
  SumTraveled3 = travcum(period3)
)


# traveled between tweets - median maxium 
mean(compare1$Traveled) # 2449.121
mean(compare2$Traveled) # 2357.797
mean(compare3$Traveled) # 1696.805

median(compare1$Traveled) # 1700.982
median(compare2$Traveled) # 1632.23
median(compare3$Traveled) # 1390.569

# traveled total
mean(compare1$SumTraveled) # 216401.09
mean(compare2$SumTraveled) # 20655.78
mean(compare3$SumTraveled) # 31451.71

median(compare1$SumTraveled) # 13755.41
median(compare2$SumTraveled) # 13978.08
median(compare3$SumTraveled) # 16425.94

############################################################
################## OH BOY! PLOTS! ##########################
############################################################

# award for most distance between two consecutive tweets goes to
max(tweets$Traveled)     # 15,601.45 km
tweets$Username[tweets$Traveled == max(tweets$Traveled)]
# mobfreshsupply

# award for greatest cumulative distance across all periods goes to
max(tweets$SumTraveled)     # 1,916,751 km
tweets$Username[tweets$SumTraveled == max(tweets$SumTraveled)]
# "cokymendoza"

# award for most tweets
(tweets %>% count(Username, sort = TRUE)) # "badhopper


############# visualization 
# distance traveled between two tweets for selected user in 
# selected period
badhopper <- tweets %>% filter(Username == "badhopper")
mobfreshsupply <- tweets %>% filter(Username == "mobfreshsupply")
cokymendoza <- tweets %>% filter(Username == "cokymendoza")
Bananadogs3 <- tweets %>% filter(Username == "Bananadogs3")
Wobble_Master <- tweets %>% filter(Username == "Wobble_Master")
thedorsenator <- tweets %>% filter(Username == "thedorsenator")
NotTheFakeDome <- tweets %>% filter(Username == "NotTheFakeDome")
martymar_214 <- tweets %>% filter(Username == 'martymar_214')
dramatic_one <- tweets %>% filter(Username == 'dramatic_one')
AdamGordon1977 <- tweets %>% filter(Username == 'AdamGordon1977')

p <- ggplot() +
  coord_cartesian(ylim = c(0, 10)) + 
  scale_x_date(limits = c(as.Date("2018-01-01"), 
                          as.Date("2022-07-01"))) +
  geom_hline(yintercept = log(10), color = "gray92",
             show.legend = TRUE) +
  geom_hline(yintercept = log(100), color = "gray92",
             show.legend = TRUE) +
  geom_hline(yintercept = log(1000), color = "gray92",
             show.legend = TRUE) +
  geom_hline(yintercept = log(10000), color = "gray92",
             show.legend = TRUE) +
  theme_classic() + 
  scale_color_brewer(palette = "Set2", name = "Period") +
  labs(title = "Distance From Last Tweet: Twitter User badhopper",
       subtitle = "Wins the Award For: Most Tweets")+
  xlab("Time") +
  ylab("Distance From Last Tweet (log km)")

# shade period 1
p <- p + geom_rect(aes(xmin = as_date('2018-03-01'),
                       xmax = as_date('2018-10-31'),
                       ymin = -2,
                       ymax = 12), 
                   alpha = 0.2,
                   fill = "#B3E2CD")

# shade period 2
p <- p + geom_rect(aes(xmin = as_date('2019-09-01'),
                       xmax = as_date('2020-05-31'),
                       ymin = -Inf,
                       ymax = Inf), 
                   fill = "#FDCDAC", 
                   alpha = 0.2)

# shade period 3
p <- p + geom_rect(aes(xmin = as_date('2021-10-01'),
                       xmax = as_date('2022-06-30'),
                       ymin = -Inf,
                       ymax = Inf), 
                   fill = "#CBD5E8", 
                   alpha = 0.2)

p
p + geom_point(data = cokymendoza, 
               aes(Datetime, log(Traveled), color = period))


############# cumulative distance plots
p <- ggplot() +
  scale_x_date(limits = c(as.Date("2018-01-01"), 
                          as.Date("2022-07-01"))) +
  theme_classic() + 
  scale_color_brewer(palette = "Set2", name = "Username") +
  labs(title = "Distance From Last Tweet: Running Total",
       subtitle = "For All Periods")+
  xlab("Time") +
  ylab("Total Kilometers Traveled")

# shade period 1
p <- p + geom_rect(aes(xmin = as_date('2018-03-01'),
                       xmax = as_date('2018-10-31'),
                       ymin = -Inf,
                       ymax = Inf), 
                   alpha = 0.2,
                   fill = "#B3E2CD")

# shade period 2
p <- p + geom_rect(aes(xmin = as_date('2019-09-01'),
                       xmax = as_date('2020-05-31'),
                       ymin = -Inf,
                       ymax = Inf), 
                   fill = "#FDCDAC", 
                   alpha = 0.2)

# shade period 3
p <- p + geom_rect(aes(xmin = as_date('2021-10-01'),
                       xmax = as_date('2022-06-30'),
                       ymin = -Inf,
                       ymax = Inf), 
                   fill = "#CBD5E8", 
                   alpha = 0.2)


p
p + geom_line(data = badhopper,
              aes(Datetime, SumTraveled, color = Username)) + 
  geom_line(data = cokymendoza, 
            aes(Datetime, SumTraveled, color = Username)) + 
  geom_line(data = mobfreshsupply, 
            aes(Datetime, SumTraveled, color = Username)) + 
  geom_line(data = Bananadogs3, 
            aes(Datetime, SumTraveled, color = Username)) + 
  geom_line(data = Wobble_Master, 
            aes(Datetime, SumTraveled, color = Username))

p + geom_line(data = thedorsenator,
              aes(Datetime, SumTraveled, color = Username)) + 
  geom_line(data = NotTheFakeDome, 
            aes(Datetime, SumTraveled, color = Username)) + 
  geom_line(data = martymar_214, 
            aes(Datetime, SumTraveled, color = Username)) + 
  geom_line(data = dramatic_one, 
            aes(Datetime, SumTraveled, color = Username)) + 
  geom_line(data = AdamGordon1977, 
            aes(Datetime, SumTraveled, color = Username))




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # #  cutting room floor # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

in_num <- sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)
out <- acos(in_num)

tempdf <- period2 %>% filter(Username == "gbowner", .preserve = TRUE)
testdf <- rep(0, each = nrow(tempdf))
for (j in 1:(nrow(tempdf)-1)) {
  testdf[j] <- distance_calc(long1 = tempdf$longitude[j],
                             lat1 = tempdf$latitude[j],
                             long2 = tempdf$longitude[j+1],
                             lat2 = tempdf$latitude[j+1])
}

### jank distance function, threw errors out of malice
### update: friendship restored with distance function, it was rs fault
distance_val <- function(pd) {
  list <- c()
  for (i in unique(pd$Username)) {
    tempdf <- pd %>% filter(Username == i, .preserve = TRUE)
    templist <- rep(0, each = nrow(tempdf))
    for (j in 1:(nrow(tempdf)-1)) {
      long1 <- deg2rad(tempdf$longitude[j])
      long2 <- deg2rad(tempdf$longitude[j+1])
      lat1 <- deg2rad(tempdf$latitude[j])
      lat2 <- deg2rad(tempdf$latitude[j+1])
      if (isTRUE(long2-long1 == 0 & lat2-lat1 == 0)) {
        out <- 0
      } else {
        out <- distance_hav(long1, lat1, long2, lat2)
      }
      templist[j] <- out #output is in kilometers
    }
    list <- c(list, templist)
  }
  return(list)
}

# and all the testing i did to find which function worked
listhav <- distance_val(period1)
listtest <- disttest(period1)
compare1 <- data.frame (listhav, listtest)
compare1$diff <- (compare1[1]-compare1[2])
compare1$id <- period1$Tweet.Id
unique(compare1$diff)

lo1 <- deg2rad(period1$longitude[10898])
lo2 <- deg2rad(period1$longitude[10898+1])
la1 <- deg2rad(period1$latitude[10898])
la2 <- deg2rad(period1$latitude[10898+1])
distance_hav(lo1, la1, lo2, la2)
period1$latitude[[10898]]
period1$Username[10899]