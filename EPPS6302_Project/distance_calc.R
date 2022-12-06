library(tidyverse)
library(SDMTools)
tweets <- read.csv("combined_full.csv")

# extract lat and long
tweets <- tweets %>% filter(Coordinates != "")
tweets <- subset(tweets, select = c("Datetime", "Tweet.Id", "Coordinates", 
                           "Username"))

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

tweets$Datetime <- as.POSIXct(tweets$Datetime)

tweets <- subset(tweets, tweets$Datetime < "2022-07-01", select = Datetime:Username)

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
    tweets$period[i] <- "Period 1: 3/2018 - 10/2018"
  } else if (date > "2019-08-31" & 
             date < "2020-06-01") {
    tweets$period[i] <- "Period 2: 9/2019 - 5/2020"
  } else if (date > "2021-09-30"  & 
             date < "2022-07-01") {
    tweets$period[i] <- "Period 3: 10/2021 - 6/2022"
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
# latitude/longitude using the Haversine formula (hf)
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
# from: https://www.r-bloggers.com/2010/11/great-circle-distance-calculations-in-r/
# distance function : pd = period# dataframe
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
period1$Travelled <- dist(period1)
period2$Travelled <- dist(period2)
period3$Travelled <- dist(period3)
tweets$Travelled <- dist(tweets)



# unique usernames in each period:
length(unique(tweets$Username)) # 296
length(unique(period1$Username)) # 147
length(unique(period2$Username)) # 182
length(unique(period3$Username)) # 258

# list of users that tweeted in all periods:
unique1 <- unique(period1$Username)
unique2 <- unique(period2$Username)
unique3 <- unique(period3$Username)

li <- match(unique1, unique2) # create locations where matches are found
st <- match(unique2, unique3) # filled with index # of matches and NAs

lista <- rep('', each=length(li)) # users who tweeted in period 1 + 2
for (i in 1:length(li)) {         # but maybe not 3
  lista[i] <- unique2[li[i]]
}
lista <- lista[!is.na(lista)]

listb <- rep('', each=length(st)) # users who tweeted in period 2 + 3
for (i in 1:length(st)) {         # but maybe not 1
  listb[i] <- unique3[st[i]]
}
listb <- listb[!is.na(listb)]

listab <- match(lista, listb)     # users in both list a and list b
uniquename <- rep('', each=length(listab)) # tweeted in all periods
for (i in 1:length(listab)) {
  uniquename[i] <- listb[listab[i]]
}
uniquename <- uniquename[!is.na(uniquename)] # 112 people
uniquename


################## OH BOY! PLOTS! ##########################

# distance travelled for selected user in selected period
p <- ggplot(tweets %>% filter(Username == "gbowner"), 
            aes(Datetime, log(Travelled), color = period)) +
       coord_cartesian(ylim = c(0, 10)) +
       geom_point() +
       theme_bw() + 
       scale_color_brewer(palette = "Set2", name = "Period") +
       labs(title = "Distance From Last Tweet: Twitter User gbowner")+
       xlab("Time") +
       ylab("Distance From Last Tweet (log km)")
p




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

