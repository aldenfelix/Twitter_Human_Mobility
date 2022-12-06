library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(moveVis)
library(sf)
use_disk()
use_multicore()

# Merge all Tweets, Wrangle, Save as tweets.rds ---- 

# test with ~20 users
tweets <- list.files(path='C:/Users/Alden/Documents/College/Year 4/Semester 1//EPPS 6302 (METHODS OF DATA COLLECTION AND PRODUCTION)/Project/Epps6302_Project/data/tweets') %>%
   lapply(read_csv) %>%
   bind_rows

tweets <- subset(tweets, !is.na(Coordinates))

tweets$Coordinates <- gsub("Coordinates", "",
                      gsub("\\(", "",
                      gsub("\\)", "",
                      gsub("longitude", "",
                      gsub("latitude", "",
                      gsub("=", "", tweets$Coordinates))))))

tweets <- separate(tweets, col = Coordinates,
         into = c("longitude", "latitude"),
         sep = c(","))

tweets$Datetime <- as.POSIXct(tweets$Datetime)

saveRDS(tweets, "data/tweets.rds")


# ~300 users
fulltweets <- read_csv("data/combined_full.csv")

fulltweets <- subset(fulltweets, !is.na(Coordinates))

fulltweets$Coordinates <- gsub("Coordinates", "",
                          gsub("\\(", "",
                          gsub("\\)", "",
                          gsub("longitude", "",
                          gsub("latitude", "",
                          gsub("=", "", fulltweets$Coordinates))))))

fulltweets <- separate(fulltweets, col = Coordinates,
                       into = c("longitude", "latitude"),
                       sep = c(","))

fulltweets$Datetime <- as.POSIXct(fulltweets$Datetime)

saveRDS(fulltweets, "data/fulltweets.rds")



# Split Data Into 3 Periods ----
# period 1: 03/01/2018 - 10/31/2018 
# period 2: 09/01/2019 - 05/31/2020 
# period 3: 10/01/2021 - 06/31/2022

fulltweets <- readRDS("data/fulltweets.rds")

period1 <- subset(fulltweets, Datetime < "2018-11-01")
period2 <- subset(fulltweets, Datetime > "2019-08-31" & 
                    Datetime < "2020-06-01")
period3 <- subset(fulltweets, Datetime > "2021-09-30" & 
                    Datetime < "2022-07-01")n



# Summarize Data ----

unique(fulltweets$Username) %>% NROW()
unique(tweets$Coordinates) %>% NROW()
unique(fulltweets$Place) %>% NROW()

summary(fulltweets$Datetime)

fulltweets %>% filter(grepl('2018', Datetime)) %>% nrow()
fulltweets %>% filter(grepl('2019', Datetime)) %>% nrow()
fulltweets %>% filter(grepl('2020', Datetime)) %>% nrow()
fulltweets %>% filter(grepl('2021', Datetime)) %>% nrow()
fulltweets %>% filter(grepl('2022', Datetime)) %>% nrow()



# (TEST) Creating MoveStack Object and Animating ~20 users ----

tweets <- readRDS("data/tweets.rds")

move_data <- subset(tweets, select = c(Datetime, longitude, latitude, Username))
move_data$longitude <- as.numeric(move_data$longitude)
move_data$latitude <- as.numeric(move_data$latitude)
move_data$Username <- factor(move_data$Username)

move_data <- move_data[-29009,]
move_data <- subset(move_data, Username!= "Nohelia_acr" & 
                      Username!= "baboquarto2" &
                      Username!= "everytract")

move_df <- moveVis::df2move(move_data, x = "longitude", y = "latitude", 
        time = "Datetime", track_id = "Username",
        proj = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

m <- align_move(move_df, res = "mean", unit = "days")

view_spatial(m)

frames <- frames_spatial(m, map_service = "osm", 
                         map_type = "watercolor", 
                         alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% 
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress() %>% 
  path_legend(FALSE)

animate_frames(frames, out_file = "move.mp4", 
               width = 1920, height = 1080, res = 100)



# Animating  Period 1 ----

# Subset Period and Coerce to Classes Required for MoveStack Object
# Date/Time needs to be class POSIXct
move1 <- subset(period1, select = c(Datetime, longitude, latitude, Username))
move1$longitude <- as.numeric(move1$longitude)
move1$latitude <- as.numeric(move1$latitude)

# Remove Observations of Usernames with < 2 Unique Coordinates
userplaces <- read_csv("data/combined_full.csv")
userplaces <- subset(userplaces, Username != "everytract")
userplaces <- userplaces %>% 
  group_by(Username) %>% 
  summarise(uniqueplaces = n_distinct(Coordinates)) %>% 
  subset(uniqueplaces < 2)
for (i in seq_along(userplaces)) {
  move1 <- subset(move1, Username != userplaces$Username[[i]])
}

# Remove Observations of Usernames with < x Observations
# (otherwise causes problems when creating MoveStack)
names <- vector("list")
c <- 1
for (i in seq_along(unique(move1$Username))) {
  if (move1 %>%
      subset(Username == unique(move1$Username)[[i]]) %>%
      nrow < 10) {
    names[[c]] <- unique(move1$Username)[[i]]
    c <- c + 1
  }
}

for (i in seq_along(names)) {
  move1 <- subset(move1, Username != names[[i]])
}

# Coerce Usernames After Dropping Users
move1$Username <- factor(move1$Username)

# Problem Users
move1 <- subset(move1, Username!= "Nohelia_acr" & 
                      Username!= "baboquarto2" &
                      Username!= "everytract")

# MoveStack Requires Unique Time Stamps
move1 <- distinct(move1, Datetime, .keep_all = TRUE)

# Restrict Data to Bounding Box of U.S.
move1 <- subset(move1, longitude > -125 &
                  longitude < -66 &
                  latitude > 24 &
                  latitude < 50)

# Restrict Data to Bounding Box of DFW Area
move1 <- subset(move1, longitude > -97.5411 &
                  longitude < -96.3066 &
                  latitude > 32.3779 &
                  latitude < 33.3308)

# Create CRS object to pass onto dfwmove()
# 4326 - EPSG 4326
# 3857 - Pseudo Mercator
projection <- st_crs(4326)

# Convert Dataframe to MoveStack
df1 <- df2move(move1, x = "longitude", y = "latitude", 
                            time = "Datetime", track_id = "Username",
                            proj = projection$input)

# Temporal Allignment for Animation
m1 <- align_move(df1, res = "mean")

# Interactive Map with all Data
# view_spatial(m1)

# Coerce to WGS 84 / Pseudo Mercator (epsg:3857) / Web Mercator
# (no noticeable improvement) (remove distortion from adjusting for data's projection)
# m1 <- sp::spTransform(m1, projection$input)

# Create frames
frames1 <- frames_spatial(m1, map_service = "carto", 
                         map_type = "dark",
                         alhpa = 1,
                         path_legend = FALSE,
                         path_size = 0.5,
                         tail_size = 0.5,
                         trace_size = 0.5,
                         equidistant = TRUE) %>% 
  add_labels(title = "Local Movement in Period 1", 
             x = "Longitude", y = "Latitude") %>% 
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

# Check Frames before Animating
# head(frames1, n = 1L)
# tail(frames1, n = 1L)

animate_frames(frames1, out_file = "Period1_Local.mp4", 
               width = 2560, height = 1440, res = 200)



# Animating  Period 2 ----
# (check Animating Period 1 for Comments)

move2 <- subset(period2, select = c(Datetime, longitude, latitude, Username))
move2$longitude <- as.numeric(move2$longitude)
move2$latitude <- as.numeric(move2$latitude)

userplaces <- read_csv("data/combined_full.csv")
userplaces <- subset(userplaces, Username != "everytract")
userplaces <- userplaces %>% 
  group_by(Username) %>% 
  summarise(uniqueplaces = n_distinct(Coordinates)) %>% 
  subset(uniqueplaces < 2)
for (i in seq_along(userplaces)) {
  move2 <- subset(move2, Username != userplaces$Username[[i]])
}

names2 <- vector("list")
c <- 1
for (i in seq_along(unique(move2$Username))) {
  if (move2 %>% 
      subset(Username == unique(move2$Username)[[i]]) %>% 
      nrow < 10) {
    names2[[c]] <- unique(move2$Username)[[i]]
    c <- c + 1
  }
}

for (i in seq_along(names2)) {
  move2 <- subset(move2, Username != names2[[i]])
}

move2$Username <- factor(move2$Username)

move2 <- subset(move2, Username!= "Nohelia_acr" & 
                  Username!= "baboquarto2" &
                  Username!= "everytract")

move2 <- distinct(move2, Datetime, .keep_all = TRUE)

# Restrict Data to Bounding Box of U.S.
move2 <- subset(move2, longitude > -125 &
                  longitude < -66 &
                  latitude > 24 &
                  latitude < 50)

# Restrict Data to Bounding Box of DFW Area
move2 <- subset(move2, longitude > -97.5411 &
                  longitude < -96.3066 &
                  latitude > 32.3779 &
                  latitude < 33.3308)

# 4326 - EPSG 4326
# 3857 - Pseudo Mercator
projection <- st_crs(4326)

df2 <- df2move(move2, x = "longitude", y = "latitude", 
               time = "Datetime", track_id = "Username",
               proj = projection$input)

m2 <- align_move(df2, res = "mean")

# view_spatial(m2)

frames2 <- frames_spatial(m2, map_service = "carto", 
                          map_type = "dark",
                          alhpa = 1,
                          path_legend = FALSE,
                          path_size = 0.5,
                          tail_size = 0.5,
                          trace_size = 0.5,
                          equidistant = TRUE) %>% 
  add_labels(title = "Local Movement in Period 2", 
             x = "Longitude", y = "Latitude") %>% 
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

# tail(frames2, n = 1L)

animate_frames(frames2, out_file = "Period2_Local.mp4", 
               width = 2560, height = 1440, res = 200)



# Animating  Period 3 ----
# (check Animating Period 1 for Comments)

move3 <- subset(period3, select = c(Datetime, longitude, latitude, Username))
move3$longitude <- as.numeric(move3$longitude)
move3$latitude <- as.numeric(move3$latitude)

userplaces <- read_csv("data/combined_full.csv")
userplaces <- subset(userplaces, Username != "everytract")
userplaces <- userplaces %>% 
  group_by(Username) %>% 
  summarise(uniqueplaces = n_distinct(Coordinates)) %>% 
  subset(uniqueplaces < 2)
for (i in seq_along(userplaces)) {
  move3 <- subset(move3, Username != userplaces$Username[[i]])
}

names3 <- vector("list")
c <- 1
for (i in seq_along(unique(move3$Username))) {
  if (move3 %>% 
      subset(Username == unique(move3$Username)[[i]]) %>% 
      nrow < 10) {
    names3[[c]] <- unique(move3$Username)[[i]]
    c <- c + 1
  }
}

for (i in seq_along(names3)) {
  move3 <- subset(move3, Username != names3[[i]])
}

move3$Username <- factor(move3$Username)

move3 <- subset(move3, Username!= "Nohelia_acr" & 
                  Username!= "baboquarto2" &
                  Username!= "everytract")

move3 <- distinct(move3, Datetime, .keep_all = TRUE)

# Restrict Data to Bounding Box of U.S.
move3 <- subset(move3, longitude > -125 &
                  longitude < -66 &
                  latitude > 24 &
                  latitude < 50)

# Restrict Data to Bounding Box of DFW Area
move3 <- subset(move3, longitude > -97.5411 &
                  longitude < -96.3066 &
                  latitude > 32.3779 &
                  latitude < 33.3308)

# 4326 - EPSG 4326
# 3857 - Pseudo Mercator
projection <- st_crs(4326)

df3 <- df2move(move3, x = "longitude", y = "latitude", 
               time = "Datetime", track_id = "Username",
               proj = projection$input)

m3 <- align_move(df3, res = "mean")

# view_spatial(m3)

frames3 <- frames_spatial(m3, map_service = "carto", 
                          map_type = "dark",
                          alhpa = 1,
                          path_legend = FALSE,
                          path_size = 0.5,
                          tail_size = 0.5,
                          trace_size = 0.5,
                          equidistant = TRUE) %>% 
  add_labels(title = "Local Movement in Period 3", 
             x = "Longitude", y = "Latitude") %>% 
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

# tail(frames3, n = 1L)

animate_frames(frames3, out_file = "Period3_Local.mp4", 
               width = 2560, height = 1440, res = 200)



# Users Arranged by Number of Unique Coordinates----
usercoords <- read_csv("data/combined_full.csv")
usercoords <- subset(usercoords, Username != "everytract")
userplaces <- usercoords %>% 
  group_by(Username) %>% 
  summarise(uniqueplaces = n_distinct(Coordinates)) %>% 
  arrange(desc(uniqueplaces))
write_csv(userplaces, "data/toptravelers.csv")