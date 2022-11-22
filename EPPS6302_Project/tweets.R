library(tidyverse)
library(dplyr)
library(readr)

# Merge all tweets, remove rows with NA coordinates, save as rds
# 
# tweets <- list.files(path='C:/Users/Alden/OneDrive - The University of Texas at Dallas/EPPS 6302 (METHODS OF DATA COLLECTION AND PRODUCTION)/Project/Epps6302_Project/data/tweets') %>% 
#   lapply(read_csv) %>% 
#   bind_rows
#
# tweets <- subset(tweets, !is.na(Coordinates))
#
# (tweets, "data/tweets.rds")

tweets <- readRDS("data/tweets.rds")

unique(tweets$Username) %>% NROW()
unique(tweets$Coordinates) %>% NROW()
unique(tweets$Place) %>% NROW()

summary(tweets$Datetime)

tweets %>% filter(grepl('2018', Datetime)) %>% nrow()
tweets %>% filter(grepl('2019', Datetime)) %>% nrow()
tweets %>% filter(grepl('2020', Datetime)) %>% nrow()
tweets %>% filter(grepl('2021', Datetime)) %>% nrow()
tweets %>% filter(grepl('2022', Datetime)) %>% nrow()
