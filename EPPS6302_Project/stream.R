library(rtweet)
library(tidyverse)

# stream_tweets(
#   c(-97.508441,32.606718,-96.438648,33.267129),
#   timeout = 3600,
#   file_name = "data/dfw2.json",
#   parse = TRUE,
#   verbose = TRUE
# )


dfw <- parse_stream("data/dfw.json")

dfw_users <- users_data(dfw) %>% 
  select(id_str) %>% 
  unique() %>% 
  as.data.frame()

colnames(dfw_users) <- "id_str"

write_csv(dfw_users, "data/dfw_users.csv")



dfw2 <- parse_stream("data/dfw2.json")

dfw2_users <- users_data(dfw2) %>% 
  select(id_str) %>% 
  unique() %>% 
  as.data.frame()

colnames(dfw2_users) <- "id_str"

write_csv(dfw2_users, "data/dfw2_users.csv")


dfw_users <- as.character(dfw_users)
dfw2_users <- as.character(dfw2_users)
dfw_users_comb <- rbind(dfw_users, dfw2_users)

dfw_users_comb <- unique(dfw_users_comb)

write_csv(dfw_users_comb, "data/dfw_users_combined.csv")



dfw_comb <- rbind(dfw, dfw2)

coords <- do.call(rbind.data.frame, dfw_comb$coordinates)
coords <- na.omit(coords)

# NOT WORKING, ONLY PULLS USERS FROM dfw_users, NONE FROM dfw2_users
# dfw_users_comb2 <- users_data(dfw_comb) %>% 
#   select(id_str) %>% 
#   unique() %>% 
#   as.data.frame()
# 
# colnames(dfw_users) <- "id_str"
# 
# write_csv(dfw_users, "data/dfw_users.csv")