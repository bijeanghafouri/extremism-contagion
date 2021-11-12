# Adoption threshold with retweet graph 
# ------------------ Setup 
# Read data
setwd(here())
source('01-read_data.R')

# if hashtag contains 'qanon', 1 if not 0 (should be all 1)
tweets$hashtag_qanon <- ifelse(grepl('qanon', tweets$hashtag), 1, 0)

# if tweet is original, 1 if not 0 
tweets$original <- ifelse(grepl('original', tweets$tweet_type), 1, 0)

# Identify time of first use of hashtag (original tweet)
# Convert time to epoch
tweets$date <- as.POSIXct(tweets$date, format = "%a %b %d %H:%M:%S %z %Y", tz = "GMT")
tweets$date <- lubridate::as_datetime(tweets$date)
tweets$date <- as.integer(tweets$date)

# find earliest tweet when original (first adoption)
times <- tweets %>% 
  group_by(screen_name) %>%
  filter(original == 1) %>% 
  summarise(adoption_time = min(date, na.rm= TRUE))

joined_df <- merge(times, tweets, by = 'screen_name', all.y = T)


# find how many exposures before adoption 
# find tweets that were before adoption for users who adopted. This drops rows where original=1, so we have to make sure to add them back at the end. 
temp <- joined_df %>% 
  filter(original == 0) %>% 
  mutate(time_difference = date - adoption_time)

# if before, 1, ifelse 0
temp$difference <- ifelse(temp$time_difference < 0, 1, 0)

# count number of exposures before adoption 
exposure_count <- temp %>% 
  filter(difference == 1) %>% 
  group_by(screen_name) %>% 
  count()

exposures <- merge(temp, exposure_count, by = 'screen_name', all.x = T)

# Now, the exposures df is missing rows where original=1. We add them back here
# add columns to joined_df that are in the exposures df
joined_df$time_difference <- NA
joined_df$difference <- NA
joined_df$n <- NA
joined_df <- joined_df %>% filter(original == 1)
tweets <- rbind(joined_df, exposures)

# assign values when tweet-type = original 
tweets <- tweets %>% 
  group_by(userid) %>% 
  fill(n) %>% #default direction down
  fill(n, .direction = "up")

# Remove unnecessary data
rm(exposure_count, exposures, joined_df, temp, times, cores, files, clean_tweets)

