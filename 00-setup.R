# Packages
library(pacman)
p_load(data.table, tidyverse, igraph, here, parallel, RColorBrewer)

# Functions
read_tweets <- function(x){
  require(data.table)
  df <- data.table::fread(x, # name of file
                          select=c(tweet_type = "character", 
                                   userid = 'integer', 
                                   screen_name = "character", 
                                   date = 'character', 
                                   hashtag = 'character', 
                                   qtd_hashtag = 'character', 
                                   rt_hashtag = 'character')) 
}

clean_tweets <- function(x){
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove punctucation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

adoption_threshold <- function(tweets, hashtag){
  tweets <- read_csv(`tweets`)
  # if hashtag contains 'qanon', 1 if not 0 (should be all 1)
  tweets$hashtag_qanon <- ifelse(grepl(`hashtag`, tweets$hashtag), 1, 0)
  
  # if tweet is original/reply, 1 if not 0. This means 1 if user adopts behavior, 0 otherwise.
  tweets$original <- ifelse(grepl('original', tweets$tweet_type), 1, 0)
  tweets$original <- ifelse(grepl('reply', tweets$tweet_type), 1, tweets$original)
  
  # Identify time of first use of hashtag (original tweet)
  # Convert time to epoch
  tweets$date <- as.POSIXct(tweets$date, format = "%a %b %d %H:%M:%S %z %Y", tz = "GMT")
  tweets$date <- lubridate::as_datetime(tweets$date)
  tweets$date <- as.integer(tweets$date)
  
  # find earliest tweet when original (first adoption). This results in a df where 1 row = 1 user = earliest adoption time. 
  times <- tweets %>% 
    group_by(screen_name) %>%
    filter(original == 1) %>% 
    summarise(adoption_time = min(date, na.rm= TRUE))
  
  joined_df <- merge(times, tweets, by = 'screen_name', all.y = T)
  
  
  # find how many exposures before adoption 
  # find tweets that were before adoption for users who adopted. This drops rows where original=1, so we have to make sure to add them back at the end. Here, I'm filtering for rows where user was exposed (as opposed to adopted) to the behavior, and computing whether the date of that exposure was before the time of adoption (time_difference). 
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
  rm(exposure_count, exposures, joined_df, temp, times)
  
  # return
  return(tweets)
}

exposed_adopters <- function(data){
  unique_users <- data %>% 
    distinct(screen_name, .keep_all = TRUE)
  
  df_adopters <- unique_users[unique_users$original == 1, ]
  df_adopters[, 'n'][is.na(df_adopters[, 'n'])] <- 0
  df_adopters_exposed <- df_adopters[!df_adopters$n == 0, ]
  return(df_adopters_exposed)
}

adopters <- function(data){
  unique_users <- data %>% 
    distinct(screen_name, .keep_all = TRUE)
  
  df_adopters <- unique_users[unique_users$original == 1, ]
  df_adopters[, 'n'][is.na(df_adopters[, 'n'])] <- 0
  return(df_adopters)
}

