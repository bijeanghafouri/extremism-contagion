# Packages
library(pacman)
p_load(data.table, tidyverse, igraph, here, parallel, RColorBrewer, xtable, modelsummary, kableExtra)
setwd('/Users/bijeanghafouri/Code/extremist-contagion')

# --------------------------- Functions  --------------------------- 
# Read data
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

# Clean data
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

# Compute adoption columns
adoption_threshold <- function(tweets, hashtag, party){
  tweets <- read_csv(`tweets`)
  # if hashtag contains 'qanon', 1 if not 0 (should be all 1)
  #tweets$hashtag_qanon <- ifelse(grepl(`hashtag`, tweets$hashtag), 1, 0)
  
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
  
  # filter based on ideology
  if(party == 'republican'){
    tweets <- tweets %>% filter(theta > 0)
  } else {
    tweets <- tweets %>% filter(theta < 0.001)
  }
  
  # return
  return(tweets)
}

# Filter data for adopters who were previously exposed
exposed_adopters <- function(data){
  unique_users <- data %>% 
    distinct(screen_name, .keep_all = TRUE)
  
  df_adopters <- unique_users[unique_users$original == 1, ]
  df_adopters[, 'n'][is.na(df_adopters[, 'n'])] <- 0
  df_adopters_exposed <- df_adopters[!df_adopters$n == 0, ]
  return(df_adopters_exposed)
}

# Filter data for all adopters
adopters <- function(data){
  unique_users <- data %>% 
    distinct(screen_name, .keep_all = TRUE)
  
  df_adopters <- unique_users[unique_users$original == 1, ]
  df_adopters[, 'n'][is.na(df_adopters[, 'n'])] <- 0
  return(df_adopters)
}

# descriptives for one dataset
desc <- function(dataset){
  # statistics
  n <- nrow(dataset)
  unique_users <- length(unique(dataset$screen_name))
  adopters <- nrow(adopters(dataset))
  adopters_exposed <- nrow(exposed_adopters(dataset))
  sole_adopters <- nrow(adopters(dataset)[!adopters(dataset)$screen_name %in% exposed_adopters(dataset)$screen_name, ])
  mean_exposures <- round(mean(exposed_adopters(dataset)$n))
  mean_theta <- mean(dataset$theta, na.rm = T)
  theta_adopters <- mean(adopters(dataset)$theta)
  theta_exposed_adopters <- mean(exposed_adopters(dataset)$theta)
  theta_sole_adopters <- mean(adopters(dataset)[!adopters(dataset)$screen_name %in% exposed_adopters(dataset)$screen_name, ]$theta)
  
    
  # return
  vec <- c(n, unique_users, adopters, adopters_exposed, sole_adopters, mean_exposures, mean_theta, theta_adopters, theta_exposed_adopters, theta_sole_adopters)
  return(vec)
}

# create table of summary statistics 
desc_table <- function(datasets){
  ex <- lapply(datasets, desc)
  df <- do.call(rbind.data.frame, ex)
  df <- tibble(df)
  # rename columns
  names(df) <- c('n', 'unique users', 'adopters', 'adopters exposed', 'sole adopters', 'mean exposures', 'mean theta', 'theta_adopters', 'theta_exposed_adopters', 'theta_sole_adopters')
  # add hashtag column 
  df$hashtag <- names(datasets)
  # place hashtag column in front
  df <- df[,c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)]
  
  
  # flip rows into column
  options(scipen = 999) # remove scientific notation
  options(digits = 3)
  df <- as.data.frame(t(df))
  names(df) <- as.matrix(df[1, ])
  df <- df[-1, ]
  df[] <- lapply(df, function(x) type.convert(as.character(x)))
  df[1, ] <- sub("0+$", "", as.character(df[1, ]))
  
  return(df)
}

# create ideology plot 
ideology_plot <- function(dataset){
  # ------------------------------------------------------
  # Function to create ideology density plot from dataset.
  # Returns one plot 
  # ------------------------------------------------------
  
  x <- na.omit(dataset$theta)
  y <- density(na.omit(x), n = 2^12)
  
  plot <- ggplot(data.frame(x = y$x, y = y$y), aes(x,y)) + geom_line() + 
    geom_segment(aes(xend = x, yend = 0, color = x)) + 
    scale_color_gradient(low = 'dodgerblue2', high = 'firebrick2') +
    labs(x = 'Ideology estimate', y = 'Density') + 
    cowplot::theme_cowplot() + guides(color = FALSE)
  
  # return
  return(plot)
}

# ridge ideology plots 
ridges_plot_democrat <- function(x){
  # Combine dataframes
  acab_test <- acab
  acab_test <- acab_test[, 'theta']
  acab_test$hashtag <- 'acab'
  
  abolishthepolice_test <- abolishthepolice
  abolishthepolice_test <- abolishthepolice_test[, 'theta']
  abolishthepolice_test$hashtag <- 'abolishthepolice'
  
  biden2020_test <- biden2020
  biden2020_test <- biden2020_test[, 'theta']
  biden2020_test$hashtag <- 'biden2020'
  
  blacklivesmatter_test <- blacklivesmatter
  blacklivesmatter_test <- blacklivesmatter_test[, 'theta']
  blacklivesmatter_test$hashtag <- 'blacklivesmatter'
  
  blm_test <- blm
  blm_test <- blm_test[, 'theta']
  blm_test$hashtag <- 'blm'
  
  boycottgoya_test <- boycottgoya
  boycottgoya_test <- boycottgoya_test[, 'theta']
  boycottgoya_test$hashtag <- 'boycottgoya'
  
  defundthepolice_test <- defundthepolice
  defundthepolice_test <- defundthepolice_test[, 'theta']
  defundthepolice_test$hashtag <- 'defundthepolice'
  
  whitesupremacy_test <- whitesupremacy
  whitesupremacy_test <- whitesupremacy_test[, 'theta']
  whitesupremacy_test$hashtag <- 'whitesupremacy'
  
  tre45on_test <- tre45on
  tre45on_test <- tre45on_test[, 'theta']
  tre45on_test$hashtag <- 'tre45on'
  
  
  trumpvirus_test <- trumpvirus
  trumpvirus_test <- trumpvirus_test[, 'theta']
  trumpvirus_test$hashtag <- 'trumpvirus'
  
  
  df <- rbind(biden2020_test, acab_test, abolishthepolice_test, blacklivesmatter_test, blm_test, boycottgoya_test, defundthepolice_test, whitesupremacy_test, tre45on_test, trumpvirus_test)
  
  
  library(ggridges)
  library(ggplot2)
  library(viridis)
  library(hrbrthemes)
  plot <- ggplot(df, aes(x = theta, y = hashtag, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Ideal points", option = "C") +
    labs(title = 'Ideal points of Democrat hashtags') +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  
  
  return(plot)
}

ridges_plot_republican <- function(x){
  # Combine dataframes
  obamagate_test <- obamagate
  obamagate_test <- obamagate_test[, 'theta']
  obamagate_test$hashtag <- 'obamagate'
  
  qanon_test <- qanon
  qanon_test <- qanon_test[, 'theta']
  qanon_test$hashtag <- 'qanon'
  
  stopthesteal_test <- stopthesteal
  stopthesteal_test <- stopthesteal_test[, 'theta']
  stopthesteal_test$hashtag <- 'stopthesteal'
  
  trump2020_test <- trump2020
  trump2020_test <- trump2020_test[, 'theta']
  trump2020_test$hashtag <- 'trump2020'
  
  whitelivesmatter_test <- whitelivesmatter
  whitelivesmatter_test <- whitelivesmatter_test[, 'theta']
  whitelivesmatter_test$hashtag <- 'whitelivesmatter'
  
  wwg1wga_test <- wwg1wga
  wwg1wga_test <- wwg1wga_test[, 'theta']
  wwg1wga_test$hashtag <- 'wwg1wga'
  
  df <- rbind(wwg1wga_test, whitelivesmatter_test, trump2020_test, stopthesteal_test, qanon_test, obamagate_test)
  
  
  library(ggridges)
  library(ggplot2)
  library(viridis)
  library(hrbrthemes)
  plot <- ggplot(df, aes(x = theta, y = hashtag, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Ideal points", option = "C") +
    labs(title = 'Ideal points of Republican hashtags') +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  
  
  return(plot)
}


# --------------- Threshold plots
ridges_plot_republican_adoption <- function(column){
  # -------------------------------
  # I limit the threhold number to 50, as over 50 are outliers
  # -------------------------------
  # Combine dataframes
  obamagate_test <- exposed_adopters(obamagate)
  obamagate_test <- obamagate_test[, 'n']
  obamagate_test$hashtag <- 'obamagate'
  
  qanon_test <- exposed_adopters(qanon)
  qanon_test <- qanon_test[, 'n']
  qanon_test$hashtag <- 'qanon'
  
  stopthesteal_test <- exposed_adopters(stopthesteal)
  stopthesteal_test <- stopthesteal_test[, 'n']
  stopthesteal_test$hashtag <- 'stopthesteal'
  
  trump2020_test <- exposed_adopters(trump2020)
  trump2020_test <- trump2020_test[, 'n']
  trump2020_test$hashtag <- 'trump2020'
  
  whitelivesmatter_test <- exposed_adopters(whitelivesmatter)
  whitelivesmatter_test <- whitelivesmatter_test[, 'n']
  whitelivesmatter_test$hashtag <- 'whitelivesmatter'
  
  wwg1wga_test <- exposed_adopters(wwg1wga)
  wwg1wga_test <- wwg1wga_test[, 'n']
  wwg1wga_test$hashtag <- 'wwg1wga'
  
  df <- rbind(wwg1wga_test, whitelivesmatter_test, trump2020_test, stopthesteal_test, qanon_test, obamagate_test)
  
  
  library(ggridges)
  library(ggplot2)
  library(viridis)
  library(hrbrthemes)
  plot <- ggplot(df, aes(x = log(n), y = hashtag, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Threshold Parameter", option = "C") +
    labs(title = 'ADoption rate for Republican hashtags') +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  
  
  return(plot)
}

ridges_plot_democrat_adoption <- function(column){
  # -------------------------------
  # 
  # -------------------------------
  # Combine dataframes
  acab_test <- acab
  acab_test <- acab_test[, 'n']
  acab_test$hashtag <- 'acab'
  
  abolishthepolice_test <- abolishthepolice
  abolishthepolice_test <- abolishthepolice_test[, 'n']
  abolishthepolice_test$hashtag <- 'abolishthepolice'
  
  biden2020_test <- biden2020
  biden2020_test <- biden2020_test[, 'n']
  biden2020_test$hashtag <- 'biden2020'
  
  blacklivesmatter_test <- blacklivesmatter
  blacklivesmatter_test <- blacklivesmatter_test[, 'n']
  blacklivesmatter_test$hashtag <- 'blacklivesmatter'
  
  blm_test <- blm
  blm_test <- blm_test[, 'n']
  blm_test$hashtag <- 'blm'
  
  boycottgoya_test <- boycottgoya
  boycottgoya_test <- boycottgoya_test[, 'n']
  boycottgoya_test$hashtag <- 'boycottgoya'
  
  defundthepolice_test <- defundthepolice
  defundthepolice_test <- defundthepolice_test[, 'n']
  defundthepolice_test$hashtag <- 'defundthepolice'
  
  whitesupremacy_test <- whitesupremacy
  whitesupremacy_test <- whitesupremacy_test[, 'n']
  whitesupremacy_test$hashtag <- 'whitesupremacy'
  
  tre45on_test <- tre45on
  tre45on_test <- tre45on_test[, 'n']
  tre45on_test$hashtag <- 'tre45on'
  
  
  trumpvirus_test <- trumpvirus
  trumpvirus_test <- trumpvirus_test[, 'n']
  trumpvirus_test$hashtag <- 'trumpvirus'
  
  
  df <- rbind(biden2020_test, acab_test, abolishthepolice_test, blacklivesmatter_test, blm_test, boycottgoya_test, defundthepolice_test, whitesupremacy_test, tre45on_test, trumpvirus_test)
  
  
  library(ggridges)
  library(ggplot2)
  library(viridis)
  library(hrbrthemes)
  plot <- ggplot(df, aes(x = log(n), y = hashtag, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Threshold Parameter", option = "C") +
    labs(title = 'Adoption rate for Democrat hashtags') +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  
  
  return(plot)
}



# --------------  Regression 
# Create adoption column 
adopted_column <- function(dataset){
  # ----------------------------------------------------------------
  # Takes a dataset and creates a binary column, 1 = user adopted (used hashtag) and 0 if not. 
  # Similar to the `adopters` fun which subsets the dataset only to users who adopted
  # ----------------------------------------------------------------
  dataset$adopted <- NA
  falseifNA <- function(x){
    ifelse(is.na(x), FALSE, x)
  }
  ifelse2 <- function(x, a, b){
    ifelse(falseifNA(x), a, b)
  }
  dataset$adopted <- ifelse2(is.na(dataset$adoption_time), 0, 1)
  return(dataset)
}

# Count total number of exposures
total_exposures <- function(dataset){
  # ----------------------------------------------------------------------
  # Function takes in a dataframe and counts the 
  # number of exposures (includes adoptions) each user has. 
  # ----------------------------------------------------------------------
  temp <- dataset %>% 
    group_by(screen_name) %>% 
    count() # count number of times user appears in dataset
  
  # rename count column (because other dataframe already has this column name)
  names(temp)[2] <- 'total_exposures'
  
  # merge with original dataframe
  temp <- merge(temp, dataset, by = 'screen_name', all.y = T)
  return(temp)
  
}

# Combine `adopted_column` and `total_exposures` to make dataset regression-ready
regression_ready <- function(dataset){
  # --------------------------------------
  # Make dataset regression-ready: Create IV and DV column. Keep unique users only. 
  # --------------------------------------
  
  # Code independent variable: total number of exposures
  # Count number of exposures for each user. Every user is exposed at least once, since they are included in the dataset if they tweeted something (qt, reply, etc) with the hasthag. 
  dataset <- total_exposures(dataset)
  
  # Code dependent variable: If user adopted = 1, if not = 0. 
  # We can use the `adoption_time` column, which states the time of adoption if user adopted. If they didn't adopt,      value is NA. 
  dataset <- adopted_column(dataset)
  
  # Subset data to unique users only, since we want our regression to be at the user-level
  dataset <- dataset %>% 
    distinct(screen_name, .keep_all = TRUE)
  
  # return
  return(dataset)
}

