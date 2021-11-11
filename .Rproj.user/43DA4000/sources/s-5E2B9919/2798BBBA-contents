# Descriptive statistics 
# Read previous script
source('02-adoption_threshold.R')

# ----------- Compute statistics ------------------

# Number of adopters 
length(tweets$tweet_type[tweets$tweet_type == 'original'])

# Number of adopters with prior exposure 
adopters_with_exposure <- tweets %>% 
  group_by(screen_name) %>% 
  distinct(n)
adopters_with_exposure <- colSums(!is.na(adopters_with_exposure))[[2]]
adopters_with_exposure

# Number of adopters without prior exposure 
adopters_without_exposure <- length(tweets$tweet_type[tweets$tweet_type == 'original']) - adopters_with_exposure
adopters_without_exposure

# --------- average number of exposures needed to adopt
# excluding sole-adopters (excluding users who only adopted)
mean(tweets[!duplicated(tweets[ , 'screen_name']), ]$n, na.rm = T)

# including users who only adopt without exposure (assigned 0)
tweets_adopters <- tweets[tweets$tweet_type == 'original', ]    # filter tweets that are originals (adoptions)
tweets_adopters$n[is.na(tweets_adopters$n)] <- 0    # set NAs to 0 (NA when adoption precedes exposure)
mean(tweets_adopters[!duplicated(tweets_adopters[ , 'screen_name']), ]$n, na.rm = T)    # compute mean 



# ------- Graph logarithm curve 
# Compute frequency table 
frequency_table <- as.data.frame(table(tweets_adopters$n))
colnames(frequency_table)[1] <- 'Exposures'
colnames(frequency_table)[2] <- 'Frequency'
frequency_table

# plot 
ggplot(frequency_table, aes(x=Exposures, y=Frequency)) + 
  geom_point()


# ---------- Ideology plot 
# Use Example 4: Visualize temperature data. from http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/


