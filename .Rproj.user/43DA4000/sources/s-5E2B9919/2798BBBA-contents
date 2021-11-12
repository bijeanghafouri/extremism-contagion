# Descriptive statistics 
# Read previous script
setwd(here())
source('02-adoption_threshold.R')

# ----------- Compute statistics ------------------
# Dataframe with unique users only. One row = one user. 
unique_users <- tweets %>% 
  distinct(screen_name, .keep_all = TRUE)


# Number of adopters, i.e. number of users who tweeted an original tweets with #qanon
df_adopters <- unique_users[unique_users$tweet_type == 'original', ]
nrow(df_adopters)


# Number of adopters with prior exposure, i.e. they were exposed to #qanon THEN they used the hashtag 
df_adopters_exposed <- df_adopters[!is.na(df_adopters$n), ]
nrow(df_adopters_exposed)


# Number of adopters without prior exposure, i.e. users who used #qanon, but were never exposed to this tweet previously  
# In the `n` column, when its NA that is user was exposed. If there is a number, it is equal to 1 or greater and means they were exposed once or more. Recode NAs into 0, then computer number of 0s in `n`  column. 
df_adopters_notexposed <- df_adopters
df_adopters_notexposed[, 'n'][is.na(df_adopters_notexposed[, 'n'])] <- 0
nrow(df_adopters_notexposed)


# Dataframe for users who (1) adopted behavior and (2) adopter AFTER at least one exposure, and each row = single user. This does not include users who adopter without prior exposures
temp <- tweets[!duplicated(tweets[ , 'screen_name']), ]
temp <- temp[!is.na(temp$adoption_time), ]



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

# plot without those without exposure 
temp <- frequency_table[!frequency_table$Exposures == 0, ]
ggplot(temp, aes(x=Exposures, y=Frequency)) + 
  geom_point()


# Density plot 
a <- ggplot(frequency_table, 
            aes(x=factor(Exposures), y = Frequency)) +
  geom_col(color = 'black', fill = 'cyan3', alpha = 0.5)
a

# rescale data
frequency_table_2$Frequency <- frequency_table_2$Frequency/sum(frequency_table_2$Frequency)


library(RColorBrewer)
plot(density(frequency_table$Frequency))

ggplot(data = frequency_table, 
                            aes(x=Frequency, fill = 'blue')) + 
                  geom_density(alpha=0.3) + 
                  ggtitle("Title") + 
  xlab('Exposures to #Qanon') + ylab("Density") + 
  theme_bw() + 
  xlim(4000, 25000) +
  guides(fill=guide_legend("Sample Size")) + 
  scale_fill_hue(labels = c("n = 15", "n = 50", "n = 100", "n = 1000")) + theme(legend.position = c(0.9, 0.5))











# ---------- Ideology plot 
# Use Example 4: Visualize temperature data. from http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/


