# Descriptive statistics 
setwd(here())

# Run 02 if data not collected
#source('02-adoption_threshold.R')

# Import data if collected
source('00-setup.R')
tweets <- read.csv("tweets-qanon-clean.csv")

# ----------- Compute statistics ------------------
# Dataframe with unique users only, where One row = one user. 
unique_users <- tweets %>% 
  distinct(screen_name, .keep_all = TRUE)


# Number of adopters, i.e. number of users who tweeted an original tweets with #qanon
df_adopters <- unique_users[unique_users$tweet_type == 'original', ]
# In the `n` column, when its NA that is user was exposed. If there is a number, it is equal to 1 or greater and means they were exposed once or more. Recode NAs into 0, then computer number of 0s in `n`  column. 
df_adopters[, 'n'][is.na(df_adopters[, 'n'])] <- 0
nrow(df_adopters)


# Number of adopters with prior exposure, i.e. they were exposed to #qanon THEN they used the hashtag 
df_adopters_exposed <- df_adopters[!df_adopters$n == 0, ]
nrow(df_adopters_exposed)


# Number of adopters without prior exposure, i.e. users who used #qanon, but were never exposed to this tweet previously  
nrow(df_adopters[df_adopters$n == 0, ])


# --------- average number of exposures needed to adopt
# excluding sole-adopters (excluding users who only adopted without prior exposure)
mean(df_adopters_exposed$n)

# including users who only adopt without exposure
mean(df_adopters$n)




# ------- Graph logarithm curve 
# Compute frequency table 
frequency_table <- as.data.frame(table(df_adopters$n))
colnames(frequency_table)[1] <- 'Exposures'
colnames(frequency_table)[2] <- 'Frequency'
frequency_table




# Density plot 
ggplot(data = df_adopters, 
       aes(x = n, fill = 'Red')) + 
  geom_density(alpha=0.3) + 
  ggtitle("Density plot of Tweet exposures before adopting #qanon") + 
  xlab('Exposures to #Qanon') + ylab("Density") + 
  theme_bw() + 
  guides(fill = FALSE) # remove title

# Density plot without those without exposure 
ggplot(data = df_adopters_exposed, 
       aes(x = n, fill = 'Red')) + 
  geom_density(alpha=0.3) + 
  ggtitle("Excluding sole adopters - Density plot of Tweet exposures before adopting #qanon") + 
  xlab('Exposures to #Qanon') + ylab("Density") + 
  theme_bw() + 
  guides(fill = FALSE) # remove title



# -------  Plotting with reduced data, where 100+ is in one category
df_adopted_reduced <- df_adopters
df_adopted_reduced[, 'n'][df_adopted_reduced[, 'n'] > 100, ] <- 0
df_adopted_reduced_exposed <- df_adopted_reduced[df_adopted_reduced$n != 0, ]  

# Plot including adoption without exposure 
ggplot(data = df_adopted_reduced, 
       aes(x = n, fill = 'Red')) + 
  geom_density(alpha=0.3) + 
  ggtitle("Reduced: Density plot of Tweet exposures before adopting #qanon") + 
  xlab('Exposures to #Qanon') + ylab("Density") + 
  theme_bw() + 
  guides(fill = FALSE) # remove title


ggplot(data = df_adopted_reduced_exposed, 
       aes(x = n, fill = 'Red')) + 
  geom_density(alpha=0.3) + 
  ggtitle("Reduced: Excluding sole adopters - Density plot of Tweet exposures before adopting #qanon") + 
  xlab('Exposures to #Qanon') + ylab("Density") + 
  theme_bw() + 
  guides(fill = FALSE) # remove title








# ---------- Ideology plot 
# Use Example 4: Visualize temperature data. from http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
