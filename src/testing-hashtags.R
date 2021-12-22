# Finding hashtags
# The purpose of this script is to test ways to find the most used hashtags by political extremists on twitter
tweets <- read_csv('/Users/bijeanghafouri/Code/extremist-contagion/data/tweets-qanon-clean_v2.csv')



# find most frequent hashtag among extremists 
extreme_left <- tweets[tweets$theta < -0.8806, ]
extreme_right <- tweets[tweets$theta > 2.9745, ]


extreme_left %>% count(hashtag, sort = T) %>% top_n(100)
extreme_right %>% count(hashtag, sort = T) %>% top_n(100)


# assign value to ideological group: -2 = extreme left, -1 left, 1 = right, 2 = extreme right (determine by quartiles)
# for some reason, theta = NA sometimes. In this case, remove row if theta=NA
tweets <- tweets[!is.na(tweets$theta), ]

quantile(tweets$theta)
tweets$group <- NA
tweets$group <- ifelse(tweets$theta < quantile(tweets$theta)[2], -2, tweets$group)
tweets$group <- ifelse(tweets$theta > quantile(tweets$theta)[2] & tweets$theta < quantile(tweets$theta)[3], -1, tweets$group)
tweets$group <- ifelse(tweets$theta > quantile(tweets$theta)[3] & tweets$theta < quantile(tweets$theta)[4], 1, tweets$group)
tweets$group <- ifelse(tweets$theta > quantile(tweets$theta)[4], 2, tweets$group)


# assign value 1 if extreme left, 0 otherwise 
tweets$extreme_left <- NA
tweets$extreme_left <- ifelse(tweets$group == -2, 'left', 'other')

# assign value 1 if extreme right, 0 otherwise
tweets$extreme_right <- NA
tweets$extreme_right <- ifelse(tweets$group == 2, 'right', 'other')



# Frequent hashtag usage from each ideological group using log-odds
# split hashtags column 
tweets_long <- tidyr::separate_rows(tweets, hashtag, sep = ' ')

# remove rows without hashtags 
tweets_long <- tweets_long[!(tweets_long$hashtag == ""), ]



# extreme left hashtags
word_ratios_left <- tweets_long %>%
  count(hashtag, extreme_left) %>%
  group_by(hashtag) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = extreme_left, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(left / other)) %>%
  arrange(desc(logratio))

fwrite(word_ratios_left, 'extreme_left_hashtags_logodds.csv')





# extreme right hashtags
word_ratios_right <- tweets_long %>%
  count(hashtag, extreme_right) %>%
  group_by(hashtag) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = extreme_right, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(right / other)) %>%
  arrange(desc(logratio))


fwrite(word_ratios_right, 'extreme_right_hashtags_logodds.csv')






# count hashtags 
extreme_right_hashtags_count <- tweets %>% 
  filter(extreme_right == 'right') %>% 
  count(hashtag, sort = TRUE)
fwrite(extreme_right_hashtags_count, 'extreme_right_hashtags_count.csv')


extreme_left_hashtags_count <- tweets %>% 
  filter(extreme_left == 'left') %>% 
  count(hashtag, sort = TRUE)
fwrite(extreme_left_hashtags_count, 'extreme_left_hashtags_count.csv')



# merge count with log odds
data.table::setDT(word_ratios_right) 
setkey(word_ratios_right, "hashtag")
setkey(extreme_right_hashtags_count, "hashtag")
#hashtag_right <- extreme_right_hashtags_count[word_ratios_right]
hashtag_right <- merge(extreme_right_hashtags_count, word_ratios_right)
fwrite(hashtag_right, 'extreme_right_hashtags_count_logodds.csv')


data.table::setDT(word_ratios_left) 
setkey(word_ratios_left, "hashtag")
setkey(extreme_left_hashtags_count, "hashtag")
hashtag_left <- extreme_left_hashtags_count[word_ratios_left]
hashtag_left <- merge(extreme_left_hashtags_count, word_ratios_left)
fwrite(hashtag_left, 'extreme_left_hashtags_count_logodds.csv')





# log odds hashtag manipulation
hashtag_left %>%
  group_by(logratio < 0) %>%
  slice_max(abs(logratio), n = 50) %>% 
  ungroup() %>%
  mutate(hashtag = reorder(hashtag, logratio))




hashtag_right %>%
  group_by(logratio < 0) %>%
  slice_max(abs(logratio), n = 50) %>% 
  ungroup() %>%
  mutate(hashtag = reorder(hashtag, logratio))


hashtag_right[order(hashtag_right$n, decreasing = TRUE),]  
hashtag_right[order(abs(hashtag_right$logratio), decreasing = TRUE),][1:100]



# To identify hashtags that are both (1) widely used by extremists and (2) have a high absolute log ratio, we identify the top 50 hashtags used (identified by count) among hashtags above the 75th quantile of log odds. In other words, the top 25% of hashtags with the highest log odds have a log odds below -0.5824 (min is -7.2546) ... (negative log odds = used by extremists more than moderate republicans).
top_log_odds_right <- hashtag_right %>% filter(logratio < quantile(hashtag_right$logratio)[2])
top_log_odds_right %>% 
  arrange(desc(n)) %>% 
  top_n(50)

top_log_odds_left <- hashtag_left %>% filter(logratio < quantile(hashtag_left$logratio)[2])
top_log_odds_left %>% 
  arrange(desc(n)) %>% 
  top_n(50)

