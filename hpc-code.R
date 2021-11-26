# HPC code

# Read emily's data, and merge with ideal points
library(tidyverse)
library(parallel)
library(data.table)
setwd('/scratch2/echen920/elections/clean/2020-07') 
cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
files <- list.files(pattern = '*.csv')


# Functions
read_tweets <- function(x){
  require(data.table)
  df <- data.table::fread(x, # name of file
                          select=c(tweet_type = "character", 
                                   userid = 'double', 
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


# ---------------- Read data 
# set counter
counter <- 0
# set list of df
datalist = list()
for(i in files){
  results <- parallel::mclapply(i, read_tweets, mc.cores = cores)
  tweets <- results[[1]]
  # counter
  counter <- counter + 1
  datalist[[i]] <- tweets # add to list
}
# Merge data
tweets <- do.call(rbind, datalist)
rm(results, datalist) 


# ------------ Merge with ideal points ----------------
# read ideological point estimates
filename <- '/project/pbarbera_665/bghafour/twitter-2020-election/data/ideal-points-merged.csv'
read_df <- function(x){
  require(data.table)
  df <- data.table::fread(x, header = T, sep = ',')
}
results <- parallel::mclapply(filename, read_df, mc.cores = cores)
ideology <- results[[1]]
rm(results)


filename <- '/project/pbarbera_665/bghafour/twitter-2020-election/data/tweets_july2020_total_ideology.csv'
read_df <- function(x){
  require(data.table)
  df <- data.table::fread(x, header = T, sep = ',')
}
results <- parallel::mclapply(filename, read_df, mc.cores = cores)
tweets <- results[[1]]
rm(results)


# Set both data files to data.tables
data.table::setDT(tweets) 
data.table::setDT(ideology)

# At this point, tweets$userid is still character (god knows why). Change to numeric for merging
tweets$id <- as.numeric(as.character(tweets$userid))
options("scipen"=100, "digits"=4)    # remove scientific notation

ideology$id_str <- as.numeric(ideology$id_str)


# merge (doesn't work for some reason...)
merge_dataframes <- function(x){
  merged_df <- data.table::merge.data.table(ideology, tweets,
                                            by.x = 'id_str', by.y = 'id', # keys to merge on
                                            all.y = F) # which column to keep all rows (only twitter data, not thetas)
}
list <- list(ideology, tweets)
tweets_ideology <- parallel::mclapply(list, merge_dataframes, mc.cores = cores)
tweets_ideology <- tweets_ideology[[2]]
data.table::setDT(tweets_ideology) 



# merge 2
setkey(ideology, "id_str")
setkey(tweets, "id")
tweets_ideology <- ideology[tweets]


tweets <- tweets_ideology

setwd('/project/pbarbera_665/bghafour/twitter-2020-election/data/')
fwrite(tweets, 'tweets_july2020_total_ideology.csv')



# --------------- Subset based on hashtag ------------------
# clean hashtag
tweets$hashtag <- tweets$hashtag %>% clean_tweets
tweets$qtd_hashtag <- tweets$qtd_hashtag %>% clean_tweets
tweets$rt_hashtag <- tweets$rt_hashtag %>% clean_tweets



# -------------- republican
# qanon 
temp_1 <- dplyr::filter(tweets, grepl("qanon", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("qanon", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("qanon", qtd_hashtag))

qanon <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

setwd('/Users/bijeanghafouri/Code/extremist-contagion/data')
fwrite(qanon, 'tweets-qanon-clean_v2.csv')


# stopthesteal
temp_1 <- dplyr::filter(tweets, grepl("stopthesteal", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("stopthesteal", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("stopthesteal", qtd_hashtag))

stopthesteal <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(stopthesteal, 'tweets-stopthesteal-clean.csv')



# wwg1wga
temp_1 <- dplyr::filter(tweets, grepl("wwg1wga", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("wwg1wga", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("wwg1wga", qtd_hashtag))

wwg1wga <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(wwg1wga, 'tweets-wwg1wga-clean.csv')




# obamagate
temp_1 <- dplyr::filter(tweets, grepl("obamagate", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("obamagate", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("obamagate", qtd_hashtag))

obamagate <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(obamagate, 'tweets-obamagate-clean.csv')





# whitelivesmatter
temp_1 <- dplyr::filter(tweets, grepl("whitelivesmatter", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("whitelivesmatter", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("whitelivesmatter", qtd_hashtag))

whitelivesmatter <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(whitelivesmatter, 'tweets-whitelivesmatter-clean.csv')



# trump2020 (control)
temp_1 <- dplyr::filter(tweets, grepl("trump2020", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("trump2020", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("trump2020", qtd_hashtag))

trump2020 <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(trump2020, 'tweets-trump2020-clean.csv')



# ------------- democrats 
# trumpvirus
temp_1 <- dplyr::filter(tweets, grepl("trumpvirus", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("trumpvirus", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("trumpvirus", qtd_hashtag))

trumpvirus <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(trumpvirus, 'tweets-trumpvirus-clean.csv')






# defundthepolice
temp_1 <- dplyr::filter(tweets, grepl("defundthepolice", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("defundthepolice", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("defundthepolice", qtd_hashtag))

defundthepolice <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(defundthepolice, 'tweets-defundthepolice-clean.csv')





# whitesupremacy
temp_1 <- dplyr::filter(tweets, grepl("whitesupremacy", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("whitesupremacy", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("whitesupremacy", qtd_hashtag))

whitesupremacy <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(whitesupremacy, 'tweets-whitesupremacy-clean.csv')





# abolishthepolice
temp_1 <- dplyr::filter(tweets, grepl("abolishthepolice", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("abolishthepolice", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("abolishthepolice", qtd_hashtag))

abolishthepolice <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(abolishthepolice, 'tweets-abolishthepolice-clean.csv')




# acab
temp_1 <- dplyr::filter(tweets, grepl("acab", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("acab", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("acab", qtd_hashtag))

acab <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(acab, 'tweets-acab-clean.csv')




# blm
temp_1 <- dplyr::filter(tweets, grepl("blm", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("blm", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("blm", qtd_hashtag))

blm <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(blm, 'tweets-blm-clean.csv')




# blacklivesmatter
temp_1 <- dplyr::filter(tweets, grepl("blacklivesmatter", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("blacklivesmatter", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("blacklivesmatter", qtd_hashtag))

blacklivesmatter <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(blacklivesmatter, 'tweets-blacklivesmatter-clean.csv')



# tre45on
temp_1 <- dplyr::filter(tweets, grepl("tre45on", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("tre45on", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("tre45on", qtd_hashtag))

tre45on <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(tre45on, 'tweets-tre45on-clean.csv')



# boycottgoya
temp_1 <- dplyr::filter(tweets, grepl("boycottgoya", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("boycottgoya", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("boycottgoya", qtd_hashtag))

boycottgoya <- rbind(temp_1, temp_2, temp_3)

#rm(temp_1, temp_2, temp_3)

fwrite(boycottgoya, 'tweets-boycottgoya-clean.csv')




# biden2020 (control)
temp_1 <- dplyr::filter(tweets, grepl("biden2020", hashtag))
temp_2 <- dplyr::filter(tweets, grepl("biden2020", rt_hashtag))
temp_3 <- dplyr::filter(tweets, grepl("biden2020", qtd_hashtag))

biden2020 <- rbind(temp_1, temp_2, temp_3)

rm(temp_1, temp_2, temp_3)

fwrite(biden2020, 'tweets-biden2020-clean.csv')





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

