# ------------------ Setup 
library(pacman)
p_load(data.table, tidyverse, igraph)
# Read data
tweets_1 <- read_csv("us-presidential-2020-clean-08-01-04.csv")
tweets_2 <- read_csv("us-presidential-2020-clean-08-01-03.csv")
tweets_3 <- read_csv("us-presidential-2020-clean-08-01-02.csv")
tweets_4 <- read_csv("us-presidential-2020-clean-08-01-01.csv")
tweets_5 <- read_csv("us-presidential-2020-clean-08-01-00.csv")
tweets <- rbind(tweets_5, tweets_1, tweets_2, tweets_3, tweets_4)

# sample 
# set.seed(10)
# tweets <- data.table(tweets)
# tweets <- tweets[sample(.N, 000)]
# tweets <- as_tibble(tweets)

# keep unique columns 
names <- unique(colnames(tweets))
tweets <- tweets[, names]

# keep necessary columns 
names <- c('rt_screen', 'rt_userid', 'rt_text', 'rt_hashtag', 'rt_qtd_count', 'rt_rt_count', 'rt_reply_count', 'rt_fav_count', 'rt_tweetid', 'display_name', 'hashtag', 'followers_count', 'friends_count', 'tweet_type', 'text', 'date', 'screen_name', 'userid', 'tweetid')
tweets <- tweets[, names]

# Find hashtags
hashtags <- as.vector(unique(tweets$hashtag))

# Let's do a case study of MAGA
df <- dplyr::filter(tweets, grepl("MAGA", hashtag))
df2 <- dplyr::filter(tweets, grepl("MAGA", rt_hashtag))
df <- rbind(df, df2)


# ------------------ Take 2
# Only keep if user retweet the hashtag from someone else (= endorsement)
df <- dplyr::filter(tweets, grepl("MAGA", rt_hashtag))

dplyr::filter(tweets, grepl("MAGA", rt_hashtag))

# Only keep if user retweets with comment (quote)
df <- dplyr::filter(tweets, grepl("quoted_tweet", tweet_type))

# Keep columns of user who retweet and source 
rt_df <- df[, c('rt_screen', 'screen_name')]
rt_df <- as.matrix(rt_df)     # igraph needs matrix object

# Create the retweet network
nw_rtweet <- graph_from_edgelist(el = rt_df, directed = TRUE)

# view retweet network 
print.igraph(nw_rtweet)





# plot graph
set.seed(1234)
glay = layout.fruchterman.reingold(nw_rtweet)
par(bg="gray15", mar=c(1,1,1,1))
plot(nw_rtweet, layout=glay,
     vertex.color="gray25",
     vertex.size=(degree(nw_rtweet, mode = "in")), #sized by in-degree centrality
     vertex.label = NA,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=edge_attr(nw_rtweet)$n/10, #sized by edge weight
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
title("Retweet Network", cex.main=1, col.main="gray95")


# plot #2 
plot(nw_rtweet, 
     vertex.color = 'green', 
     vertex.size = 2, 
     vertex.label.dist = 1.5, 
     vertex.label = NA, 
     edge.arrow.size = 0.01)






# export graph
write_graph(
  nw_rtweet,
  'graph_retweet',
  format = "edgelist"
)








# graphTweets package
library(twinetverse)
remotes::install_github("JohnCoene/twinetverse")
net <- tweets_1 %>% 
  gt_edges(screen_name, rt_screen) %>% # get edges
  gt_nodes() %>% # get nodes
  gt_collect() # collect

lapply(net, class)

# visualize
c(edges, nodes) %<-% net
head(edges)
head(nodes)

nodes$id <- as.factor(nodes$nodes) 
nodes$size <- nodes$n 
edges$id <- seq(1, nrow(edges)) 

nodes <- nodes2sg(nodes)
edges <- edges2sg(edges)

# id column in 'nodes' appears twice for some reason, so remove one 
nodes[4] <- NULL    

sigmajs() %>% 
  sg_nodes(nodes, id, size) 












# Qanon Take 3
# ----- Import data
# clean hashtag 
clean_tweets <- function(x) {
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
tweets$hashtag <- tweets$hashtag %>% clean_tweets
ex <- dplyr::filter(tweets, grepl("qanon", hashtag))

# remove unnecessary columns
names <- c('screen_name', 'tweet_type', 'date', 'tweetid', 'rt_hashtag', 'hashtag')
ex <- ex[, names]

# if hashtag contains 'qanon', 1 if not 0 (should be all 1)
ex$hashtag_qanon <- ifelse(grepl('qanon', ex$hashtag), 1, 0)

# if tweet is original, 1 if not 0 
ex$original <- ifelse(grepl('original', ex$tweet_type), 1, 0)


# Identify time of first use of hashtag (original tweet)
# Convert time to epoch
ex$date <- as.POSIXct(ex$date, format = "%a %b %d %H:%M:%S %z %Y", tz = "GMT")
ex$date <- lubridate::as_datetime(ex$date)
ex$date <- as.integer(ex$date)

# find earliest tweet when original (first adoption)
times <- ex %>% 
  group_by(screen_name) %>%
  filter(original == 1) %>% 
  summarise(adoption_time = min(date, na.rm= TRUE))

joined_df <- merge(times, ex, by = 'screen_name', all.y = T)


# find how many exposures before adoption 
# find tweets that were before adoption for users who adopted. This drops rows where original=1, so we have to make sure to add them back at the end. 
temp <- joined_df %>% 
  filter(original == 0) %>% 
  mutate(time_difference = date - adoption_time)

# if before, 1, ifelse 0
temp$difference <- ifelse(temp$time_difference < 0, 1, 0)

# merge temp and joined_df, since temp lost rows where original=1
#joined_df <- joined_df %>% filter(original == 1)
#temp2 <- merge(temp, joined_df, by = 'screen_name', all.x = T, all.y = T)

#temp2 <- bind_rows(joined_df, temp)
#temp2 <- temp2[!duplicated(temp2), ]

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
final_df <- rbind(joined_df, exposures)


