source('00-setup.R')

# cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
cores <- 1
files <- c('/Users/bijeanghafouri/us-presidential-2020-clean-08-01-04.csv', 
           '/Users/bijeanghafouri/us-presidential-2020-clean-08-01-03.csv', 
           '/Users/bijeanghafouri/us-presidential-2020-clean-08-01-02.csv', 
           '/Users/bijeanghafouri/us-presidential-2020-clean-08-01-01.csv')

  
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
rm(results) 
rm(datalist) 


# -------------------- Clean data 
# clean hashtag columns 
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
tweets$qtd_hashtag <- tweets$qtd_hashtag %>% clean_tweets
tweets$rt_hashtag <- tweets$rt_hashtag %>% clean_tweets

# keep rows that have 'qanon' hashtag 
ex1 <- dplyr::filter(tweets, grepl("qanon", hashtag))
ex2 <- dplyr::filter(tweets, grepl("qanon", rt_hashtag))
ex3 <- dplyr::filter(tweets, grepl("qanon", qtd_hashtag))

# bind temporary dataframes 
tweets <- rbind(ex1, ex2, ex3)

# remove temporary dataframes 
rm(ex1)
rm(ex2)
rm(ex3)

# Save data as .csv 
setwd('/Users/bijeanghafouri/Code/extremist-contagion/data')
fwrite(tweets, 'tweets-qanon-clean.csv')


