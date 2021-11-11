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


