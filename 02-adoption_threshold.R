# Adoption threshold with retweet graph 
# Read data
setwd(here())
#source('01-read_data.R')

# Import data if collected
source('00-setup.R')

# qanon 
data <- 'data/tweets-qanon-clean_v2.csv'
qanon <- adoption_threshold(data, 'qanon', 'republican')


# stopthesteal
data <- 'data/tweets-stopthesteal-clean.csv'
stopthesteal <- adoption_threshold(data, 'stopthesteal', 'republican')


# whitelivesmatter
data <- 'data/tweets-whitelivesmatter-clean.csv'
whitelivesmatter <- adoption_threshold(data, 'whitelivesmatter', 'republican')


# wwg1wga
data <- 'data/tweets-wwg1wga-clean.csv'
wwg1wga <- adoption_threshold(data, 'wwg1wga', 'republican')


# obamagate
data <- 'data/tweets-obamagate-clean.csv'
obamagate <- adoption_threshold(data, 'obamagate', 'republican')


# trump2020
data <- 'data/tweets-trump2020-clean.csv'
trump2020 <- adoption_threshold(data, 'trump2020', 'republican')

