# Adoption threshold with retweet graph 
# Read data
setwd(here())
#source('01-read_data.R')

# Import data if collected
source('00-setup.R')

# Republican hashtags ------------------------------------- 
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




# Democrat hashtags ------------------------------------- 
# boycottgoya
data <- 'data/tweets-boycottgoya-clean.csv'
boycottgoya <- adoption_threshold(data, 'boycottgoya', 'democrat')


# tre45on
data <- 'data/tweets-tre45on-clean.csv'
tre45on <- adoption_threshold(data, 'tre45on', 'democrat')


# blacklivesmatter
data <- 'data/tweets-blacklivesmatter-clean.csv'
blacklivesmatter <- adoption_threshold(data, 'blacklivesmatter', 'democrat')


# blm
data <- 'data/tweets-blm-clean.csv'
blm <- adoption_threshold(data, 'blm', 'democrat')


# acab
data <- 'data/tweets-acab-clean.csv'
acab <- adoption_threshold(data, 'acab', 'democrat')


# abolishthepolice
data <- 'data/tweets-abolishthepolice-clean.csv'
abolishthepolice <- adoption_threshold(data, 'abolishthepolice', 'democrat')


# defundthepolice
data <- 'data/tweets-defundthepolice-clean.csv'
defundthepolice <- adoption_threshold(data, 'defundthepolice', 'democrat')


# trumpvirus
data <- 'data/tweets-trumpvirus-clean.csv'
trumpvirus <- adoption_threshold(data, 'trumpvirus', 'democrat')


# whitesupremacy
data <- 'data/tweets-whitesupremacy-clean.csv'
whitesupremacy <- adoption_threshold(data, 'whitesupremacy', 'democrat')


# biden2020 (control)
data <- 'data/tweets-biden2020-clean.csv'
biden2020 <- adoption_threshold(data, 'biden2020', 'democrat')