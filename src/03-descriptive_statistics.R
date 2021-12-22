# Descriptive statistics 
setwd(here())

# Import data if collected
source('/Users/bijeanghafouri/Code/extremist-contagion/02-adoption_threshold.R')



# Distribution of ideal points of hashtags -------------------------------------
# Republicans ------
democrat_hashtag_plots <- ridges_plot_democrat()
democrat_hashtag_plots


# Democrats ------
republican_hashtag_plots <- ridges_plot_republican()
republican_hashtag_plots


# Threshold parameters and descriptive statistics ---------------------------------
# Republicans -----------------------------------
# Make list of datasets ------
datasets <- list('trump2020' = trump2020, 'qanon' = qanon, 'obamagate' = obamagate, 'wwg1wga' = wwg1wga, 'whitelivesmatter' = whitelivesmatter, 'stopthesteal' = stopthesteal)

# Thresholds and descriptive statistics ------
# Statistics on users, exposures and ideal points
republican_descriptives <- desc_table(datasets)
republican_descriptives

# latex output 
xtable(republican_descriptives)


# Democrats -------------------------------------
# Datasets
datasets <- list('biden2020' = biden2020, 'boycottgoya' = boycottgoya, 'tre45on' = tre45on, 'blacklivesmatter' = blacklivesmatter, 'blm' = blm, 'acab' = acab, 'abolishthepolice' = abolishthepolice, 'defundthepolice' = defundthepolice, 'trumpvirus' = trumpvirus, 'whitesupremacy' = whitesupremacy)

# Statistics
democrat_descriptives <- desc_table(datasets)
democrat_descriptives

# latex output 
xtable(democrat_descriptives)




# ---------------------------- Ridge plots of threshold parameters
# Republicans ----------------
ridges_plot_republican_adoption()

# Democrats ------------------
ridges_plot_democrat_adoption()


