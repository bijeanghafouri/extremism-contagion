# -----------------------------------------------------#
#                 Regression Analysis  
# -----------------------------------------------------#
setwd(here())
source('/Users/bijeanghafouri/Code/extremist-contagion/02-adoption_threshold.R')

# ---------------------- Setup datasets to be ready for regressions ----------------------------------
# This will overwrite existing datasets to: 
# (1) filter for unique users only
# (2) create DV column (whether user adopted or not)
# (3) create IV column (number of exposures)

# *Important*: Only run this section once; otherwise columns will be modified in unintented ways

# ------- Republicans
# create list of dataframes
dataframe_list_republican <- list(trump2020, 
                                  obamagate, 
                                  qanon, 
                                  stopthesteal, 
                                  whitelivesmatter, 
                                  wwg1wga)

# loop over dataframes with `regression_ready` function, which creates DV and IV columns
dataframes <- lapply(dataframe_list_republican, regression_ready)
# rename elements of list 
names(dataframes) = c('trump2020',
              'obamagate',
              'qanon',
              'stopthesteal',
              'whitelivesmatter',
              'wwg1wga')

# unlist list of dataframes. This will overwrite previous dataframes with the dataframes in `dataframes` list
invisible(lapply(names(dataframes),function(x) assign(x, dataframes[[x]],.GlobalEnv)))



# ------- Democrats
# create list of dataframes
dataframe_list_democrat <- list(biden2020,
                                acab,
                                blm,
                                blacklivesmatter,
                                boycottgoya,
                                tre45on,
                                abolishthepolice,
                                defundthepolice,
                                trumpvirus,
                                whitesupremacy)

# loop over dataframes with `regression_ready`, which create DV and IV columns
dataframes <- lapply(dataframe_list_democrat, regression_ready)
# rename elements of list 
names(dataframes) = c("biden2020",
              "acab",
              'blm',
              'blacklivesmatter',
              'boycottgoya',
              'tre45on',
              'abolishthepolice',
              'defundthepolice',
              'trumpvirus',
              'whitesupremacy')

# unlist list of dataframes. This will overwrite previous dataframes with the dataframes in `dataframes` list
invisible(lapply(names(dataframes),function(x) assign(x, dataframes[[x]],.GlobalEnv)))




# ------------------------------------ Regression ----------------------------------------
# DV = `adopted`
# IV = `total_exposures`

# Democrats --------------------------
models <- list(
  "biden2020"     = glm(adopted ~ total_exposures + theta, data = biden2020,
                        family = binomial),
  "acab"     = glm(adopted ~ total_exposures + theta, data =  acab,
                   family = binomial),
  "blacklivesmatter"     = glm(adopted ~ total_exposures + theta, data =  blacklivesmatter,
                               family = binomial),
  "blm"     = glm(adopted ~ total_exposures + theta, data =  blm,
                  family = binomial),
  "boycottgoya" = glm(adopted ~ total_exposures + theta, data =  boycottgoya,
                      family = binomial),
  "tre45on" = glm(adopted ~ total_exposures + theta, data =  tre45on,
                  family = binomial),
  "abolishthepolice" = glm(adopted ~ total_exposures + theta, data =  abolishthepolice,
                           family = binomial),
  "defundthepolice" = glm(adopted ~ total_exposures + theta, data =  defundthepolice,
                          family = binomial),
  "trumpvirus" = glm(adopted ~ total_exposures + theta, data =  trumpvirus,
                     family = binomial),
  "whitesupremacy" = glm(adopted ~ total_exposures + theta, data =  whitesupremacy,
                         family = binomial)
)


# Regression output
cm <- c('(Intercept)' = 'Constant', 'total_exposures' = 'Exposures')
cap <- 'GLM for Democrat hashtags'
democrat_regressions <- modelsummary(models, output = 'kableExtra',
                    stars = TRUE, 
                    coef_map = cm,
                    title = cap, gof_omit = 'IC|Log|Adj') %>% 
  kable_classic_2(full_width = F) %>% row_spec(3, color = 'red')
democrat_regressions

# latex output
democrat_regressions_tex <- modelsummary(models, output = 'latex',
                    stars = TRUE, 
                    coef_map = cm,
                    title = cap, gof_omit = 'IC|Log|Adj') 
democrat_regressions_tex

# Plot regression coefficients
democrat_regression_plot <- modelplot(models, coef_omit = 'Interc|theta') +
  labs(x = 'Coefficients', 
       y = 'Exposure effect',
       title = 'Effect of exposures on adoption likelihood',
       caption = "Tweets related to the 2020 US Presidential Election in July 2020")
democrat_regression_plot




# Republicans --------------------------
# Run regression ---------
# DV = `adopted`
# IV = `total_exposures`

models <- list(
  "trump2020"     = glm(adopted ~ total_exposures + theta, trump2020,
                        family = binomial),
  "obamagate"     = glm(adopted ~ total_exposures + theta, obamagate,
                        family = binomial),
  "qanon"     = glm(adopted ~ total_exposures + theta, qanon,
                    family = binomial),
  "stopthesteal"     = glm(adopted ~ total_exposures + theta, stopthesteal,
                           family = binomial),
  "whitelivesmatter" = glm(adopted ~ total_exposures + theta, whitelivesmatter,
                           family = binomial),
  "wwg1wga" = glm(adopted ~ total_exposures + theta, wwg1wga,
                  family = binomial)
)


cm <- c( '(Intercept)' = 'Constant', 'total_exposures' = 'Exposures', 'theta' = 'ideology')
cap <- 'GLM for Republican hashtags'
republican_regressions <- modelsummary(models, output = 'kableExtra',
                    stars = TRUE, 
                    coef_map = cm,
                    title = cap, gof_omit = 'IC|Log|Adj') %>% 
  kable_classic_2(full_width = F) %>% row_spec(3, color = 'red')
republican_regressions


republican_regressions_tex <- modelsummary(models, output = 'latex',
                    stars = TRUE, 
                    coef_map = cm,
                    title = cap, gof_omit = 'IC|Log|Adj')
republican_regressions_tex



# Plot regression coefficients
republican_regression_plot <- modelplot(models, coef_omit = 'Interc|theta') +
  labs(x = 'Coefficients', 
       y = 'Exposure effect',
       title = 'Effect of exposures on adoption likelihood',
       caption = "Tweets related to the 2020 US Presidential Election in July 2020")
republican_regression_plot
