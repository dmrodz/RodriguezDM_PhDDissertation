print('Library call...')
packages = c('tidyverse', 'tm', 'caret', 'crfsuite', 'tictoc', 
             'pryr', 'benchmarkme', 'stringdist', 'ggplot2',
             'micemd', 'proxy', 'class', 'mice','CRF','quanteda',
             'data.table', 'hunspell','reticulate','keras')
print(packages)
################################################################################
print('Install CRAN packages (if not already installed)...')

installed <- packages %in% installed.packages()
if(length(packages[!installed]) > 0) install.packages(packages[!installed])
################################################################################
print('Loading packages into session') 

lapply(packages, require, character.only = TRUE, quietly = TRUE)
################################################################################
print('Starting the data import process...')

# Set paths
path = 'your/prefered/path'
setwd(path)
print('Importing Gold Standard dictionary...')
gold_standard <- read.csv('gold_standard_dictionary.csv',
                                     header = TRUE, 
                                     stringsAsFactors = FALSE) %>%
  mutate(
    Class_Identifier = paste0(Class, ':', Label, ':', Entity),
    Identifier = paste0(Label, ':', Entity)
  )
 
# Final parameters per dataset
hyperparameters <- read.csv(paste0(getwd(),
                                   '/hp_final_parameters.csv'),
                            header = TRUE,
                            stringsAsFactors = FALSE)
# Importing prepared training data
for(ii in 1:5) {
  training_data_balanced <- read.csv(paste0(getwd(), '/train_balanced_data ', 
                                             ii, '.csv'),
                                      header = TRUE,
                                      stringsAsFactors = FALSE)
  labeled_test_data <- read.csv(paste0(getwd(), '/labeled_test_data ', ii, 
                                        '.csv'),
                                      header = TRUE,
                                      stringsAsFactors = FALSE)
  D0 <- read.csv(paste0(getwd(), '/D0_', ii, '.csv'),
                  header = TRUE,
                  stringsAsFactors = FALSE)
  # These could be commented out for faster runs
  source('crf_ed_layer.R')
  rm(list=setdiff(ls(), c("D0", "labeled_test_data", "training_data_balanced",
                          "hyperparameters", "gold_standard", "ii")))
  
  source('crf_edk_layer.R')
  rm(list=setdiff(ls(), c("D0", "labeled_test_data", "training_data_balanced",
                          "hyperparameters", "gold_standard", "ii")))
   
  source('bilstm_ed_layer.R')
  rm(list=setdiff(ls(), c("D0", "labeled_test_data", "training_data_balanced",
                          "hyperparameters", "gold_standard", "ii")))
    
  source('bilstm_edk_layer.R')
  rm(list=setdiff(ls(), c("D0", "labeled_test_data", "training_data_balanced",
                          "hyperparameters", "gold_standard", "ii")))
}
