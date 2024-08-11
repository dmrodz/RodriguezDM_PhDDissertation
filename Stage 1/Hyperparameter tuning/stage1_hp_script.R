## This code will be sourced by stage1.R
################################################################################
print('Hypertuning an aggregated dataset composed of 5 randomly selected datasets.')
################################################################################
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
path = 'your/path/here'
setwd(path)
print('Importing COPA Individual file...')
ind_data <- read.csv('data/copa_ind.csv', header = TRUE, stringsAsFactors = FALSE)

print('Importing COPA KAP file...')
kap_data <- read.csv('data/copa_kap.csv', header = TRUE, stringsAsFactors = FALSE)

print('Importing COPA House Representative file...')
hr_data <- read.csv('data/copa_hh.csv', header = TRUE, stringsAsFactors = FALSE) %>%
  select(-FUMAGATION_COMM_12MO)

merged_data <- Reduce(function(...) merge(..., all = TRUE), 
                       list(ind_data, kap_data, hr_data))

print('Importing Gold Standard dictionary...')
gold_standard_dictionary <- read.csv('gold_standard_dictionary.csv',
                                     header = TRUE, 
                                     stringsAsFactors = FALSE) %>%
  mutate(
    Class_Identifier = paste0(Class, ':', Label, ':', Entity),
    Identifier = paste0(Label, ':', Entity)
  )
 
################################################################################
# If you'd like to recreate the synthetic datasets, un-comment this code.
# print('Generate synthetic datasets and store in front-end memory...')
# source('synthetic_data_generation.R')
# full_file_names <- c(paste0(getwd(), '/data/synthetic_data', 1:20, '.csv'))
# file_names <- c(paste0('synthetic_data', 1:20, '.csv'))
# synth_files <- lapply(full_file_names, read.csv, header = TRUE,
#                       stringsAsFactors = FALSE)
#
# print('Randomly select 4 synthetic datasets for testing...')
# set.seed(12345)
# selected_datasets_testing <- sample(synth_files, 4)
# for (i in seq_along(selected_datasets_testing)) {
#   filename <- paste0('synthetic_test_dataset_', i, ".csv")
#   write.csv(selected_datasets_testing[[i]], file = filename, row.names = FALSE)
#   cat(sprintf("Dataset %d saved as '%s'\n", i, filename))
# }
# write.csv(merged_data, paste0(getwd(), '/merged_raw_data.csv'), row.names = FALSE)

# Importing synthetic datasets for later testing...
full_file_names <- c(paste0(getwd(), '/synthetic_test_dataset_', 1:4, '.csv'))
file_names <- c(paste0('synthetic_test_dataset_', 1:4, '.csv'))
synth_files <- lapply(full_file_names, read.csv, header = TRUE,
                      stringsAsFactors = FALSE)
################################################################################
set.seed(56789)
rawdata_list <- c(list(merged_data), synth_files)
## LOOP STARTS HERE FOR EACH DATASET
## Four code files will be sourced for the CRF and Bi-LSTM models, for each layer
for(ii in 1) {

source('data_preparation.R')

start_time1 <- Sys.time()
source('crf_hp_ed_layer.R')
end_time <- Sys.time()
print(paste0('CRF ED-Layer was run in ',
             round(difftime(end_time, start_time1, units = 'secs'), 1), ' secs',
             'for data ', ii))
start_time <- Sys.time()
source('crf_hp_edk_layer.R')
end_time <- Sys.time()
print(paste0('CRF EDK-Layer was run in ',
             round(difftime(end_time, start_time, units = 'secs'), 1), ' secs',
             'for data ', ii))
start_time <- Sys.time()
source('bilstm_hp_ed_layer.R')
end_time <- Sys.time()
print(paste0('Bi-LSTM ED-Layer was run in ',
             round(difftime(end_time, start_time, units = 'secs'), 1), ' secs',
             'for data ', ii))
start_time <- Sys.time() 
source('bilstm_hp_edk_layer.R')
end_time1 <- Sys.time()
print(paste0('Bi-LSTM EDK-Layer was run in ', 
             round(difftime(end_time1, start_time, units = 'secs'), 1), ' secs',
             'for data ', ii))
print(paste0('Hyoertuning for he four layers for data ', ii, ' were run in ', 
             round(difftime(end_time1, start_time1, units = 'secs'), 1), ' secs'))

}
