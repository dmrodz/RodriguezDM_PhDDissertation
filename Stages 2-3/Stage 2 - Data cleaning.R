packages = c('tidyverse','stringdist','micemd','proxy', 'class', 'mice',
             'data.table','doParallel')
installed <- packages %in% installed.packages()
if(length(packages[!installed]) > 0) install.packages(packages[!installed])
lapply(packages, require, character.only = TRUE, quietly = TRUE)

# Set paths
path = "your/path/here"
setwd(path)


# Data import
# Gold standard dictionary
GS <- read.csv(paste0(getwd(), '/gold_standard_dictionary.csv'), 
               header = TRUE, stringsAsFactors = FALSE)

DS1 <- read.csv(paste0(getwd(), '/dataset_1.csv'), header = TRUE, stringsAsFactors = FALSE) %>%
  select(-FUMAGATION_COMM_12MO, -ILL0_HOSPITALIZED_DAYS)
DS2 <- read.csv(paste0(getwd(), '/dataset_2.csv'), header = TRUE, stringsAsFactors = FALSE)
DS3 <- read.csv(paste0(getwd(), '/dataset_3.csv'), header = TRUE, stringsAsFactors = FALSE)
DS4 <- read.csv(paste0(getwd(), '/dataset_4.csv'), header = TRUE, stringsAsFactors = FALSE)
DS5 <- read.csv(paste0(getwd(), '/dataset_5.csv'), header = TRUE, stringsAsFactors = FALSE)
# Dictionary import
D1_1 <- read.csv(paste0(getwd(), '/D1_1.csv'), header = TRUE, stringsAsFactors = FALSE)
D1_2 <- read.csv(paste0(getwd(), '/D1_2.csv'), header = TRUE, stringsAsFactors = FALSE)
D1_3 <- read.csv(paste0(getwd(), '/D1_3.csv'), header = TRUE, stringsAsFactors = FALSE)
D1_4 <- read.csv(paste0(getwd(), '/D1_4.csv'), header = TRUE, stringsAsFactors = FALSE)
D1_5 <- read.csv(paste0(getwd(), '/D1_5.csv'), header = TRUE, stringsAsFactors = FALSE)

D0_1 <- read.csv(paste0(getwd(), '/D0_1.csv'), header = TRUE, stringsAsFactors = FALSE)
D0_2 <- read.csv(paste0(getwd(), '/D0_2.csv'), header = TRUE, stringsAsFactors = FALSE)
D0_3 <- read.csv(paste0(getwd(), '/D0_3.csv'), header = TRUE, stringsAsFactors = FALSE)
D0_4 <- read.csv(paste0(getwd(), '/D0_4.csv'), header = TRUE, stringsAsFactors = FALSE)
D0_5 <- read.csv(paste0(getwd(), '/D0_5.csv'), header = TRUE, stringsAsFactors = FALSE)

D0_1 <- D0_1[!duplicated(D0_1), ]
D0_2 <- D0_2[!duplicated(D0_2), ]
D0_3 <- D0_3[!duplicated(D0_3), ]
D0_4 <- D0_4[!duplicated(D0_4), ]
D0_5 <- D0_5[!duplicated(D0_5), ]

D1_1 <- D1_1[!duplicated(D1_1), ]
D1_2 <- D1_2[!duplicated(D1_2), ]
D1_3 <- D1_3[!duplicated(D1_3), ]
D1_4 <- D1_4[!duplicated(D1_4), ]
D1_5 <- D1_5[!duplicated(D1_5), ]

D0_1 <- D0_1 %>% left_join(GS %>% select(Label, contains('TEXT_RULES')), 
                           by = c('Label'))

D0_2 <- D0_2 %>% left_join(GS %>% select(Label, contains('TEXT_RULES')), 
                           by = c('Label'))

D0_3 <- D0_3 %>% left_join(GS %>% select(Label, contains('TEXT_RULES')), 
                           by = c('Label'))

D0_4 <- D0_4 %>% left_join(GS %>% select(Label, contains('TEXT_RULES')), 
                           by = c('Label'))

D0_5 <- D0_5 %>% left_join(GS %>% select(Label, contains('TEXT_RULES')), 
                           by = c('Label'))

# Variable classes re-assignments
datevars <- GS$Label[which(GS$Type == 'date')]
numvars <- GS$Label[which(GS$Type == 'numeric')]
textvars <- GS$Label[which(GS$Type == 'text')]

DS1 <- DS1 %>% mutate_if(names(DS1) %in% datevars, as.Date)
DS1 <- DS1 %>% mutate_if(names(DS1) %in% numvars, as.numeric)
DS1 <- DS1 %>% mutate_if(names(DS1) %in% textvars, as.character)

DS2 <- DS2 %>% mutate_if(names(DS2) %in% datevars, as.Date)
DS2 <- DS2 %>% mutate_if(names(DS2) %in% numvars, as.numeric)
DS2 <- DS2 %>% mutate_if(names(DS2) %in% textvars, as.character)

DS3 <- DS3 %>% mutate_if(names(DS3) %in% datevars, as.Date)
DS3 <- DS3 %>% mutate_if(names(DS3) %in% numvars, as.numeric)
DS3 <- DS3 %>% mutate_if(names(DS3) %in% textvars, as.character)

DS4 <- DS4 %>% mutate_if(names(DS4) %in% datevars, as.Date)
DS4 <- DS4 %>% mutate_if(names(DS4) %in% numvars, as.numeric)
DS4 <- DS4 %>% mutate_if(names(DS4) %in% textvars, as.character)

DS5 <- DS5 %>% mutate_if(names(DS5) %in% datevars, as.Date)
DS5 <- DS5 %>% mutate_if(names(DS5) %in% numvars, as.numeric)
DS5 <- DS5 %>% mutate_if(names(DS5) %in% textvars, as.character)

################################################################################
## Functions call
find_closest_match <- function(value, label, dictionary) {
  # Convert the input value to lowercase
  value <- tolower(value)
  
  # Subset the dictionary based on label
  subset_gs <- dictionary %>% filter(Label == label)
  
  # Convert the dictionary values to lowercase
  subset_gs$Value_lower <- tolower(subset_gs$Entity)
  
  # Compute Jaccard distances
  distances <- stringdist::stringdist(value, subset_gs$Value_lower, method = 'jaccard')
  
  # Find the index of the minimum distance
  closest_index <- which.min(distances)
  
  # Return the closest match value with original case from dictionary
  return(subset_gs$Entity[closest_index])
}

# Vectorize the find_closest_match function
find_closest_match_vec <- Vectorize(find_closest_match, vectorize.args = c('value', 'label'))

# Define the function to process the dataset
clean_text_values <- function(data, dictionary) {
  # Convert the data from wide to long format
  
  datevars <- unique(GS$Label[which(GS$Type == 'date')])
  numvars <- unique(GS$Label[which(GS$Type == 'numeric')])
  textvars <- c('HHID_ID', 'PART_ID', unique(GS$Label[which(GS$Type == 'text')]))
  freetextvars <- c('HHID_ID', 'PART_ID', unique(GS$Label[which(GS$Type == 'free text field')]))
  
  
  text_data <- subset(data, select = c(textvars)) %>%
    mutate(across(everything(), ~ gsub("_", " ", .x))) %>%
    mutate(across(everything(), ~ tolower(.)))
  
  non_textdata <- select(data, HHID_ID, PART_ID, any_of(numvars), any_of(datevars)) %>%
    mutate(PART_ID = tolower(PART_ID),
           HHID_ID = tolower(HHID_ID))
  non_textdata2 <- select(data, HHID_ID, PART_ID, any_of(freetextvars)) %>%
    mutate(PART_ID = tolower(PART_ID),
           HHID_ID = tolower(HHID_ID))
  
  long_data <- text_data %>%
    pivot_longer(-c(PART_ID, HHID_ID), names_to = 'Label', values_to = 'Value') 
  
  # Apply the function to each non-missing value
  long_data <- long_data %>%
    mutate(
      Corrected_Value = ifelse(!is.na(Value) & Value != '', 
                               find_closest_match_vec(Value, Label, dictionary), 
                               Value)) %>%
    unnest(Corrected_Value, keep_empty = TRUE)
  
  # Convert the data back to wide format
  wide_data <- long_data %>%
    select(PART_ID, HHID_ID, Label, Corrected_Value) %>%
    pivot_wider(names_from = Label, values_from = Corrected_Value)
  
  # Clearing up space
  remove(long_data)  
  remove(text_data)
  
  
  # Restore the original column order
  wide_data <- wide_data %>%
    # select(all_of(textvars)) %>%
    left_join(non_textdata, by = c('PART_ID', 'HHID_ID')) %>%
    left_join(non_textdata2, by = c('PART_ID', 'HHID_ID')) %>%
    select(names(data))
  
  return(wide_data)
}
## Generating the cleaned text-based dataset that underwent Jaccard distance clean-up for both layers...

DS01_1 <- clean_text_values(DS1, D1_1)
DS00_1 <- clean_text_values(DS1, D0_1)

DS01_2 <- clean_text_values(DS2, D1_2)
DS00_2 <- clean_text_values(DS2, D0_2)

DS01_3 <- clean_text_values(DS3, D1_3)
DS00_3 <- clean_text_values(DS3, D0_3)

DS01_4 <- clean_text_values(DS4, D1_4)
DS00_4 <- clean_text_values(DS4, D0_4)

DS01_5 <- clean_text_values(DS5, D1_5)
DS00_5 <- clean_text_values(DS5, D0_5)

################################################################################
## Function call
# Replacing values outside the expected range with NA
drop_outliers_from_expected_range <- function(data, dictionary) {
  dictionary <- dictionary[dictionary$Type %in% c('numeric', 'date'), ] %>%
    filter(Label != 'ILL0_HOSPITALIZED_DAYS')
  for(i in seq_len(nrow(dictionary))) {
    label <- dictionary$Label[i]
    value <- dictionary$Entity[i]
    type <- dictionary$Type[i]
    
    # Parse the min and max values
    min_val <- ifelse(type == 'numeric', as.numeric(strsplit(value, ',')[[1]][1]),
                      as.character(strsplit(value, ',')[[1]][1]))
    
    max_val <- ifelse(type == 'numeric', as.numeric(strsplit(value, ',')[[1]][2]),
                      as.character(strsplit(value, ',')[[1]][2]))
    
    # Replace values outside the range with NA
    data <- data %>%
      mutate_if(is.Date, as.character) %>%
      mutate(!!label := ifelse(get(label) >= min_val & 
                               get(label) <= max_val, get(label), NA))
  }
  
  data <- data %>%
    mutate_at(datevars, ~ as.Date(.x, format = '%Y-%m-%d'))
  return(data)
}

DS01_1 <- as.data.frame(drop_outliers_from_expected_range(DS01_1, D1_1))
DS01_1[DS01_1 == ''] <- NA

DS01_2 <- as.data.frame(drop_outliers_from_expected_range(DS01_2, D1_2))
DS01_2[DS01_2 == ''] <- NA

DS01_3 <- as.data.frame(drop_outliers_from_expected_range(DS01_3, D1_3))
DS01_3[DS01_3 == ''] <- NA

DS01_4 <- as.data.frame(drop_outliers_from_expected_range(DS01_4, D1_4))
DS01_4[DS01_4 == ''] <- NA

DS01_5 <- as.data.frame(drop_outliers_from_expected_range(DS01_5, D1_5))
DS01_5[DS01_5 == ''] <- NA

#####

DS00_1 <- as.data.frame(drop_outliers_from_expected_range(DS00_1, D0_1))
DS00_1[DS00_1 == ''] <- NA

DS00_2 <- as.data.frame(drop_outliers_from_expected_range(DS00_2, D0_2))
DS00_2[DS00_2 == ''] <- NA

DS00_3 <- as.data.frame(drop_outliers_from_expected_range(DS00_3, D0_3))
DS00_3[DS00_3 == ''] <- NA

DS00_4 <- as.data.frame(drop_outliers_from_expected_range(DS00_4, D0_4))
DS00_4[DS00_4 == ''] <- NA

DS00_5 <- as.data.frame(drop_outliers_from_expected_range(DS00_5, D0_5))
DS00_5[DS00_5 == ''] <- NA

################################################################################
get_mode <- function(x) {
  ux <- na.omit(x)
  uniq_x <- unique(ux)
  uniq_x[which.max(tabulate(match(ux, uniq_x)))]
}

# Simplified imputation function
simple_impute <- function(df) {
  # Copy the dataframe to avoid modifying the original data
  df_imputed <- df
  
  # Identify numerical and categorical columns
  num_vars <- sapply(df_imputed, is.numeric)
  cat_vars <- sapply(df_imputed, is.factor) | sapply(df_imputed, is.character)
  
  # Impute numerical variables with mean
  num_vars <- names(num_vars)[num_vars]
  for (var in num_vars) {
    if (any(is.na(df_imputed[[var]]))) {
      df_imputed[[var]][is.na(df_imputed[[var]])] <- mean(df_imputed[[var]], na.rm = TRUE)
    }
  }
  
  # Impute categorical variables with mode
  cat_vars <- names(cat_vars)[cat_vars]
  for (var in cat_vars) {
    if (any(is.na(df_imputed[[var]]))) {
      df_imputed[[var]][is.na(df_imputed[[var]])] <- get_mode(df_imputed[[var]])
    }
  }
  
  return(df_imputed)
}

## Imputing data for ED-Layer...
DS01_1 <- simple_impute(DS01_1)
DS01_2 <- simple_impute(DS01_2)
DS01_3 <- simple_impute(DS01_3)
DS01_4 <- simple_impute(DS01_4)
DS01_5 <- simple_impute(DS01_5)


## Imputing data for EDK-Layer...
DS00_1 <- simple_impute(DS00_1)
DS00_2 <- simple_impute(DS00_2)
DS00_3 <- simple_impute(DS00_3)
DS00_4 <- simple_impute(DS00_4)
DS00_5 <- simple_impute(DS00_5)

################################################################################
# Function to apply rules to data
apply_rules <- function(data, dictionary) {
  # Convert data values to lowercase and date columns to character
  data <- data %>%
    mutate(COVID_POSITIVE_DT = as.character(COVID_POSITIVE_DT),
           ODATE = as.character(ODATE)) %>%
    mutate(across(where(is.character), tolower))
  
  # Ensure dictionary is filtered to remove NAs in TEXT_RULES_VAR
  dictionary <- filter(dictionary, !is.na(TEXT_RULES_VAR))
  
  # Iterate over the rules
  for (i in seq_len(nrow(dictionary))) {
    rule <- dictionary[i, ]
    rule_var <- rule$TEXT_RULES_VAR
    rule_val <- unlist(strsplit(rule$TEXT_RULES_VAL, ','))
    target_var <- rule$Label
    target_val <- rule$Entity
    
    # Apply rules to the data
    data <- data %>%
      mutate(
        !!target_var := ifelse(
          get(rule_var) %in% rule_val, get(target_var), 'N/A')
      )
  }
  
  # Convert specific columns to character after applying rules
  data <- data %>%
    mutate(COVID_POSITIVE_DT = as.character(COVID_POSITIVE_DT),
           ODATE = as.character(ODATE))
  
  return(data)
}

DS00_1 <- apply_rules(DS00_1, D0_1)
DS00_1[DS00_1==''] <- NA

DS00_2 <- apply_rules(DS00_2, D0_2)
DS00_2[DS00_2==''] <- NA

DS00_3 <- apply_rules(DS00_3, D0_3)
DS00_3[DS00_3==''] <- NA

DS00_4 <- apply_rules(DS00_4, D0_4)
DS00_4[DS00_4==''] <- NA

DS00_5 <- apply_rules(DS00_5, D0_5)
DS00_5[DS00_5==''] <- NA



write.csv(DS01_1, paste0(getwd(), '/results_stage2/DS01_1.csv'), row.names = FALSE)
write.csv(DS01_2, paste0(getwd(), '/results_stage2/DS01_2.csv'), row.names = FALSE)
write.csv(DS01_3, paste0(getwd(), '/results_stage2/DS01_3.csv'), row.names = FALSE)
write.csv(DS01_4, paste0(getwd(), '/results_stage2/DS01_4.csv'), row.names = FALSE)
write.csv(DS01_5, paste0(getwd(), '/results_stage2/DS01_5.csv'), row.names = FALSE)

write.csv(DS00_1, paste0(getwd(), '/results_stage2/DS00_1.csv'), row.names = FALSE)
write.csv(DS00_2, paste0(getwd(), '/results_stage2/DS00_2.csv'), row.names = FALSE)
write.csv(DS00_3, paste0(getwd(), '/results_stage2/DS00_3.csv'), row.names = FALSE)
write.csv(DS00_4, paste0(getwd(), '/results_stage2/DS00_4.csv'), row.names = FALSE)
write.csv(DS00_5, paste0(getwd(), '/results_stage2/DS00_5.csv'), row.names = FALSE)
