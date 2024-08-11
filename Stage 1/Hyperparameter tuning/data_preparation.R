################################################################################
print('Define variable classes...')
################################################################################

ind_data$ILL0_HOSPITALIZED_DAYS <- as.numeric(ind_data$ILL0_HOSPITALIZED_DAYS)
ind_data$ODATE <- as.Date(ind_data$ODATE)
ind_data$COVID_POSITIVE_DT <- as.Date(ind_data$COVID_POSITIVE_DT)

################################################################################
print('Removing completely empty columns.')
################################################################################

numvars_ind <- names(ind_data)[sapply(ind_data, is.numeric)]
numvars_kap <- names(kap_data)[sapply(kap_data, is.numeric)]
numvars_hr <- names(hr_data)[sapply(hr_data, is.numeric)]
numvars <- c(unique(numvars_ind), unique(numvars_kap), unique(numvars_hr))

datevars_ind <- names(ind_data)[sapply(ind_data, is.Date)]
datevars_kap <- names(kap_data)[sapply(kap_data, is.Date)]
datevars_hr <- names(hr_data)[sapply(hr_data, is.Date)]
datevars <- c(unique(datevars_ind), unique(datevars_kap), unique(datevars_hr))

textvars_ind <- names(ind_data)[sapply(ind_data, is.character)]
textvars_kap <- names(kap_data)[sapply(kap_data, is.character)]
textvars_hr <- names(hr_data)[sapply(hr_data, is.character)]
textvars <- c(unique(textvars_ind), unique(textvars_kap), unique(textvars_hr))


################################################################################
print('Data pre-processing. Making corpus & preliminary dictionary for training.')
################################################################################
## Splitting criteria 80/20
random_split_data <- function(data, train_frac = 0.8) {
  set.seed(54689)
  trainIndex <- sample(1:nrow(data), floor(train_frac * nrow(data)),
                       replace = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-c(trainIndex), ]
  
  return(list(train = trainData, test = testData))
}

splits <- random_split_data(rawdata_list[[ii]])
training_data <- splits$train
testing_data <- splits$test


# Function call
preprocess_text_data <- function(data) {
  textvars <- names(data)[sapply(data, is.character)]
  text_data <- subset(data, select = textvars)
  text_values <- unlist(text_data)
  corpus <- Corpus(VectorSource(text_values))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, function(x) gsub('_', ' ', x))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, function(x) iconv(x, to = 'ASCII//TRANSLIT'))
  
  return(list(corpus = corpus, textvars = textvars, text_data = text_data))
}

train_text <- preprocess_text_data(training_data) 
test_text <- preprocess_text_data(testing_data) 

# Function call
generate_text_dictionary <- function(corpus, text_data) {
  text_labels <- rep(names(text_data), each = nrow(text_data))
  labeled_textdata <- data.frame(
    Entity = sapply(corpus, as.character),
    Label = text_labels
  )
  return(labeled_textdata)
}

# Generate dictionaries
labeled_train_data <- generate_text_dictionary(train_text$corpus, 
                                               train_text$text_data) %>%
  filter(Entity != '') %>%
  filter(!Label %in% c('HHID_ID', 'PART_ID', 'ODATE', 'COVID_POSITIVE_DT')) %>%
  mutate(Type = gold_standard_dictionary$Type[match(Label, gold_standard_dictionary$Label)],
         Class = gold_standard_dictionary$Class[match(Label, gold_standard_dictionary$Label)])


labeled_test_data <- generate_text_dictionary(train_text$corpus, 
                                              train_text$text_data) %>%
  filter(Entity != '') %>%
  filter(!Label %in% c('HHID_ID', 'PART_ID', 'ODATE', 'COVID_POSITIVE_DT')) %>%
  mutate(Type = gold_standard_dictionary$Type[match(Label, gold_standard_dictionary$Label)],
         Class = gold_standard_dictionary$Class[match(Label, gold_standard_dictionary$Label)])


ed_dictionary <- labeled_train_data %>%
  filter(!Label %in% c('HHID_ID', 'PART_ID', 'ODATE', 'COVID_POSITIVE_DT'))

num_rows <- data.frame(
  Label = character(),
  Entity = character(),
  Type = character(),
  Class = character(),
  stringsAsFactors = FALSE
)
existing_vars <- intersect(numvars, names(rawdata_list[[ii]]))

if (length(existing_vars) > 0) {
  num_rows <- data.frame(
    Label = existing_vars, 
    Entity = sapply(existing_vars, function(var) {
      min_val <- min(rawdata_list[[ii]][[var]], na.rm = TRUE)
      max_val <- max(rawdata_list[[ii]][[var]], na.rm = TRUE)
      paste0(min_val, ',', max_val)
    }), 
    Type = 'numeric',
    stringsAsFactors = FALSE
  )
}

num_rows <- num_rows %>%
  mutate(
    Class = gold_standard_dictionary$Class[match(Label, gold_standard_dictionary$Label)],
  )

date_rows <- data.frame(
  Label = character(),
  Entity = character(),
  Type = character(),
  Class = character(),
  stringsAsFactors = FALSE
)

existing_vars <- intersect(datevars, names(rawdata_list[[ii]]))

if (length(existing_vars) > 0) {
  date_rows <- data.frame(
    Label = existing_vars, 
    Entity = sapply(existing_vars, function(var) {
      min_val <- min(rawdata_list[[ii]][[var]], na.rm = TRUE)
      max_val <- max(rawdata_list[[ii]][[var]], na.rm = TRUE)
      paste0(min_val, ',', max_val)
    }), 
    Type = 'date',
    stringsAsFactors = FALSE
  )
}
date_rows <- date_rows %>%
  mutate(
    Class = gold_standard_dictionary$Class[match(Label, gold_standard_dictionary$Label)],
  )
ed_dictionary <- do.call(rbind, list(ed_dictionary[!duplicated(ed_dictionary), ],
                                     num_rows,
                                     date_rows))
D1 <- ed_dictionary[!duplicated(ed_dictionary), ]
write.csv(D1, paste0(getwd(), '/D1_', ii, '.csv'), row.names = FALSE)

################################################################################
print('Feature selection: TF_IDF feature selection for training data only.')
################################################################################
# Functions call
compute_tfidf_features_per_label <- function(data) {
  # Combine all entities for each label into a single document for TF-IDF calculation
  data_combined <- data %>%
    group_by(Label) %>%
    summarize(CombinedEntity = paste(Entity, collapse = ", ")) %>%
    ungroup()
  
  # Calculate term frequencies (TF) for each label
  data_tf <- data_combined %>%
    rowwise() %>%
    mutate(tf = list(as.data.frame(table(unlist(strsplit(CombinedEntity, ", "))))))
  
  # Un-nest the list column and rename the columns appropriately
  data_tf <- data_tf %>%
    unnest(tf) %>%
    rename(sentence = Var1, tf = Freq) %>%
    ungroup()
  
  # Calculate document frequencies (DF)
  df <- data_tf %>%
    group_by(sentence) %>%
    summarise(df = n_distinct(Label)) %>%
    ungroup()
  
  # Calculate inverse document frequencies (IDF)
  total_labels <- n_distinct(data_tf$Label)
  df <- df %>%
    mutate(idf = log(total_labels / df))
  
  # Compute TF-IDF
  result <- data_tf %>%
    inner_join(df, by = 'sentence') %>%
    mutate(tfidf = tf * idf,
           Identifier = paste0(Label, ':', sentence)) %>%
    select(Label, Identifier, sentence, tfidf, idf)
  
  return(result)
}

train_tfidf_data <- compute_tfidf_features_per_label(labeled_train_data)
train_tfidf_data <- as.data.frame(train_tfidf_data) %>%
  mutate(
    Type = gold_standard_dictionary$Type[match(Label, gold_standard_dictionary$Label)],
    Class = gold_standard_dictionary$Class[match(Label, gold_standard_dictionary$Label)],
    Entity = sentence) %>%
  group_by(Label) %>%
  mutate(Identifier = paste0(Label, ':', Entity)) %>%
  select(-sentence)


################################################################################
print('Data balancing for the training set: Applying a customized overSampling technique.')
################################################################################
# Function call
oversample_minority <- function(data, target_column) {
  # Check if target_column exists in the data
  if (!(target_column %in% colnames(data))) {
    stop("The target column does not exist in the data.")
  }
  
  # Get the number of instances per class
  class_counts <- table(data[[target_column]])
  max_count <- max(class_counts)
  
  # Ensure target_column is a factor for consistent behavior
  data[[target_column]] <- as.factor(data[[target_column]])
  
  # Perform oversampling
  oversampled_data <- data %>%
    group_by(across(all_of(target_column))) %>%
    group_split() %>%
    lapply(function(class_data) {
      n <- nrow(class_data)
      if (n < max_count) {
        # Sample with replacement to balance the class size
        oversampled_class_data <- class_data %>%
          slice_sample(n = max_count, replace = TRUE)
      } else {
        oversampled_class_data <- class_data
      }
      return(oversampled_class_data)
    }) %>%
    bind_rows()
  
  # Ensure that `Entity` column is not affected
  oversampled_data <- oversampled_data %>%
    group_by(across(all_of(target_column))) %>%
    arrange(Entity) %>%
    ungroup()
  
  # Return the oversampled data
  return(oversampled_data)
}

################################################################################
print('Balancing training dataset...')
################################################################################
training_data_balanced <- oversample_minority(train_tfidf_data, 'Entity') %>%
  filter(!is.na(Class)) %>%
  filter(!Label %in% c('COVID_POSITIVE_DT', 'ODATE', 'HHID_ID', 'PART_ID')) %>%
  mutate(Entity = as.character(Entity),
         Class = as.character(Class))

write.csv(training_data_balanced, 
          paste0(getwd(), '/train_balanced_data ', ii, '.csv'),
          row.names = FALSE)

write.csv(labeled_test_data,
          paste0(getwd(), '/labeled_test_data ', ii, '.csv'),
          row.names = FALSE)
                   
