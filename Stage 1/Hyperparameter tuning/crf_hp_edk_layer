################################################################################
print('Starting CRF EDK-Layer: Domain Knowledge injection... per dataset')
################################################################################

################################################################################
print('Process 4: Incorporate domain knowledge')
## In this case, DK will be introduced using jaccard distances based on the 
## visual inspection of DK expert.
## This code is interactive.
################################################################################
## Function call
# This was done once and then stored to avoid performing the same over 5 hypertuning rounds
inspect_and_inject_domain_knowledge <- function(data, common_words = NULL) {
  data <- filter(data, !Label %in% c('COVID_POSITIVE_DT', 'ODATE',
                                     'HHID_ID',
                                     'PART_ID')) %>%
    filter(Type == 'text')
  unique_labels <- unique(data$Label)
  result <- data.frame()
  
  for (label in unique_labels) {
    repeat {
      cat('Processing label:', label, '\n')
      
      # Filter data by the current label
      filtered_data <- data %>%
        filter(Label == label, Type == 'text')
      
      # Tokenize and count occurrences
      tokenized_data <- filtered_data %>%
        count(Entity, name = "count") %>%
        arrange(desc(count))
      
      # Check spelling if no common words are provided
      if (is.null(common_words)) {
        spell_check <- hunspell(as.character(tokenized_data$Entity))
        tokenized_data$highlight <- ifelse(sapply(spell_check, function(x) 
          length(x) == 0), "Likely Correct", "Likely Incorrect")
      } else {
        # Highlight words based on common words list
        tokenized_data$highlight <- ifelse(tokenized_data$Entity %in% 
                                             common_words, "Common", "Uncommon")
      }
      
      # Display categorized words
      cat('Words categorized as Likely Correct:\n')
      print(tokenized_data %>% filter(highlight == "Likely Correct"), n = Inf)
      
      cat('\nSelect the words to keep from the Likely Correct category (comma separated), type "keep" to keep all or "drop" to drop all:\n')
      correct_words_to_keep <- unlist(strsplit(readline(prompt = 'Words to keep: '), ','))
      correct_words_to_keep <- trimws(correct_words_to_keep)
      
      if (!("drop" %in% tolower(correct_words_to_keep)) && 
          !("keep" %in% tolower(correct_words_to_keep))) {
        correct_words_to_keep <- unique(correct_words_to_keep)
        tokenized_data <- tokenized_data %>%
          filter(Entity %in% correct_words_to_keep |
                   highlight == 'Likely Incorrect')
      } else if ("drop" %in% tolower(incorrect_words_to_keep)) {
        tokenized_data <- tokenized_data %>%
          filter(highlight != 'Likely Correct')
        correct_words_to_keep <- c()
      } else {
        tokenized_data <- tokenized_data
        correct_words_to_keep <- c()
      }
      
      # Display categorized words
      cat('Words categorized as Likely Incorrect:\n')
      print(tokenized_data %>% filter(highlight == "Likely Incorrect"), n = Inf)
      
      cat('\nSelect the words to keep from the Likely Incorrect category (comma separated), or type "keep" to keep all or "drop" to drop all:\n')
      incorrect_words_to_keep <- unlist(strsplit(readline(prompt = 'Words to keep: '), ','))
      incorrect_words_to_keep <- trimws(incorrect_words_to_keep)
      
      if (!("keep" %in% tolower(incorrect_words_to_keep)) &&
          !("drop" %in% tolower(incorrect_words_to_keep))) {
        incorrect_words_to_keep <- unique(incorrect_words_to_keep)
        tokenized_data <- tokenized_data %>%
          filter(Entity %in% c(incorrect_words_to_keep) |
                   highlight == 'Likely Correct')
      } else if ("drop" %in% tolower(incorrect_words_to_keep)) {
        tokenized_data <- tokenized_data %>%
          filter(highlight != 'Likely Incorrect')
        incorrect_words_to_keep <- c()
      } else {
        tokenized_data <- tokenized_data
        incorrect_words_to_keep <- c()
      }
      
      cat('\014')
      cat('\nSelected words so far:\n')
      print(unique(as.character(tokenized_data$Entity)))
      
      # Allow user to add any additional words
      cat('\nEnter any additional valid words (comma separated) or type "skip" to skip:\n')
      additional_words <- unlist(strsplit(readline(prompt = 'Additional words: '), ','))
      additional_words <- trimws(additional_words)
      
      if (!("skip" %in% tolower(additional_words))) {
        additional_words <- unique(additional_words)
        additional_data <- data.frame(Entity = additional_words, 
                                      count = rep(0, length(additional_words)), 
                                      highlight = "User Added", 
                                      Label = label)
        tokenized_data <- bind_rows(tokenized_data, additional_data) %>%
          arrange(desc(count))
      } else {
        tokenized_data <- tokenized_data
        additional_words <- c()
      }
      
      # Combine and filter results based on user input
      final_words <- c(correct_words_to_keep, 
                       incorrect_words_to_keep, 
                       additional_words)
      if(length(final_words) > 0) {
        final_tokenized_data <- tokenized_data %>%
          filter(Entity %in% final_words) %>%
          arrange(desc(count))
      } else {
        final_tokenized_data <- tokenized_data
      }
      
      # Ensure the Label column is correctly assigned
      if (any(is.na(final_tokenized_data$Label))) {
        final_tokenized_data <- final_tokenized_data %>%
          mutate(Label = ifelse(is.na(Label), label, Label))
      }
      
      cat('Final list of words for label', label, ':\n')
      print(final_tokenized_data, n = Inf)
      
      # Confirm final selection
      confirm <- tolower(readline(prompt = 'Confirm this final list of words (yes/no)? '))
      
      if (confirm %in% c('yes', 'y')) {
        # Append the result for this label
        result <- bind_rows(result, final_tokenized_data %>% mutate(Label = label))
        break  # Exit the repeat loop if selection is confirmed
      } else {
        cat('Revising the selection process for label', label, '\n')
        # Repeat the process for this label
      }
    }
  }
  
  return(result)
}

## Function call
find_closest_match <- function(value, label, pre_dictionary) {
  # Subset the pre_dictionary based on label
  subset_gs <- pre_dictionary %>% filter(Label == label)

  # Compute Jaccard distances
  distances <- stringdist::stringdist(value, subset_gs$Entity, method = 'jaccard')

  # Find the index of the minimum distance
  closest_index <- which.min(distances)

  # Return the closest match value
  return(subset_gs$Entity[closest_index])
}

# Numeric value domain knowledge
domain_knowledge_min_max <- function(data) {
  
  # Helper function to get user input for a variable
  get_user_input <- function(var_name, current_min, current_max) {
    cat('Current min value for', var_name, 'is:', current_min, '\n')
    cat('Current max value for', var_name, 'is:', current_max, '\n')
    min_val <- readline(prompt = paste('Enter the minimum value for', var_name, ': '))
    max_val <- readline(prompt = paste('Enter the maximum value for', var_name, ': '))
    return(paste0(min_val, ',', max_val))
  }
  
  # Initialize an empty data frame to store the results
  results_df <- data.frame(
    Label = character(),
    Entity = character(),
    Type = character(),
    stringsAsFactors = FALSE
  )
  
  numvars = unique(data$Label[which(data$Type == 'numeric')])
  # Process numeric variables
  for (var in numvars) {
    current_min <- min(data[[var]], na.rm = TRUE)
    current_max <- max(data[[var]], na.rm = TRUE)
    user_input <- get_user_input(var, current_min, current_max)
    results_df <- rbind(results_df, data.frame(Label = var, 
                                               Entity = user_input, 
                                               Type = 'numeric', 
                                               stringsAsFactors = FALSE))
  }
  datevars = unique(data$Label[which(data$Type == 'date')])
  # Process date variables
  for (var in datevars) {
    current_min <- min(as.character(data[[var]]), na.rm = TRUE)
    current_max <- max(as.character(data[[var]]), na.rm = TRUE)
    user_input <- get_user_input(var, current_min, current_max)
    results_df <- rbind(results_df, data.frame(Label = var, 
                                               Entity = user_input, 
                                               Type = 'date', 
                                               stringsAsFactors = FALSE))
  }
  
  return(results_df)
}

# hp_num_dictionary <- domain_knowledge_min_max(training_data_balanced_ndk)

# Call the function and store the result in what will become a preliminary dictionary with domain knowledge
# hp_text_dictionary <- inspect_and_inject_domain_knowledge(training_data_balanced_ndk)
# 
# hp_text_dictionary <- hp_text_dictionary %>%
#    select(-count, -highlight) %>%
#   mutate(
#     Class = training_data_balanced_ndk$Class[match(Label, training_data_balanced_ndk$Label)],
#     Class_Identifier = paste0(Class, ':', Label, ':', Entity),
#     Identifier = paste0(Label, ':', Entity),
#     Type = training_data_balanced_ndk$Type[match(Label, training_data_balanced_ndk$Label)]
#   )
# print('Write Domain Knowledge-injected pre-dictionary for use during Triangulation.')
# hp_pre_dictionary <- rbind(hp_text_dictionary, hp_num_dictionary)
# write.csv(hp_pre_dictionary, 'hp_pre_dictionary.csv', row.names = FALSE)
# Calling pre-dictionary
hp_pre_dictionary <- read.csv(paste0(getwd(), '/hp_pre_dictionary.csv'),
                              header = TRUE, stringsAsFactors = FALSE)
hp_num <- filter(hp_pre_dictionary, Type != 'text')
hp_txt <- filter(hp_pre_dictionary, Type == 'text') %>%
  filter(Entity %in% training_data_balanced$Entity)
hp_pre_dictionary <- rbind(hp_num, hp_txt) %>%
  mutate(Class = gold_standard_dictionary$Class[match(Label, gold_standard_dictionary$Label)])
  
D0 <- hp_pre_dictionary 
write.csv(D0, paste0(getwd(), '/D0_', ii, '.csv'), row.names = FALSE)
################################################################################
print('Process 6: Use training data dictionary to train CRF model.')
print('Hyper-parameter tuning is performed using Grid-Search.')
################################################################################
## Conditional Random Fields model
train_and_evaluate_crf <- function(train_data, 
                                   val_data, 
                                   gold_standard, 
                                   algorithm, 
                                   max_iterations) {
  
  # Start time
  start_time <- Sys.time()  
  set.seed(54689)
  model <- crf(
    x = train_data[, names(train_data) %in% c('Entity', 'tfidf', 'Label')], 
    y = train_data$Class,
    group = as.integer(factor(train_data$Class)),
    method = algorithm,
    options = list(max_iterations = max_iterations, feature.minfreq = 5)
  )
  
  validation_pred <- predict(
    model,
    val_data[, names(val_data) %in% c('Entity', 'tfidf', 'Label')],
    group = as.integer(factor(val_data$Class)))
  
  validation_pred_df <- data.frame(Label = val_data$Label, 
                                   Entity = val_data$Entity, 
                                   Class = val_data$Class,
                                   Predicted_Label = validation_pred$label) %>%
    mutate(Predicted_Class_Identifier = 
             paste0(Predicted_Label, ':', Label, ':', Entity), 
           Class_Identifier = paste0(Class, ':', Label, ':', Entity),
           Gold_Class_Identifier = 
             gold_standard$Class_Identifier[match(Class_Identifier,
                                                  gold_standard$Class_Identifier)])
  
  conf_matrix <- confusionMatrix(factor(validation_pred_df$Predicted_Label), 
                                 factor(validation_pred_df$Class))
  
  label_metrics <- as.data.frame(conf_matrix$byClass) %>%
    mutate(Class = rownames(conf_matrix$byClass),
           Class = substr(Class, 8, nchar(Class)))
  
  
  # End time
  end_time <- Sys.time()  
  label_metrics$algorithm <- algorithm
  label_metrics$max_iterations <- max_iterations
  
  overall_accuracy <- data.frame(Accuracy=conf_matrix$overall['Accuracy'])
  label_metrics$oAccuracy <- overall_accuracy$Accuracy
  
  gold_matrix <- validation_pred_df %>%
    mutate(
      Gold_Class_Identifier = ifelse(is.na(Gold_Class_Identifier), '', 
                                     Gold_Class_Identifier),
      TP = ifelse(Predicted_Label == Class & 
                    Predicted_Class_Identifier == Gold_Class_Identifier, 1, 0),
      TN = ifelse(Predicted_Class_Identifier != Gold_Class_Identifier &
                    Class != Predicted_Label &
                    !Gold_Class_Identifier %in% gold_standard$Class_Identifier, 1, 0),
      FP = ifelse(Predicted_Label == Class & 
                    Gold_Class_Identifier != Predicted_Class_Identifier, 1, 0),
      FN = ifelse(Predicted_Label != Class & 
                    Gold_Class_Identifier %in% gold_standard$Class_Identifier &
                    Predicted_Class_Identifier != Gold_Class_Identifier,
                  1, 0)
    ) %>%
    group_by(Class) %>%
    summarise(
      G_Accuracy = sum(TP + TN) / sum(TP + TN + FP + FN),
      G_Precision = sum(TP) / sum(TP + FP),
      G_Recall = sum(TP) / sum(TP + FN),
      G_F1 = ifelse(G_Precision + G_Recall == 0, 0, 
                  2 * G_Precision * G_Recall / (G_Precision + G_Recall))
    ) %>%
    left_join(label_metrics %>% 
                dplyr::select(Class, Precision, Recall, F1, oAccuracy,
                              algorithm, max_iterations),
              by = 'Class')
  
  # Return both per label and overall metrics
  return(gold_matrix)
}

## Function call
## k-fold cross-validation
cross_validation_crf <- function(data, 
                                 k, 
                                 gold_standard, 
                                 hyperparameters) {
  data <- filter(data, Type == 'text')
################################################################################
  ## Inject Domain Knowledge to training set
  # Apply the function to each row in the original dataset
  data$Corrected_Entity <- mapply(
    find_closest_match,
    data$Entity,
    data$Label,
    MoreArgs = list(pre_dictionary = hp_pre_dictionary))

  data <- data %>%
    mutate(
      Entity = ifelse(!Label %in% c('RENT_OWN_OTH', 'OTHER_INSECTICIDE_SPEC'),
                      Corrected_Entity, Entity),
      Identifier = paste0(Label, ':', Entity)
    ) %>%
    select(-Corrected_Entity)
################################################################################
  
  folds <- createFolds(data$Label, k = k)
  results <- list()
  
  for (i in 1:k) {
    fold <- folds[[i]]
    train_data <- data[-fold, ]
    val_data <- data[fold, ]
    
    for (j in 1:nrow(hyperparameters)) {
      parameters <- hyperparameters[j, ]
      
      metrics <- train_and_evaluate_crf(
        train_data = train_data,
        val_data = val_data,
        gold_standard = gold_standard,
        algorithm = as.character(parameters$algorithm),
        max_iterations = as.numeric(parameters$max_iterations))
      
      # Store metrics and elapsed time
      metrics$fold <- i
      results[[paste(i, j, sep = '_')]] <- metrics
    }
  }
  # Combine and aggregate label metrics across all folds
  all_label_metrics <- bind_rows(results)
  
  # Return aggregated metrics and mean elapsed time
  return(all_label_metrics)
}

# Grid Search Hyper-parameter tuning
hyperparameters <- expand.grid(
  algorithm = c('lbfgs', 
                'l2sgd', 
                'averaged-perceptron', 
                'passive-aggressive', 
                'arow'),
  max_iterations = c(5, 10, 50, 100))

print('Hyper-parameter tuning in progress...')
results <- cross_validation_crf(training_data_balanced, 
                                k = 5, 
                                gold_standard = gold_standard_dictionary, 
                                hyperparameters = hyperparameters)

results_per_class_edk <- results %>%
  group_by(Class, algorithm, max_iterations) %>%
  summarise(.groups = 'drop',
            G_Accuracy = mean(G_Accuracy, na.rm = TRUE),
            G_Precision = mean(G_Precision, na.rm = TRUE),
            G_Recall = mean(G_Recall, na.rm = TRUE),
            G_F1 = mean(G_F1, na.rm = TRUE),
            Accuracy = mean(oAccuracy, na.rm = TRUE),
            Precision = mean(Precision, na.rm = TRUE),
            Recall = mean(Recall, na.rm = TRUE),
            F1 = mean(F1, na.rm = TRUE),
            #time = mean(elapsed_time, na.rm = TRUE)
  )
results_overall_edk <- results %>%
  group_by(algorithm, max_iterations) %>%
  summarise(.groups = 'drop',
            G_Accuracy = mean(G_Accuracy, na.rm = TRUE),
            G_Precision = mean(G_Precision, na.rm = TRUE),
            G_Recall = mean(G_Recall, na.rm = TRUE),
            G_F1 = mean(G_F1, na.rm = TRUE),
            Accuracy = mean(oAccuracy, na.rm = TRUE),
            Precision = mean(Precision, na.rm = TRUE),
            Recall = mean(Recall, na.rm = TRUE),
            F1 = mean(F1, na.rm = TRUE),
            #time = mean(elapsed_time, na.rm = TRUE)
  )

print('Saving training performance metrics...')
# Set path to preferred folder
path = # your/path/here
write.csv(results_per_class_edk, 
          paste0(path, '/crf_hp_class_lv_metrics_edk - data ', ii, '.csv'),
          row.names = FALSE)
write.csv(results_overall_edk, 
          paste0(path, '/crf_hp_overall_metrics_edk - data ', ii, '.csv'),
          row.names = FALSE)
