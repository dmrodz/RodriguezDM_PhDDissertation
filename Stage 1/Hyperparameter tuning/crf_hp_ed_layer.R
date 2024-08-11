################################################################################
print('Starting CRF ED-Layer: No Domain Knowledge injection.')
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
  # Grabbind model metrics (not verified against gold standard)
  overall_accuracy <- data.frame(Accuracy=conf_matrix$overall['Accuracy'])
  label_metrics$oAccuracy <- overall_accuracy$Accuracy
  # Gold standard verification
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
                    Gold_Class_Identifier %in% gold_standard$Class_Identifier,
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

results_per_class_ed <- results %>%
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
results_overall_ed <- results %>%
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
path = # set/path/here
write.csv(results_per_class_ed, 
          paste0(path, '/crf_hp_class_lv_metrics_ed - data ', ii, '.csv'),
          row.names = FALSE)
write.csv(results_overall_ed, 
          paste0(path, '/crf_hp_overall_metrics_ed - data ', ii, '.csv'),
          row.names = FALSE)
