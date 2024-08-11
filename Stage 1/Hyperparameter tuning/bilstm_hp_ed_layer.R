################################################################################
print('Starting BILSTM ED-Layer for BiLSTM algorithm.')
################################################################################
print('Setting Python Resiculate environment...')
################################################################################
# For Python Reticulate, create a python environment using anaconda (CMD.exe) with the following code:
## conda create -n r-tensorflow python=3.8
## conda activate r-tensorflow
## pip install tensorflow keras
# In your R script or R console, set the RETICULATE_PYTHON environment variable
Sys.setenv(RETICULATE_PYTHON = "path/to/your/anaconda3/envs/r-tensorflow/python.exe")
use_condaenv("path/to/your/anaconda3/envs/r-tensorflow/python.exe")
################################################################################
## Functions call
## Bi-LSTM CRF Model
train_and_evaluate_bilstm_crf <- function(train_data, 
                                          val_data, 
                                          gold_standard, 
                                          max_epochs, 
                                          lstm_units, 
                                          batch_size) {
  
  # Start time
  start_time <- Sys.time()  
  set.seed(54689)
  
  # Prepare input data for LSTM
  
  entity_levels <- unique(train_data$Entity)
  train_data$Entity_encoded <- as.integer(factor(train_data$Entity, 
                                                 levels = entity_levels))
  
  # Combine Entity and TF-IDF scores into a feature matrix
  train_data$combined_features <- train_data$Entity_encoded * train_data$tfidf
  
  # Val data
  entity_vlevels <- unique(val_data$Entity)
  val_data$Entity_encoded <- as.integer(factor(val_data$Entity, 
                                               levels = entity_vlevels))
  
  # Combine Entity and TF-Itrain_data scores into a feature matrix
  val_data$combined_features <- val_data$Entity_encoded * val_data$tfidf
  ###
  
  # Extract relevant columns for training
  X_train <- as.matrix(train_data$combined_features)
  X_val <- as.matrix(val_data$combined_features)
  
  # Reshape the data to 3D tensor for LSTM
  X_train <- array_reshape(X_train, c(nrow(train_data), 1, 1))
  X_val <- array_reshape(X_val, c(nrow(val_data), 1, 1))
  
  # Convert Class labels to one-hot encoded format
  num_classes <- length(unique(train_data$Class))
  y_train <- to_categorical(as.integer(factor(train_data$Class)) - 1, 
                            num_classes = num_classes)
  y_val <- to_categorical(as.integer(factor(val_data$Class)) - 1,
                          num_classes = num_classes)
  
  y_train <- array_reshape(y_train, c(nrow(train_data), 1, num_classes))
  y_val <- array_reshape(y_val, c(nrow(val_data), 1, num_classes))
  
  input <- layer_input(shape = c(1, 1))
  bi_lstm <- bidirectional(input, layer_lstm(units = lstm_units, 
                                             return_sequences = TRUE))
  output <- layer_dense(units = num_classes, activation = "softmax")(bi_lstm)
  
  model <- keras_model(inputs = input, outputs = output)
  model %>% compile(
    optimizer = optimizer_adam(),
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )
    
  history <- model %>% fit(
    X_train, y_train,
    epochs = max_epochs, 
    batch_size = batch_size, 
    validation_data = list(X_val, y_val),
    callbacks = list(
      callback_early_stopping(monitor = "val_loss", patience = 3, restore_best_weights = TRUE)
    )
  )
    
  predictions <- model %>% predict(X_val)
  
  # Convert predictions to class labels
  predicted_classes <- apply(predictions, 1, function(x) which.max(x) - 1)
  
  # Convert one-hot encoded true labels back to class labels
  true_classes <- apply(y_val, 1, function(x) which.max(x) - 1)
  
  validation_pred_df <- data.frame(
    Label = val_data$Label,
    Entity = val_data$Entity,
    Class = val_data$Class,
    Predicted_Label = factor(predicted_classes, 
                             levels = 0:(num_classes - 1), 
                             labels = levels(factor(val_data$Class)))
  ) %>%
    mutate(Predicted_Class_Identifier = paste0(Predicted_Label, ':', Label, ':', Entity), 
           Class_Identifier = paste0(Class, ':', Label, ':', Entity),
           Gold_Class_Identifier = 
             gold_standard$Class_Identifier[match(Class_Identifier, gold_standard$Class_Identifier)])
  
  conf_matrix <- confusionMatrix(factor(validation_pred_df$Predicted_Label), 
                                 factor(validation_pred_df$Class))
  
  label_metrics <- as.data.frame(conf_matrix$byClass) %>%
    mutate(Class = rownames(conf_matrix$byClass),
           Class = substr(Class, 8, nchar(Class)))
  
  
  # End time
  end_time <- Sys.time()  
  # label_metrics$elapsed_time <- as.numeric(difftime(end_time, 
  #                                                   start_time, units = 'mins'))  
  label_metrics$max_epochs <- max_epochs
  label_metrics$lstm_units <- lstm_units
  label_metrics$batch_size <- batch_size
  
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
                              lstm_units, batch_size, max_epochs),
              by = 'Class')
  # Return both per label and overall metrics
  return(gold_matrix)
}

## Function call
## k-fold cross-validation
cross_validation_bilstm_crf <- function(data, k, gold_standard, hyperparameters) {
  data <- filter(data, Type == 'text')
  
  folds <- createFolds(data$Label, k = k)
  results <- list()
  
  for (i in 1:k) {
    fold <- folds[[i]]
    train_data <- data[-fold, ]
    val_data <- data[fold, ]
    
    for (j in 1:nrow(hyperparameters)) {
      parameters <- hyperparameters[j, ]
      
      metrics <- train_and_evaluate_bilstm_crf(
        train_data = train_data,
        val_data = val_data,
        gold_standard = gold_standard,
        max_epochs = as.numeric(parameters$max_epochs),
        lstm_units = as.numeric(parameters$lstm_units),
        batch_size = as.numeric(parameters$batch_size)
      )
      
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
  max_epochs = c(5, 10, 50),
  lstm_units = c(32, 64),
  batch_size = c(16, 32, 64)
)

print('Hyper-parameter tuning in progress...')
results <- cross_validation_bilstm_crf(training_data_balanced, 
                                       k = 5, 
                                       gold_standard = gold_standard_dictionary, 
                                       hyperparameters = hyperparameters)

results_per_class_ed <- results %>%
  group_by(Class, max_epochs, lstm_units, batch_size) %>%
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
  group_by(max_epochs, lstm_units, batch_size) %>%
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
# Set path to prefered folder
path = # set/path/here
write.csv(results_per_class_ed, 
          paste0(path, '/bilstm_hp_class_lv_metrics_ed - data ', ii, '.csv'),
          row.names = FALSE)
write.csv(results_overall_ed, 
          paste0(path, '/bilstm_hp_overall_metrics_ed - data ', ii, '.csv'),
          row.names = FALSE)
