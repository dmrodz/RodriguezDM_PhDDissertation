### CRF FINAL Run
print('Final CRF model run - No Domain Knowledge')
## Conditional Random Fields model
# TFIDF based on trained data
compute_tfidf_features_validation <- function(validation_data, train_idf) {
  # Combine all entities for each label into a single document for TF-IDF calculation
  validation_combined <- validation_data %>%
    group_by(Label) %>%
    summarize(CombinedEntity = paste(Entity, collapse = ", ")) %>%
    ungroup()
  
  # Calculate term frequencies (TF) for each label in validation data
  validation_tf <- validation_combined %>%
    rowwise() %>%
    mutate(tf = list(as.data.frame(table(unlist(strsplit(CombinedEntity, ", "))))))
  
  # Un-nest the list column and rename the columns appropriately
  validation_tf <- validation_tf %>%
    unnest(tf) %>%
    rename(Entity = Var1, tf = Freq) %>%
    ungroup()
  
  # Join with training IDF values to calculate TF-IDF
  idfs <- validation_tf %>%
    inner_join(train_idf %>% select(Entity, idf), by = 'Entity') %>%
    mutate(tfidf = tf * idf,
           Identifier = paste0(Label, ':', Entity)) %>%
    select(Label, Identifier, Entity, tfidf)
  
  result <- validation_data %>%
    left_join(idfs, by = c('Label', 'Entity'))
  
  return(result)
}

train_data <- training_data_balanced
val_data <- compute_tfidf_features_validation(labeled_test_data,
                                              training_data_balanced)

params <- filter(hyperparameters, dataset == ii &
                 model == 'crf' &
                 layer == 'ed')

set.seed(54689)
model <- crf(
    x = train_data[, names(train_data) %in% c('Entity', 'tfidf', 'Label')],
    y = train_data$Class,
    group = as.integer(factor(train_data$Class)),
    method = params$algorithm,
    options = list(max_iterations = params$iterations, feature.minfreq = 5)
  )

validation_pred <- predict(
    model,
    val_data[, names(val_data) %in% c('Entity', 'tfidf', 'Label')],
    group = as.integer(factor(val_data$Class)))

validation_pred_df <- data.frame(Label = val_data$Label,
                                 Entity = val_data$Entity,
                                 Class = val_data$Class,
                                 Predicted_Label = validation_pred$label) %>%
  mutate(Predicted_Class_Identifier =  paste0(Predicted_Label, ':', Label, ':', Entity),
         Class_Identifier = paste0(Class, ':', Label, ':', Entity),
         Gold_Class_Identifier = gold_standard$Class_Identifier[match(Class_Identifier,
                                                                      gold_standard$Class_Identifier)])

conf_matrix <- confusionMatrix(factor(validation_pred_df$Predicted_Label),
                               factor(validation_pred_df$Class))
label_metrics <- as.data.frame(conf_matrix$byClass) %>%
    mutate(Class = rownames(conf_matrix$byClass),
           Class = substr(Class, 8, nchar(Class)))

overall_accuracy <- data.frame(Accuracy=conf_matrix$overall['Accuracy'])
label_metrics$oAccuracy <- overall_accuracy$Accuracy

results <- validation_pred_df %>%
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
                dplyr::select(Class, Precision, Recall, F1, oAccuracy),
              by = 'Class')

final_results_per_class_ed <- results %>%
  group_by(Class) %>%
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
final_results_overall_ed <- results %>%
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

all_results_ed <- validation_pred_df %>%
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
  )

# Set path to prefered folder
path = 'your/prefered/folder'
write.csv(final_results_per_class_ed,
          paste0(path, '/crf_class_lv_metrics_ed - datab ', ii, '.csv'),
          row.names = FALSE)
write.csv(final_results_overall_ed,
          paste0(path, '/crf_overall_metrics_ed - datab ', ii, '.csv'),
          row.names = FALSE)
write.csv(all_results_ed,
          paste0(path, '/crf_all_results_ed - data ', ii, '.csv'),
          row.names = FALSE)
