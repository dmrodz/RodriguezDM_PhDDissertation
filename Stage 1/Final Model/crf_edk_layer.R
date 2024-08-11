print('Final CRF model run - Domain Knowledge')
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


### CRF FINAL Run
print('CRF - Domain Knowledge')
## Conditional Random Fields model
train_edk <- training_data_balanced
test_edk <- compute_tfidf_features_validation(labeled_test_data,
                                              training_data_balanced)
remove(training_data_balanced)
train_edk$Corrected_Entity <- mapply(
  find_closest_match,
  train_edk$Entity,
  train_edk$Label,
  MoreArgs = list(pre_dictionary = D0))

train_edk <- train_edk %>%
  mutate(
    Entity = ifelse(!Label %in% c('RENT_OWN_OTH', 'OTHER_INSECTICIDE_SPEC'),
                    Corrected_Entity, Entity),
    Identifier = paste0(Label, ':', Entity)
  ) %>%
  select(-Corrected_Entity)

test_edk$Corrected_Entity <- mapply(
  find_closest_match,
  test_edk$Entity,
  test_edk$Label,
  MoreArgs = list(pre_dictionary = D0))

test_edk <- test_edk %>%
  mutate(
    Entity = ifelse(!Label %in% c('RENT_OWN_OTH', 'OTHER_INSECTICIDE_SPEC'),
                    Corrected_Entity, Entity),
    Identifier = paste0(Label, ':', Entity)
  ) %>%
  select(-Corrected_Entity)
################################################################################

params <- filter(hyperparameters, dataset == ii &
                   model == 'crf' &
                   layer == 'edk')
set.seed(54689)
model <- crf(
    x = train_edk[, names(train_edk) %in% c('Entity', 'tfidf', 'Label')],
    y = train_edk$Class,
    group = as.integer(factor(train_edk$Class)),
    method = params$algorithm,
    options = list(max_iterations = params$iterations, feature.minfreq = 5)
)

  validation_pred <- predict(
    model,
    test_edk[, names(test_edk) %in% c('Entity', 'tfidf', 'Label')],
    group = as.integer(factor(test_edk$Class)))

  validation_pred_df <- data.frame(Label = test_edk$Label,
                                   Entity = test_edk$Entity,
                                   Class = test_edk$Class,
                                   Predicted_Label = validation_pred$label) %>%
    mutate(Predicted_Class_Identifier =
             paste0(Predicted_Label, ':', Label, ':', Entity),
           Class_Identifier = paste0(Class, ':', Label, ':', Entity),
           Gold_Class_Identifier =
             gold_standard$Class_Identifier[match(Class_Identifier,
                                                  gold_standard$Class_Identifier)])
  
  remove(train_edk)
  remove(test_edk)

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
                dplyr::select(Class, Precision, Recall, F1, oAccuracy),
              by = 'Class')


final_results_per_class_edk <- results %>%
  group_by(Class) %>%
  summarise(.groups = 'drop',
            G_Accuracy = mean(G_Accuracy, na.rm = TRUE),
            G_Precision = mean(G_Precision, na.rm = TRUE),
            G_Recall = mean(G_Recall, na.rm = TRUE),
            G_F1 = mean(G_F1, na.rm = TRUE),
            Accuracy = mean(oAccuracy, na.rm = TRUE),
            Precision = mean(Precision, na.rm = TRUE),
            Recall = mean(Recall, na.rm = TRUE),
            F1 = mean(F1, na.rm = TRUE)
  )
final_results_overall_edk <- results %>%
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

# Set prefered folder to path
path = 'your/prefered/folder'
write.csv(final_results_per_class_edk,
          paste0(path, '/crf_class_lv_metrics_edk - datab ', ii, '.csv'),
          row.names = FALSE)
write.csv(final_results_overall_edk,
          paste0(path, '/crf_overall_metrics_edk - datab ', ii, '.csv'),
          row.names = FALSE)
all_results_edk <- validation_pred_df %>%
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

write.csv(all_results_edk,
          paste0(path, '/crf_all_results_edk - data ', ii, '.csv'),
          row.names = FALSE)
