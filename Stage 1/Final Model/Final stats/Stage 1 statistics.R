## This is the ccomparison of the metrics between both layers for each dataset.
library(tidyverse)
library(xlsx)
path = 'your/path/here'
gold_standard <- read.csv(paste0(path, '/gold_standard_dictionary.csv'),
                          header = TRUE, 
                          stringsAsFactors = FALSE) %>%
  mutate(
    Class_Identifier = paste0(Class, ':', Label, ':', Entity),
    Identifier = paste0(Label, ':', Entity)
  )

path = "your/path/here"
setwd(path)

## ED-Layer results
metrics_ed1 <- read.csv(paste0(getwd(), '/crf_all_results_ed - data 1.csv'),
                             header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(dataset = 1, layer = 'ed')
metrics_ed2 <-  read.csv(paste0(getwd(), '/crf_all_results_ed - data 2.csv'),
                         header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(dataset = 2, layer = 'ed')
metrics_ed3 <-  read.csv(paste0(getwd(), '/crf_all_results_ed - data 3.csv'),
                         header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(dataset = 3, layer = 'ed')
metrics_ed4 <-  read.csv(paste0(getwd(), '/crf_all_results_ed - data 4.csv'),
                         header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(dataset = 4, layer = 'ed')
metrics_ed5 <-  read.csv(paste0(getwd(), '/crf_all_results_ed - data 5.csv'),
                         header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(dataset = 5, layer = 'ed')

## EDK-Layer results
metrics_edk1 <-  read.csv(paste0(getwd(), '/crf_all_results_edk - data 1.csv'),
                         header = TRUE, stringsAsFactors = FALSE)
metrics_edk2 <-  read.csv(paste0(getwd(), '/crf_all_results_edk - data 2.csv'),
                         header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(dataset = 2, layer = 'edk') 

metrics_edk3 <-  read.csv(paste0(getwd(), '/crf_all_results_edk - data 3.csv'),
                         header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(dataset = 3, layer = 'edk')
metrics_edk4 <-  read.csv(paste0(getwd(), '/crf_all_results_edk - data 4.csv'),
                         header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(dataset = 4, layer = 'edk')
metrics_edk5 <-  read.csv(paste0(getwd(), '/crf_all_results_edk - data 5.csv'),
                         header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(dataset = 5, layer = 'edk')


metrics_comparison <- function(metrics_layer_ed, metrics_layer_edk) {
  m_ed1=metrics_layer_ed
  m_edk1=metrics_layer_edk

  m_ed = select(m_ed1, Class, TP, TN, FP, FN)
  m_edk = select(m_edk1, TP, TN, FP, FN)
  colnames(m_edk) <- c('TPk', 'TNk', 'FPk', 'FNk')
  
  merge = cbind(m_ed, m_edk)
  
  class_contingency <- merge %>%
    group_by(Class) %>%
    summarise(.groups = 'drop',
              # For Accuracy
              both_correct = sum((TN == 1 & TNk == 1) | 
                                 (TP == 1 & TPk == 1)),
              # Model 1 correct, Model 2 incorrect
              ed_correct_edk_incorrect = sum((TN == 1 & TNk == 0) | 
                                             (TP == 1 & TPk == 0)),
              
              # Model 1 incorrect, Model 2 correct
              ed_incorrect_edk_correct = sum((TN == 0 & TNk == 1) | 
                                             (TP == 0 & TPk == 1)),
              
              # Both models incorrect (either FP or FN)
              both_incorrect = sum((FP == 1 & FPk == 1) | 
                                   (FN == 1 & FNk == 1)),
              
              N = sum((TN == 1 & TNk == 1) | (TP == 1 & TPk == 1)) + 
                  sum((TN == 1 & TNk == 0) | (TP == 1 & TPk == 0)) + 
                  sum((TN == 0 & TNk == 1) | (TP == 0 & TPk == 1)) + 
                  sum((FP == 1 & FPk == 1) | (FN == 1 & FNk == 1)),
              
              # Precision
              both_correct_tp = sum(TP == 1 & TPk == 1),
              ed_correct_tp_edk_incorrect_fp = sum(TP == 1 & FPk == 1),
              ed_incorrect_fp_edk_correct_tp = sum(FP == 1 & TPk == 1),
              both_incorrect_fp = sum(FP == 1 & FPk == 1),
              TP_ed = sum(TP),
              FP_ed = sum(FP),
              
              TP_edk = sum(TPk),
              FP_edk = sum(FPk),
              
              # Recall
              ed_correct_tp_edk_incorrect_fn = sum(TP == 1 & FNk == 1),
              ed_incorrect_fn_edk_correct_tp = sum(FN == 1 & TPk == 1),
              both_incorrect_fn = sum(FN == 1 & FNk == 1),
              FN_ed = sum(FN),
              FN_edk = sum(FNk)
              ) %>% ungroup()
  
  calculate_metrics <- function(metrics) {
    metrics %>%
      group_by(Class) %>%
      summarise(
        Accuracy_ED = ((both_correct + ed_correct_edk_incorrect) / N)*100,
        Accuracy_EDK = ((both_correct + ed_incorrect_edk_correct) / N)*100,
        Precision_ED = (TP_ed / (TP_ed + FP_ed))*100,
        Precision_EDK = (TP_edk / (TP_edk + FP_edk))*100,
        Recall_ED = (TP_ed / (TP_ed + FN_ed))*100,
        Recall_EDK = (TP_edk / (TP_edk + FN_edk))*100,
        
        F1_ED = ifelse(Precision_ED + Recall_ED == 0, 0,
                       (2 * Precision_ED * Recall_ED / (Precision_ED + Recall_ED))),
        F1_EDK = ifelse(Precision_EDK + Recall_EDK == 0, 0,
                        (2 * Precision_EDK * Recall_EDK / (Precision_EDK + Recall_EDK)))
      ) 
  }
  
  calculate_metrics_all <- function(metrics) {
    metrics %>%
      janitor::adorn_totals('row') %>%
      filter(Class == 'Total') %>%
      reframe(
        Accuracy_ED = ((both_correct + ed_correct_edk_incorrect) / N)*100,
        Accuracy_EDK = ((both_correct + ed_incorrect_edk_correct) / N)*100,
        Precision_ED = (TP_ed / (TP_ed + FP_ed))*100,
        Precision_EDK = (TP_edk / (TP_edk + FP_edk))*100,
        Recall_ED = (TP_ed / (TP_ed + FN_ed))*100,
        Recall_EDK = (TP_edk / (TP_edk + FN_edk))*100,
        
        F1_ED = ifelse(Precision_ED + Recall_ED == 0, 0,
                    (2 * Precision_ED * Recall_ED / (Precision_ED + Recall_ED))),
        F1_EDK = ifelse(Precision_EDK + Recall_EDK == 0, 0,
                       (2 * Precision_EDK * Recall_EDK / (Precision_EDK + Recall_EDK)))
      ) 
  }
  
  # Calculate metrics for each dataset
  class_metrics <- calculate_metrics(class_contingency)
  class_metrics[is.na(class_metrics)] <- 0
  all_metrics <- calculate_metrics_all(class_contingency) %>%
    mutate(Class = 'All') %>% select(Class, everything())
  all_metrics[is.na(all_metrics)] <- 0
  
  metrics <- rbind(all_metrics, class_metrics)
  
  
  
  ## McNemar Tests
  ## Accuracy
  class_accuracy <- list()
  for(i in unique(class_contingency$Class)) {
    # Create the contingency table for accuracy
    df <- subset(class_contingency, Class == i)
    dft <- class_contingency %>%
      janitor::adorn_totals('row') %>%
      filter(Class == 'Total')
    
    # Accuracy contingency table
    accuracy <- matrix(c(
      df$both_correct, 
      df$ed_correct_edk_incorrect, 
      df$ed_incorrect_edk_correct, 
      df$both_incorrect
    ), nrow = 2, byrow = TRUE,
    dimnames = list("nDK Model" = c("Correct", "Incorrect"),
                    "iDK Model" = c("Correct", "Incorrect")))
    
    
    # McNemar test for accuracy
    mcnemar_accuracy <- mcnemar.test(accuracy)
    results_accuracy <- data.frame(Class = i,
                          Statistic = mcnemar_accuracy$statistic[[1]],
                          df = mcnemar_accuracy$parameter[[1]],
                          p = mcnemar_accuracy$p.value[[1]])
    
    accuracyt <- matrix(c(
      dft$both_correct, 
      dft$ed_correct_edk_incorrect, 
      dft$ed_incorrect_edk_correct, 
      dft$both_incorrect
    ), nrow = 2, byrow = TRUE,
    dimnames = list("nDK Model" = c("Correct", "Incorrect"),
                    "iDK Model" = c("Correct", "Incorrect")))
    
    
    # McNemar test for accuracy
    mcnemar_accuracyt <- mcnemar.test(accuracyt)
    results_accuracyt <- data.frame(Class = 'All',
                                   Statistic = mcnemar_accuracyt$statistic[[1]],
                                   df = mcnemar_accuracyt$parameter[[1]],
                                   p = mcnemar_accuracyt$p.value[[1]])
    
    results_accuracy_all <- rbind(results_accuracyt, results_accuracy)
    
    class_accuracy[[i]] <- results_accuracy_all
  }
  
  ## Precision
  class_precision <- list()
  for(i in unique(class_contingency$Class)) {
    df <- subset(class_contingency, Class == i)
    
    # Precision contingency table
    precision <- matrix(c(
      df$both_correct_tp,
      df$ed_correct_tp_edk_incorrect_fp,
      df$ed_incorrect_fp_edk_correct_tp,
      df$both_incorrect_fp
    ), nrow = 2, byrow = TRUE,
    dimnames = list("nDK Model" = c("Correct (TP)", "Incorrect (FP)"),
                    "iDK Model" = c("Correct (TP)", "Incorrect (FP)")))
    
    # McNemar test for precision
    mcnemar_precision <- mcnemar.test(precision)
    results_precision <- data.frame(Class = i,
                          Statistic = mcnemar_precision$statistic[[1]],
                          df = mcnemar_precision$parameter[[1]],
                          p = mcnemar_precision$p.value[[1]])
    dft <- class_contingency %>%
      janitor::adorn_totals('row') %>%
      filter(Class == 'Total')
    precisiont <- matrix(c(
      dft$both_correct_tp,
      dft$ed_correct_tp_edk_incorrect_fp,
      dft$ed_incorrect_fp_edk_correct_tp,
      dft$both_incorrect_fp
    ), nrow = 2, byrow = TRUE,
    dimnames = list("nDK Model" = c("Correct (TP)", "Incorrect (FP)"),
                    "iDK Model" = c("Correct (TP)", "Incorrect (FP)")))
    
    # McNemar test for precision
    mcnemar_precisiont <- mcnemar.test(precisiont)
    results_precisiont <- data.frame(Class = 'All',
                                    Statistic = mcnemar_precisiont$statistic[[1]],
                                    df = mcnemar_precisiont$parameter[[1]],
                                    p = mcnemar_precisiont$p.value[[1]])
    results_precision_all <- rbind(results_precisiont, results_precision)
    
    class_precision[[i]] <- results_precision_all
  }
  
  class_recall <- list()
  for(i in unique(class_contingency$Class)) {
    df <- subset(class_contingency, Class == i)
    dft <- class_contingency %>%
      janitor::adorn_totals('row') %>%
      filter(Class == 'Total')
   
    # Recall contingency table
    recall <- matrix(c(
      df$both_correct_tp,
      df$ed_correct_tp_edk_incorrect_fn,
      df$ed_incorrect_fn_edk_correct_tp,
      df$both_incorrect_fn
    ), nrow = 2, byrow = TRUE,
    dimnames = list("nDK Model" = c("Correct (TP)", "Incorrect (FN)"),
                    "iDK Model" = c("Correct (TP)", "Incorrect (FN)")))
    
    # Perform the McNemar test for recall
    mcnemar_recall <- mcnemar.test(recall)
    results_recall <- data.frame(Class = i,
                          Statistic = mcnemar_recall$statistic[[1]],
                          df = mcnemar_recall$parameter[[1]],
                          p = mcnemar_recall$p.value[[1]])
    
    recallt <- matrix(c(
      dft$both_correct_tp,
      dft$ed_correct_tp_edk_incorrect_fn,
      dft$ed_incorrect_fn_edk_correct_tp,
      dft$both_incorrect_fn
    ), nrow = 2, byrow = TRUE,
    dimnames = list("nDK Model" = c("Correct (TP)", "Incorrect (FN)"),
                    "iDK Model" = c("Correct (TP)", "Incorrect (FN)")))
    
    # Perform the McNemar test for recall
    mcnemar_recallt <- mcnemar.test(recallt)
    results_recallt <- data.frame(Class = 'All',
                                 Statistic = mcnemar_recallt$statistic[[1]],
                                 df = mcnemar_recallt$parameter[[1]],
                                 p = mcnemar_recallt$p.value[[1]])
    results_recall_all <- rbind(results_recallt, results_recall)
    
    class_recall[[i]] <- results_recall_all
  }
  
  
  class_f1 <- list()
  for(i in unique(metrics$Class)) {
    
    df=metrics
    f1_scores_modelnDK <- df$F1_ED  
    f1_scores_modeliDK <- df$F1_EDK
    
    
    # Paired t-test
    ttest_results <- t.test(f1_scores_modelnDK, f1_scores_modeliDK, paired = TRUE)
    
    results_f1 <- data.frame(Class = i,
                          Statistic = ttest_results$statistic[[1]],
                          df = ttest_results$parameter[[1]],
                          p = ttest_results$p.value[[1]])
    
    
    class_f1[[i]] <- results_f1
  }
  
  binded_accuracy <- bind_rows(class_accuracy) %>%
    mutate(Measure = 'Accuracy')
  binded_precision <- bind_rows(class_precision) %>%
    mutate(Measure = 'Precision')
  binded_recall <- bind_rows(class_recall) %>%
    mutate(Measure = 'Recall')
  
  binded_f1 <- bind_rows(class_f1) %>%
    mutate(Measure = 'F1')
  class_metrics_bind <- bind_rows(binded_accuracy, binded_precision, 
                                  binded_recall, binded_f1)
  
  
  return(list('metrics'=class_metrics_bind, 'measures'=metrics))
  }
  
# Create workbook and sheets
library(openxlsx)

metrics_ed_list <- list(metrics_ed1, metrics_ed2, metrics_ed3, metrics_ed4, metrics_ed5)
metrics_edk_list <- list(metrics_edk1, metrics_edk2, metrics_edk3, metrics_edk4, metrics_edk5)

metrics_per_dataset <- list()
measures_per_dataset <- list()

for (i in seq_along(metrics_ed_list)) {
  results <- metrics_comparison(metrics_ed_list[[i]], metrics_edk_list[[i]])
  results$metrics$dataset = i
  results$measures$dataset = i
  
  metrics_per_dataset[[i]] <- results$metrics
  measures_per_dataset[[i]] <- results$measures
  
}

binded_metrics <- bind_rows(metrics_per_dataset)
binded_metrics <- binded_metrics[!duplicated(binded_metrics), ]
binded_measures <- bind_rows(measures_per_dataset)

# Save workbook
write.xlsx(binded_metrics, "class_level_comparison_results_final.xlsx")
write.xlsx(binded_measures, "class_level_measures_final.xlsx")
