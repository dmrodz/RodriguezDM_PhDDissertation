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
    df <- data.frame(
      Measure = c("TP", "FP", "FN", "TN"),
      ModelED = c(sum(metrics_layer_ed$TP==1), sum(metrics_layer_ed$FP==1), 
                 sum(metrics_layer_ed$FN==1), sum(metrics_layer_ed$TN==1)),
      ModelEDK = c(sum(metrics_layer_edk$TP==1), sum(metrics_layer_edk$FP==1), 
                 sum(metrics_layer_edk$FN==1), sum(metrics_layer_edk$TN==1))
      )
    
    ## Accuracy
    # Create the contingency table for accuracy
    correct_modelED <- df$ModelED[df$Measure == "TP"] + df$ModelED[df$Measure == "TN"]
    incorrect_modelED <- df$ModelED[df$Measure == "FP"] + df$ModelED[df$Measure == "FN"]
    correct_modelEDK <- df$ModelEDK[df$Measure == "TP"] + df$ModelEDK[df$Measure == "TN"]
    incorrect_modelEDK <- df$ModelEDK[df$Measure == "FP"] + df$ModelEDK[df$Measure == "FN"]
    
    # Accuracy contingency table
    accuracy <- matrix(c(correct_modelED, incorrect_modelED,
                         correct_modelEDK, incorrect_modelEDK),
                       nrow = 2, byrow = TRUE,
                       dimnames = list("Model ED" = c("Correct", "Incorrect"),
                                       "Model EDK" = c("Correct", "Incorrect"))
                       )
    
    # McNemar test for accuracy
    mcnemar_accuracy <- mcnemar.test(accuracy)
  
  
    ## Precision
    tp_modelED <- df$ModelED[df$Measure == "TP"]
    fp_modelED <- df$ModelED[df$Measure == "FP"]
    tp_modelEDK <- df$ModelEDK[df$Measure == "TP"]
    fp_modelEDK <- df$ModelEDK[df$Measure == "FP"]
  
    # Precision contingency table
    precision <- matrix(c(tp_modelED, fp_modelED, tp_modelEDK, fp_modelEDK),
                        nrow = 2, byrow = TRUE,
                        dimnames = list("Model ED" = c("TP", "FP"),
                                        "Model EDK" = c("TP", "FP")))
  
    # McNemar test for precision
    mcnemar_precision <- mcnemar.test(precision)
  
  
    # Recall
    tp_modelED <- df$ModelED[df$Measure == "TP"]
    fn_modelED <- df$ModelED[df$Measure == "FN"]
    tp_modelEDK <- df$ModelEDK[df$Measure == "TP"]
    fn_modelEDK <- df$ModelEDK[df$Measure == "FN"]
    
    # Recall contingency table
    recall <- matrix(c(tp_modelED, fn_modelED, tp_modelEDK, fn_modelEDK),
                     nrow = 2, byrow = TRUE,
                     dimnames = list("Model ED" = c("TP", "FN"),
                                     "Model EDK" = c("TP", "FN")))
    
    # Perform the McNemar test for recall
    mcnemar_recall <- mcnemar.test(recall)
    
    ## Actual scores
    scoresED <- metrics_layer_ed %>%
      summarise(
        Accuracy = sum(TP + TN) / sum(TP + TN + FP + FN),
        Precision = sum(TP) / sum(TP + FP),
        Recall = sum(TP) / sum(TP + FN),
        F1 = ifelse(Precision + Recall == 0, 0,
                    2 * Precision * Recall / (Precision + Recall))
      )
    
    class_scoresED <- metrics_layer_ed %>%
      group_by(Class) %>%
      summarise(
        Accuracy = sum(TP + TN) / sum(TP + TN + FP + FN),
        Precision = sum(TP) / sum(TP + FP),
        Recall = sum(TP) / sum(TP + FN),
        F1 = ifelse(Precision + Recall == 0, 0,
                    2 * Precision * Recall / (Precision + Recall))
      )
    
    scoresEDK <- metrics_layer_edk %>%
      summarise(
        Accuracy = sum(TP + TN) / sum(TP + TN + FP + FN),
        Precision = sum(TP) / sum(TP + FP),
        Recall = sum(TP) / sum(TP + FN),
        F1 = ifelse(Precision + Recall == 0, 0,
                    2 * Precision * Recall / (Precision + Recall))
      )
    
    class_scoresEDK <- metrics_layer_edk %>%
      group_by(Class) %>%
      summarise(
        Accuracy = sum(TP + TN) / sum(TP + TN + FP + FN),
        Precision = sum(TP) / sum(TP + FP),
        Recall = sum(TP) / sum(TP + FN),
        F1 = ifelse(Precision + Recall == 0, 0,
                    2 * Precision * Recall / (Precision + Recall))
      )  
    
    ed_results <- rbind(data.frame(scoresED) %>%
                          mutate(Class = 'All classes') %>%
                          select(Class, everything()),
                        data.frame(class_scoresED)) %>%
      mutate(Layer = 'ED') %>% select(Layer, everything())
    
    edk_results <- rbind(data.frame(scoresEDK) %>%
                           mutate(Class = 'All classes') %>%
                           select(Class, everything()),
                         data.frame(class_scoresEDK)) %>%
      mutate(Layer = 'EDK') %>% select(Layer, everything())
    Metrics <- rbind(ed_results, edk_results)
    Comparison <- data.frame(
      Method = rep(mcnemar_accuracy$method, 3),
      Metric = c('Accuracy', 'Precision', 'Recall'),
      Statistic = c(mcnemar_accuracy$statistic, 
                    mcnemar_precision$statistic,
                    mcnemar_recall$statistic),
      DF = rep(mcnemar_accuracy$parameter, 3),
      PValue = c(mcnemar_accuracy$p.value,
                 mcnemar_precision$p.value,
                 mcnemar_recall$p.value)
      
    )
    
    sheet=createSheet(wb, paste0('Results Data ', i))
    addDataFrame(accuracy, sheet = sheet, startRow = 1, row.names = FALSE)
    addDataFrame(precision, sheet = sheet, startRow = 6, row.names = FALSE)
    addDataFrame(recall, sheet = sheet, startRow = 11, row.names = FALSE)
    addDataFrame(Comparison, sheet = sheet, startRow = 17, row.names = FALSE)
    addDataFrame(Metrics, sheet = sheet, startRow = 22, row.names = FALSE)
    
    
    
}

wb = createWorkbook()
for (i in 1:5) {
  metrics_comparison(get(paste0('metrics_ed', i)), get(paste0('metrics_edk', i)))
}

saveWorkbook(wb, "Stage 1 Results.xlsx")

## F1 Scores Dataset 1
# These were entered manually
f1_scores_modelnDK <- c(90.35, 87.29, 96.57, 86.34, 87.25, 80.18)  
f1_scores_modeliDK <- c(92.22, 87.49, 97.94, 82.45, 99.77, 82.24)  

# Paired t-test
t.test(f1_scores_modelnDK, f1_scores_modeliDK, paired = TRUE)

## F1 Scores Dataset 2
f1_scores_modelnDK <- c(57.82, 0.00, 97.22, 0.00, 0.00, 0.00)  
f1_scores_modeliDK <- c(95.36, 97.25, 90.87, 98.85, 99.96, 97.12)  

# Paired t-test
t.test(f1_scores_modelnDK, f1_scores_modeliDK, paired = TRUE)

## F1 Scores Dataset 3
f1_scores_modelnDK <- c(51.93, 0.00, 56.96, 96.27, 0.00, 0.00)  
f1_scores_modeliDK <- c(99.40, 98.98, 99.41, 98.90, 100.00, 100.00)  

# Paired t-test
t.test(f1_scores_modelnDK, f1_scores_modeliDK, paired = TRUE)

## F1 Scores Dataset 4
f1_scores_modelnDK <- c(78.25, 60.91, 97.21, 64.13, 52.02, 56.44)  
f1_scores_modeliDK <- c(98.26, 98.09, 99.95, 93.40, 98.81, 99.17)  

# Paired t-test
t.test(f1_scores_modelnDK, f1_scores_modeliDK, paired = TRUE)

## F1 Scores Dataset 5
f1_scores_modelnDK <- c(67.72304765, 0.311486048, 97.25683011, 64.39081813,
                        0.015578751, 25.37097833)
f1_scores_modeliDK <- c(99.18783934, 98.25858181, 99.99566903, 98.86767423,
                        97.1166348, 100)  
# Paired t-test
t.test(f1_scores_modelnDK, f1_scores_modeliDK, paired = TRUE)
