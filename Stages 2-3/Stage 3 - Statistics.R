library(tidyverse)
library(xlsx)
path = 'your/path/here'
setwd(path)
print('Importing Gold Standard dictionary...')
gold_standard <- read.csv('gold_standard_dictionary.csv',
                          header = TRUE, 
                          stringsAsFactors = FALSE)

# Variable classes re-assignments
datevars <- gold_standard$Label[which(gold_standard$Type == 'date')]
numvars <- gold_standard$Label[which(gold_standard$Type == 'numeric')]
textvars <- gold_standard$Label[which(gold_standard$Type == 'text')]

DS01_1 <- read.csv(paste0(path, "/DS01_1.csv"), header = TRUE, stringsAsFactors = FALSE)
DS01_2 <- read.csv(paste0(path, "/DS01_2.csv"), header = TRUE, stringsAsFactors = FALSE)
DS01_3 <- read.csv(paste0(path, "/DS01_3.csv"), header = TRUE, stringsAsFactors = FALSE)
DS01_4 <- read.csv(paste0(path, "/DS01_4.csv"), header = TRUE, stringsAsFactors = FALSE)
DS01_5 <- read.csv(paste0(path, "/DS01_5.csv"), header = TRUE, stringsAsFactors = FALSE)

DS00_1 <- read.csv(paste0(path, "/DS00_1.csv"), header = TRUE, stringsAsFactors = FALSE)%>%
  select(-ILL0_HOSPITALIZED_DAYS)
DS00_2 <- read.csv(paste0(path, "/DS00_2.csv"), header = TRUE, stringsAsFactors = FALSE)%>%
  select(-ILL0_HOSPITALIZED_DAYS)
DS00_3 <- read.csv(paste0(path, "/DS00_3.csv"), header = TRUE, stringsAsFactors = FALSE)%>%
  select(-ILL0_HOSPITALIZED_DAYS)
DS00_4 <- read.csv(paste0(path, "/DS00_4.csv"), header = TRUE, stringsAsFactors = FALSE)%>%
  select(-ILL0_HOSPITALIZED_DAYS)
DS00_5 <- read.csv(paste0(path, "/DS00_5.csv"), header = TRUE, stringsAsFactors = FALSE)%>%
  select(-ILL0_HOSPITALIZED_DAYS)


## Function call: Identify valid and invalid values compared to the gold standard dictionary.
compare_to_gold_standard <- function(cleaned_data, gold_standard) {
  
  # Exclude columns HHID and PID from comparison
  exclude_cols <- c('HHID_ID', 'PART_ID')
  compare_cols <- setdiff(colnames(cleaned_data), exclude_cols)
  
  # Initialize a matrix to store validity indicators
  valid_indicators <- matrix(0, nrow = nrow(cleaned_data), ncol = length(compare_cols))
  colnames(valid_indicators) <- compare_cols
  
  # Iterate over each variable (column) in cleaned_data
  for (col_index in seq_along(compare_cols)) {
    var <- compare_cols[col_index]
    cleaned_var <- cleaned_data[[var]]
    
    # Filter the gold_standard for the current variable
    gold_var <- gold_standard %>% filter(Label == var)
    
    # Handle the case where there are no matching rows in gold_standard
    if (nrow(gold_var) == 0) {
      next
    }
    
    # Get the relevant TEXT_RULES_VAR and TEXT_RULES_VAL
    rules_var <- gold_var$TEXT_RULES_VAR[1]
    rules_val <- gold_var$TEXT_RULES_VAL[1]
    
    # Handle the case where the variable is a free text field
    if (gold_var$Type[1] == 'free text field') {
      valid_indicators[, col_index] <- 1
      next
    }
    
    # Iterate over each row in cleaned_data
    for (i in 1:nrow(cleaned_data)) {
      cleaned_value <- cleaned_var[i]
      
      # Handle numeric and date conversions
      if (var %in% numvars) {
        cleaned_value <- as.numeric(cleaned_value)
        gold_entities <- as.numeric(unlist(strsplit(as.character(gold_var$Entity), ",")))
      } else if (var %in% datevars) {
        cleaned_value <- as.character(cleaned_value)  # Adjust date format if needed
        gold_entities <- as.character(unlist(strsplit(as.character(gold_var$Entity), ",")))
      } else {
        gold_entities <- as.character(gold_var$Entity)
      }
      
      # Validate the cleaned value
      if (is.na(cleaned_value)) {
        next
      }
      
      # Apply validation rules
      is_valid <- FALSE
      
      if (is.na(rules_var)) {
        if (var %in% numvars || var %in% datevars) {
          if (cleaned_value >= min(gold_entities, na.rm = TRUE) && 
              cleaned_value <= max(gold_entities, na.rm = TRUE)) {
            is_valid <- TRUE
          }
        } else if (var %in% textvars) {
          if (cleaned_value %in% gold_entities) {
            is_valid <- TRUE
          }
        }
      } else if (!is.na(rules_var) && !is.na(rules_val)) {
        if (var %in% textvars) {
          if (cleaned_value %in% gold_entities &&
              cleaned_data[[rules_var]][i] %in% unlist(strsplit(as.character(rules_val), ","))) {
            is_valid <- TRUE
          }
          if (cleaned_value == "N/A" &&
              !cleaned_data[[rules_var]][i] %in% unlist(strsplit(as.character(rules_val), ","))) {
            is_valid <- TRUE
          }
        } else if (var %in% numvars || var %in% datevars) {
          if (cleaned_value >= min(gold_entities, na.rm = TRUE) && 
              cleaned_value <= max(gold_entities, na.rm = TRUE) &&
              cleaned_data[[rules_var]][i] %in% unlist(strsplit(as.character(rules_val), ","))) {
            is_valid <- TRUE
          }
          if (cleaned_value == "N/A" &&
              !cleaned_data[[rules_var]][i] %in% unlist(strsplit(as.character(rules_val), ","))) {
            is_valid <- TRUE
          }
        }
      }
      
      if (is_valid) {
        valid_indicators[i, col_index] <- 1
      }
    }
  }
  
  # Calculate valid counts per variable and overall
  valid_counts <- colSums(valid_indicators, na.rm = TRUE)
  
  # Create a data frame to summarize valid counts
  valid_summary <- data.frame(
    Variable = compare_cols,
    Valid_Count = valid_counts
  )
  
  # Return the summary and valid indicators matrix
  return(list(valid_summary = valid_summary, valid_indicators = valid_indicators))
}
  
  
# Loop function over the five datasaet pairs
wb = createWorkbook()
for (i in 1:5) {
  ed_comparison <- compare_to_gold_standard(cleaned_data = get(paste0('DS01_', i)), 
                                            gold_standard = gold_standard)
  edk_comparison <- compare_to_gold_standard(cleaned_data = get(paste0('DS00_', i)), 
                                             gold_standard = gold_standard)
  
  # Function to calculate TP, TN, FP, FN
  calculate_tp_tn_fp_fn <- function(valid_indicators_df1, valid_indicators_df2) {
    # Ensure both matrices have the same dimensions
    stopifnot(identical(dim(valid_indicators_df1), dim(valid_indicators_df2)))
    
    # Calculate TP, TN, FP, FN
    TP <- sum(valid_indicators_df1 == 1 & valid_indicators_df2 == 1)
    TN <- sum(valid_indicators_df1 == 0 & valid_indicators_df2 == 0)
    FP <- sum(valid_indicators_df1 == 1 & valid_indicators_df2 == 0)
    FN <- sum(valid_indicators_df1 == 0 & valid_indicators_df2 == 1)
    
    # Create a contingency table
    cont_table <- matrix(
      c(TP, FP, FN, TN),
      nrow = 2,
      byrow = TRUE,
      dimnames = list("Dataset 1" = c("Valid", "Invalid"),
                      "Dataset 2" = c("Valid", "Invalid"))
    )
    
    return(list(cont_table = cont_table, TP = TP, TN = TN, FP = FP, FN = FN))
  }
  
  # Calculate TP, TN, FP, FN 
  results <- calculate_tp_tn_fp_fn(ed_comparison$valid_indicators, 
                                   edk_comparison$valid_indicators)
  cont_table <- results$cont_table
  
  # Perform McNemar's test
  mcnemar_test <- mcnemar.test(cont_table)
  
  # Calculate proportions
  prop_validED <- sum(ed_comparison$valid_indicators) / 
    (nrow(ed_comparison$valid_indicators) * ncol(ed_comparison$valid_indicators))
  prop_validEDK <- sum(edk_comparison$valid_indicators) / 
    (nrow(edk_comparison$valid_indicators) * ncol(edk_comparison$valid_indicators))
  
  
  Comparison <- data.frame(
    ValidPropED = prop_validED,
    ValidPropEDK = prop_validEDK,
    Method = mcnemar_test$method,
    Statistic = mcnemar_test$statistic,
    DF = mcnemar_test$parameter,
    PValue = mcnemar_test$p.value)
    
  
  sheet=createSheet(wb, paste0('Results Data ', i))
  addDataFrame(cont_table, sheet = sheet, startRow = 1, row.names = FALSE)
  addDataFrame(Comparison, sheet = sheet, startRow = 6, row.names = FALSE)
}

saveWorkbook(wb, "Stage 3 Results.xlsx")

