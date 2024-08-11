# This code will create a synthetic dataset based on selected variables from two
# COPA datasets. Errors will be introduced at 5%.

print('Starting the synthetic data generation process using raw data...')
print('Function calls... Generating function to count syllables and introduce errors...')
print('This funtion introduces word perturbations depending on the number of syllables found...')

# Function to count syllables in a word
count_syllables <- function(word) {
  word <- tolower(word)
  vowels <- c('a', 'e', 'i', 'o', 'u', 'y')
  count <- 0
  in_vowel <- FALSE
  for (char in strsplit(word, '')[[1]]) {
    if (char %in% vowels) {
      if (!in_vowel) {
        count <- count + 1
        in_vowel <- TRUE
      }
    } else {
      in_vowel <- FALSE
    }
  }
  return(count)
}

# Introduce 5% of errors
introduce_errors <- function(text, perturbation_probability = 0.05) {
  text <- tolower(text)
  words <- unlist(strsplit(text, '_'))
  words <- words[!is.na(words)]
  
  # Check if words is empty
  if (length(words) == 0) {
    return('')
  }
  
  # Determine if perturbation should occur
  if (runif(1) < perturbation_probability) {
    # Select a random word
    word_index <- sample(seq_along(words), 1)
    word <- words[word_index]
    
    # Get syllable count of the word
    syllable_count <- count_syllables(word)
    
    # Perturb a single character within the word based on syllable count
    if (syllable_count == 1) {
      # Monosyllable: Show only the first character
      words[word_index] <- substr(word, 1, 1)
    } else if (syllable_count == 2) {
      # Bi-syllable: Introduce typographical error
      # Randomly choose a character to replace with a random letter
      replace_index <- sample(1:nchar(word), 1)
      new_char <- sample(setdiff(letters, substr(word, replace_index, 
                                                 replace_index)), 1)
      words[word_index] <- paste0(substr(word, 1, replace_index - 1), new_char, 
                                  substr(word, replace_index + 1, nchar(word)))
    } else {
      # Poly-syllable: Introduce typographical error
      # Randomly choose a character to replace with a random letter
      replace_index <- sample(1:nchar(word), 1)
      new_char <- sample(setdiff(letters, substr(word, replace_index, 
                                                 replace_index)), 1)
      words[word_index] <- paste0(substr(word, 1, replace_index - 1), new_char, 
                                  substr(word, replace_index + 1, nchar(word)))
    }
  }
  
  # Combine perturbed words
  perturbed_text <- paste(words, collapse = '_')
  return(perturbed_text)
}


print('Function to alter numeric values...')

# Function to increase a numeric value times 100
alter_num_range <- function(data, replacement_pct = 0.3) {
  # Copy the data to avoid modifying the original data frame
  new_data <- data
  
  # Loop over each numeric variable in the data frame
  for (col_name in names(data)) {
    if (is.numeric(data[[col_name]])) {
      num_var <- data[[col_name]]
      
      # Remove missing values and get the indices of non-missing values
      non_missing_indices <- which(!is.na(num_var))
      num_non_missing <- length(non_missing_indices)
      
      # Determine the number of numeric values to replace
      num_to_replace <- ceiling(num_non_missing * replacement_pct)
      
      # Randomly select numeric values to replace from non-missing values
      replace_indices <- sample(non_missing_indices, num_to_replace, replace = FALSE)
      
      # Increase selected numeric values by 10
      for (index in replace_indices) {
        num_var[index] <- num_var[index] + 10
      }
      
      # Update the specific variable in the copied data frame
      new_data[[col_name]] <- num_var
    }
  }
  
  return(new_data)
}


print('Function to generate synthetic dataset using previous functions...')
generate_synthetic_data <- function(data, original_data, synth_name) {
  tmp_data <- data %>%
    mutate(ODATE = as.Date(ODATE),
           COVID_POSITIVE_DT = as.Date(COVID_POSITIVE_DT))
  text <- select(tmp_data, names(tmp_data)[sapply(tmp_data, is.character)])
  num <- select(tmp_data, names(tmp_data)[sapply(tmp_data, is.numeric)],
                          names(tmp_data)[sapply(tmp_data, is.integer)])
  
  synth_text <- list()
  for(i in names(text)) {
    tmp <- data.frame(x = sapply(text[[i]], introduce_errors))
    colnames(tmp) <- c(i)
    synth_text[[i]] <- tmp
  }
  
  synthetic_text_data <- do.call(cbind, synth_text)
  synthetic_text_data[synthetic_text_data == ''] <- NA
  
  synth_num <- list()
  for(i in names(num)) {
    tmp <- data.frame(x = sapply(num[[i]], alter_num_range))
    colnames(tmp) <- c(i)
    synth_num[[i]] <- tmp
  }
  
  synthetic_num_data <- do.call(cbind, synth_num)
  synthetic_num_data[synthetic_num_data == ''] <- NA
  
  synthetic_data <- do.call(cbind, list(synthetic_text_data, 
                                        synthetic_num_data)) %>%
    mutate(ODATE = original_data$ODATE,
           COVID_POSITIVE_DT = original_data$COVID_POSITIVE_DT,
           PART_ID = original_data$PART_ID,
           HHID_ID = original_data$HHID_ID,
           AGE_GROUP = original_data$AGE_GROUP) %>%
    select(HHID_ID, PART_ID, AGE_GROUP, everything())
  
  assign(synth_name, synthetic_data, envir = .GlobalEnv)
}

# Function to sample and maintain missing data proportion
sample_data_with_na <- function(data, na_percentage = 0.25) {
  data = select(data, -PART_ID, -HHID_ID, -AGE_GROUP)
  sampled_data <- data[sample(nrow(data), replace = TRUE), ]
  for (col in names(data)) {
    original_values <- data[[col]]
    sampled_values <- sampled_data[[col]]
    
    if (sum(is.na(original_values)) > 0) {
      na_indices <- which(is.na(original_values))
      
      # Calculate how many NA values to replace
      num_na_to_replace <- round(na_percentage * length(na_indices))
      na_indices_to_replace <- sample(na_indices, num_na_to_replace)
      
      # Replace NA values at sampled indices
      sampled_values[na_indices_to_replace] <- NA
      sampled_data[[col]] <- sampled_values
    }
  }
  return(sampled_data)
}

print('Setting seed...')
set.seed(54321)
print('Looping through the synthetic data generation process.')
for (i in 1:20) {
  output_file <- paste0('synthetic_data', i) 
  sampled_data <- sample_data_with_na(merged_data)
  generate_synthetic_data(sampled_data, merged_data, output_file)
  write.csv(get(output_file), paste0('data/', output_file, '.csv'), row.names = FALSE)
}

print('Datasets have been stored in front-end memory and can be sourced.')
