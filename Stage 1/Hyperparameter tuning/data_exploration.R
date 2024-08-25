## This is the ccomparison of the metrics between both layers for each dataset.
library(tidyverse)
library(xlsx)
library(ggplot2)
library(showtext)
library(tm)
path = '<your/path/here>'
setwd(path)
print('Importing COPA Individual file...')
ind_data <- read.csv('data/copa_ind.csv', header = TRUE, stringsAsFactors = FALSE)

print('Importing COPA KAP file...')
kap_data <- read.csv('data/copa_kap.csv', header = TRUE, stringsAsFactors = FALSE)

print('Importing COPA House Representative file...')
hr_data <- read.csv('data/copa_hh.csv', header = TRUE, stringsAsFactors = FALSE) %>%
  select(-FUMAGATION_COMM_12MO)

dfa <- Reduce(function(...) merge(..., all = TRUE),
                      list(ind_data, kap_data, hr_data))

print('Importing Gold Standard dictionary...')
gold_standard_dictionary <- read.csv('gold_standard_dictionary.csv',
                                     header = TRUE, 
                                     stringsAsFactors = FALSE) %>%
  mutate(
    Class_Identifier = paste0(Class, ':', Label, ':', Entity),
    Identifier = paste0(Label, ':', Entity)
  )
dfb <- read.csv(paste0(path, '/dataset_2.csv'),
               header = T, stringsAsFactors = F)
dfc <- read.csv(paste0(path, '/dataset_3.csv'),
                header = T, stringsAsFactors = F)
dfd <- read.csv(paste0(path, '/dataset_4.csv'),
                header = T, stringsAsFactors = F)
dfe <- read.csv(paste0(path, '/dataset_5.csv'),
                header = T, stringsAsFactors = F)
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

df <- dfb
transformed_data <- preprocess_text_data(df) 

# Function call
long_format <- function(corpus, text_data) {
  text_labels <- rep(names(text_data), each = nrow(text_data))
  labeled_textdata <- data.frame(
    Entity = sapply(corpus, as.character),
    Label = text_labels
  )
  return(labeled_textdata)
}

# Generate dictionaries
labeled_data <- long_format(transformed_data$corpus,
                            transformed_data$text_data) %>%
  filter(Entity != '') %>%
  filter(!Label %in% c('HHID_ID', 'PART_ID', 'ODATE', 'COVID_POSITIVE_DT')) %>%
  mutate(Type = gold_standard_dictionary$Type[match(Label, gold_standard_dictionary$Label)],
         Class = gold_standard_dictionary$Class[match(Label, gold_standard_dictionary$Label)])


numdate <- select(df, any_of(c(numvars, datevars))) %>%
  mutate_all(as.character) %>%
  pivot_longer(everything(), names_to = 'Label', values_to = "Entity") %>%
  mutate(Type = gold_standard_dictionary$Type[match(Label, gold_standard_dictionary$Label)],
         Class = gold_standard_dictionary$Class[match(Label, gold_standard_dictionary$Label)])

for_exploration <- do.call(rbind, list(labeled_data[!duplicated(labeled_data), ],
                                       numdate[!duplicated(numdate), ]))
final_labeled_data <- for_exploration[!duplicated(for_exploration), ]

## Data exploration
df1 <- final_labeled_data %>%
  group_by(Class) %>%
  summarise(.groups = 'drop', Labels = length((Label)))

font_add_google("Roboto Condensed")
showtext_auto()  

ggplot(df1, aes(y = Class, x = Labels)) +
  geom_text(label= df1$Labels, nudge_x = 50, size = 5) +
  geom_bar(stat = 'identity', fill = 'cadetblue')  +
  theme_classic() +
  theme(text=element_text(size=20, 
                          family = "Roboto Condensed")) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, face = "plain",
                                   family = "Roboto Condensed"),
        axis.text.y = element_text(color = "grey20", size = 18, face = "plain",
                                   family = "Roboto Condensed"),  
        axis.title.x = element_text(color = "grey20", size = 25, face = "bold",
                                    family = "Roboto Condensed"),
        axis.title.y = element_text(color = "grey20", size = 25, face = "bold",
                                    family = "Roboto Condensed")) 


df2 <- final_labeled_data %>%
  group_by(Class) %>%
  summarise(.groups = 'drop', Entities = length(unique(Entity)))

ggplot(df2, aes(y = Class, x = Entities)) +
  geom_text(label= df2$Entities, 
            nudge_x = 20, size = 5) +
  geom_bar(stat = 'identity', fill = 'cadetblue') +
  theme_classic() +
  theme(text=element_text(size=20, 
                          family = "Roboto Condensed")) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, face = "plain",
                                   family = "Roboto Condensed"),
        axis.text.y = element_text(color = "grey20", size = 18, face = "plain",
                                   family = "Roboto Condensed"),  
        axis.title.x = element_text(color = "grey20", size = 25, face = "bold",
                                    family = "Roboto Condensed"),
        axis.title.y = element_text(color = "grey20", size = 25, face = "bold",
                                    family = "Roboto Condensed")) 



df1means <- final_labeled_data %>%
  group_by(Class, Label) %>%
  count() %>%
  group_by(Class) %>%
  summarise(.groups = 'drop',
            min = summary(n)[1],
            max = summary(n)[6],
            mean = summary(n)[4],
            median = summary(n)[3],
            Q1 = summary(n)[2],
            Q2 = summary(n)[5])

ggplot(df1means, aes(y=Class, x=n), fill='cadetblue') +
  geom_boxplot()
