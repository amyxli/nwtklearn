# learning NWTK Exp 1
# created axl 22/2/22
# lasted edited axl 10/8/23

# load the libraries
library(tidyverse)
library(here)
library(data.table)
library(dplyr)
library(emmeans)
library(ggthemes) 

# load in the data
files <- list.files(path = here("data","exp1"), pattern = "*.csv", full.names = TRUE) 

combine <- lapply(files, function(file_path) {
  if (file.size(file_path) > 0) {  # Check if file is not empty
    p <- read.csv(file_path)
    p
  } else {
    NULL  # Skip empty files
  }
})

#exclude incomplete datasets

combine <- combine %>%
  lapply(function(df){
    df %>%
      filter(nrow(df) >= 76)
    # if the tibble is truncated (i.e. has less than 30 columns) it's technically incomplete
    # but those with 29 columns still have a complete dataset, just missing either engagement or post-engagement data
    # so we'll allow it...
  }) %>%
  discard(function(x) nrow(x) == 0) # discard all 0-row tibbles

data <-  rbindlist(combine, use.names = TRUE, fill=TRUE) # check whether people finish

# Exclusions ####

# exclude people if they've failed the attention check more than three times

attnAttempts <- data %>%
  filter(grepl("Q1", responses)) %>% # attn check item
  group_by(PID) %>%
  summarise(n_tries = n()) %>% # number of attempts
  arrange(desc(n_tries))

attnExclude_ids <- subset(attnAttempts, n_tries > 3)$PID

data <- data %>% filter(!(PID %in% attnExclude_ids)) # filter these people out of dataset

# Demographics ####

age <- data %>%
  filter( trial_index == 3) %>%
  select(PID, responses)

gender <- data %>%
  filter( trial_index == 4) %>%
  select(PID, responses)

age$responses <- str_extract(age$responses, "\\d+\\.?\\d*")

gender$responses <- gsub("[{}]", "", gender$responses)  # curly brackets
gender$responses <- gsub('"', '', gender$responses)

mean(as.numeric(age$responses))
sd(as.numeric(age$responses))
range(as.numeric(age$responses))

gender %>% count(responses)
# Output:
# > mean(as.numeric(age$responses))
# [1] 36.92929
# > sd(as.numeric(age$responses))
# [1] 10.35616
# > range(as.numeric(age$responses))
# [1] 23 73
# > gender %>% count(responses)
# responses  n
# 1: Q0:Female 37
# 2:   Q0:Male 61
# 3:  Q0:Other  1

#write.csv(data, here("data_tidy","s1_dataRaw_postExclude.csv"), row.names = FALSE)

# Overall KIS vs FON choices
data$PID <- as.character(data$PID) # convert to factors so that .drop=FALSE will work
data$condition <- as.factor(data$condition)

data <- data %>%
  mutate(choice = case_when(
    choice == 1 ~ 0, # 1 was KIS, 2 was FON
    choice == 2 ~ 1, # now 0 is KIS, 1 is FON
    TRUE ~ NA_real_
  ))

#write.csv(data, here("data_tidy", "s1_data_cleaned.csv"), row.names = FALSE)
