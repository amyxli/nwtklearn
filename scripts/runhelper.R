packages = c('tidyverse','here', 'data.table')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

## load data

files <- list.files(path = here("data", "exp3"), pattern = "*.csv", full.names = TRUE,  recursive = TRUE) 

combine <- lapply(1:length(files), function(x){
  
  p <- read.csv(files[x])
  
  p$subject = x
  
  p
  
})

data <- rbindlist(combine, use.names = TRUE, fill=TRUE)

data <- data %>% mutate(version = case_when(is.na(choiceName) ~ 'pilot_1',
                                    !is.na(choiceName) ~ 'pilot_2',
                                    TRUE ~ ''))


# keep the rows with prolific ID
tmp <- data %>%
  dplyr::filter(trial_index == 0)

# check bonuses for assignment
tmp$responses <- substr(tmp$responses, 15, 34)
bonuses <- tmp %>%
  dplyr::select(c(responses, bonus))
view(tmp)


# assign link versions

data <- data %>%
  mutate(linkversion = case_when(
    infoCond == "info" & optimCond == "sub" ~ "1",
    infoCond == "info" & optimCond == "eq" ~ "2",
    infoCond == "info" & optimCond == "optim" ~ "3",
    
    infoCond == "noninfo" & optimCond == "sub" ~ "4",
    infoCond == "noninfo" & optimCond == "eq" ~ "5",
    infoCond == "noninfo" & optimCond == "optim" ~ "6",

    TRUE ~ "ERROR"
    
  ))

## Exclusions ####
attnAttempts <- data %>%
  filter(grepl("Q1", responses)) %>% # attn check item
  group_by(PID) %>%
  summarise(n_tries = n()) %>% # number of attempts
  arrange(desc(n_tries))

attnExclude_ids <- subset(attnAttempts, n_tries > 3)$PID

data <- data %>% filter(!(PID %in% attnExclude_ids)) # filter these people out of dataset
data <- data %>% filter(PID != 97580)


# demographics information
demographics <- 
  data %>% 
  dplyr::select(c(subject, age, gender, version, linkversion, infoCond, optimCond, bonus)) %>% 
  unique()

demographics %>% count(gender)

# count how many of each cell
demographics %>% count(infoCond, optimCond)
demographics %>% count(linkversion)


