# learning NWTK Exp 2
# created axl 7/9/22
# last updated 8/2/23

# load the libraries
library(tidyverse)
library(here)
library(data.table)
library(ggthemes)

# load in the data
files <- list.files(path = here("data","exp2"), pattern = "*.csv", full.names = TRUE) 

combine <- lapply(1:length(files), function(x){
  p <- read.csv(files[x])
  p
}) 

data <- rbindlist(combine, use.names = TRUE, fill=TRUE)

# Some preliminaries.... ####

# Assign link version
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
data <- data %>% filter(PID != 97580) # had nonsense data

#write.csv(data, here("data_tidy","s2_dataRaw_postExclude.csv"), row.names = FALSE)

# See how many in each condition ####

demographics <- data %>% 
  dplyr::select(c(PID, age, gender, infoCond, optimCond, bonus, linkversion)) %>% 
  unique()

demographics %>% count(optimCond, infoCond)
demographics %>% count(linkversion)

demographics %>% count(gender)
demographics$age %>% mean()
demographics$age %>% sd()
demographics$age %>% range()

demographics$bonus %>% mean()

# Overall suboptimal vs optimal choices

data$PID <- as.character(data$PID) # convert to factors so that .drop=FALSE will work
data$infoCond <- as.factor(data$infoCond)
data$optimCond <- as.factor(data$optimCond)

# assign block numbers
data <- data %>%
  mutate(block = case_when(
    banditTrial <= 15 ~ 1,
    banditTrial <= 30 ~ 2,
    banditTrial <= 45 ~ 3,
    banditTrial <= 60 ~ 4,
    TRUE ~ NA_real_
  ))


# convert to 0/1 for analysis

data$choice <- as.numeric(data$choice)

data <- data %>%
  mutate(choice = case_when(
    choice == 1 ~ 0, # 0 is KIS if infoCond
    choice == 2 ~ 1, # 1 is FON
    TRUE ~ NA_real_
  ))


## Explanation of the choice variable ####

# | Choice Value      | Info Cond |  Optim Cond  | Option       | Outcome     |
# | ----------------- | --------- | ------------ | ------------ | ----------- |
# | 0                 | noninfo   | sub          | sq/tri       | optimal~120 |
# | 1                 | noninfo   | sub          | sq/tri       | sub~80      |
# | 0                 | info      | sub          | KIS          | optimal~120 |
# | 1                 | info      | sub          | FON          | sub~80      |

# | 0                 | noninfo   | eq           | sq/tri       | ~100        |
# | 1                 | noninfo   | eq           | sq/tri       | ~100        |
# | 0                 | info      | eq           | KIS          | ~100        |
# | 1                 | info      | eq           | FON          | ~100        |

# | 0                 | noninfo   | optim        | sq/tri       | sub~80      |
# | 1                 | noninfo   | optim        | sq/tri       | optimal~120 |
# | 0                 | info      | optim        | KIS          | sub~80      |
# | 1                 | info      | optim        | FON          | optimal~120 |

## Fix data inconsistencies ####

# sometimes images were in NWTKimg instead of imgNWTK

data <- data %>% 
  mutate_all(funs(str_replace(., "NWTKimg", "imgNWTK")))

## Correct data errors ####

# substim and optstim is swapped for info optim, info sub, and non-info sub!
# however correct for noninfo optim

data <- data %>% mutate(
  subStim_corr = case_when(
    infoCond == "info" ~ optStim,
    infoCond == "noninfo" & optimCond == "sub" ~ optStim,
    TRUE ~ subStim # otherwise just use the original
  ),
  optStim_corr = case_when(
    infoCond == "info" ~ subStim,
    infoCond == "noninfo" & optimCond == "sub" ~ subStim,
    TRUE ~ optStim # otherwise just use the original
  )
)

data <- data %>% select(c(PID, infoCond, optimCond, optStim_corr, subStim_corr, block, banditTrial, choice, cue, choiceName, feedback, leftOption, rightOption, leftInput, rightInput, bonus,
                          responses, postQ, singleEst)) %>%
  rename(optStim = optStim_corr, subStim = subStim_corr)

## Combine sub, noninfo, and optim, noninfo, since they are the same condition; and duplicate ####
data <- data %>%
  mutate(condition = paste0(optimCond, ", ", infoCond))

data_subopt_noninfo <- data %>%
  filter(condition == "sub, noninfo" | condition == "optim, noninfo")

# Need to flip choice from 0 to 1 if using sub, noninfo in optim, noninfo; and vice versa

## make a new "sub, noninfo" dataframe which contains data from both sub and optim noninfo; note that we need
## to flip choice values
data_subopt_noninfo$choice <- as.numeric(data_subopt_noninfo$choice)

data_subnoninfo <- data_subopt_noninfo %>%
  mutate(choice2 = ifelse(condition == "optim, noninfo",
                          ifelse(choice == 0, 1, 0),
                          choice))

data_subnoninfo$optimCond <- "sub" #forcibly assign infocond and condition
data_subnoninfo$condition <- "sub, noninfo" #forcibly assign infocond and condition

data_subnoninfo <- data_subnoninfo %>%
  select(-c(choice)) %>%
  rename(choice = choice2)

## do the same for a new "optim, noninfo" condition

data_optimnoninfo <- data_subopt_noninfo %>%
  mutate(choice2 = ifelse(condition == "sub, noninfo",
                          ifelse(choice == 0, 1, 0),
                          choice))

data_optimnoninfo$optimCond <- "optim" #forcibly assign infocond and condition
data_optimnoninfo$condition <- "optim, noninfo" #forcibly assign infocond and condition

data_optimnoninfo <- data_optimnoninfo %>%
  select(-c(choice)) %>%
  rename(choice = choice2)

## do a check

data_optimnoninfo$PID %>% unique() == data_subnoninfo$PID %>% unique() # check that all PIDs equivalent in both dfs

### assign some random PIDs to second df or else processing becomes painful later

data_optimnoninfo$PID <- as.numeric(data_optimnoninfo$PID)
data_optimnoninfo$PID <- data_optimnoninfo$PID + 191293823

data_optimnoninfo$PID <- as.character(data_optimnoninfo$PID)

## now substitute old dataframe rows with these new ones

data <- data %>%
  filter(condition != "sub, noninfo" & condition != "optim, noninfo")

data <- rbind(data, data_subnoninfo) # now bind the rows together
data <- rbind(data, data_optimnoninfo)

#write.csv(data, here("data_tidy", "s2_data_cleaned.csv"), row.names = FALSE)
