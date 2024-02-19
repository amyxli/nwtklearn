# learning NWTK Exp 2
# created axl 7/9/22
# last updated 12/12/23

# load the libraries
library(tidyverse)
library(here)
library(data.table)
library(ggthemes)
library(lmtest)

# load the data
data <- read.csv(here("data_tidy", "s2_data_cleaned.csv"))

# note that now "data" has two copies of the noninformative, unequal participants!!

trialdat <- data %>% filter(!is.na(banditTrial)) %>%
  select(c(PID, condition, infoCond, optimCond, block, banditTrial, choice, choiceName, cue, feedback)) %>%
  arrange(PID, banditTrial) 

## CHOICE NAME CODING; save into df so it's clear what "target option" was for non-info condition

choiceNameData<- trialdat %>% select(c(PID, choice, choiceName)) %>% unique()

# Analysis of the trial choice data ####

## visualise prop FON / "1" choices over blocks ####

trialdat$block <- as.factor(trialdat$block)
trialdat$choice <- as.factor(trialdat$choice)
# trialdat$condition <- as.factor(trialdat$condition)

propFON_blocks <- trialdat %>%
  group_by(PID, block, choice, condition, .drop=FALSE) %>%
  summarise(n_FON = n()) %>% # chose FON
  filter(choice == 1) 
# note that where there are no choices == 1 in a block, condition is marked as NA so we need to fix this
# by getting the condition that each participant is in,
# and left_joining this information with propFON_blocks

conditionInfo <- trialdat %>%
  select(c(PID, condition)) %>%
  unique()  # get the condition info for each participant

# save trial information

propFON_blocks <- propFON_blocks %>% select(-c(condition))

propFON_blocks <- left_join(conditionInfo, propFON_blocks)

propFON_blocks <- propFON_blocks %>%
  mutate(percent_FON = (n_FON/15)*100) # 15 trials per block

propFON_summary <- propFON_blocks %>%
  group_by(condition, block) %>%
  summarise(stdev = sd(percent_FON, na.rm=TRUE),
            mean = mean(percent_FON, na.rm=TRUE),
            n = n(),
            stderr = stdev/sqrt(n)) %>%
  ungroup()
# check: value of mean for each block of optim, noninfo should be the complement of corresponding row of sub,noninfo
# and stdev should also be the same
propFON_summary %>% filter(condition == "optim, noninfo" | condition == "sub, noninfo")

# add back the optim/info conditions for plotting (a bit convoluted but works)
propFON_summary$optimCond <- gsub(",.*$", "", propFON_summary$condition) #grab bit before comma
propFON_summary$infoCond <- gsub(".*, ", "", propFON_summary$condition) #grab bit after comma

# for plotting
propFON_summary$optimCond <- factor(propFON_summary$optimCond, levels = c("eq", "optim", "sub"),
                                    labels = c("Target & Non-Target \nOptions Equal", "Target Optimal", "Target Suboptimal")
)


ggplot(propFON_summary,
       aes(block, mean, group = condition, col = infoCond)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1) +
  theme_bw() +
  facet_grid(~ optimCond) +
  labs(x="Block", y="Mean % Target Option Choices", col = "Condition") +
  scale_colour_colorblind(labels = c("Informative", "Non-Informative")
  ) 
#ggsave(here("plots","s2_overallTargetChoices.jpg"), width = 6.5, height = 4.2)

## KEY: Predictors of choice on each trial: analyse using glmer ####

library(lme4)
library(emmeans)

## subset based on optimCond, then compare between infoCond within each optimCond
trialdat$PID <- as.factor(trialdat$PID)
trialdatEq <- subset(trialdat, optimCond == "eq")
trialdatOptim <- subset(trialdat, optimCond == "optim")
trialdatSub <- subset(trialdat, optimCond == "sub")


### examine between infocond differences for equal optimcond ####

## relevel for analysis

trialdatEq$infoCond <- as.factor(trialdatEq$infoCond)
trialdatEq$infoCond <-relevel(trialdatEq$infoCond, ref = "info")

# model with both main effects
mod_eq_1 <- glmer(choice ~ infoCond + banditTrial + (1|PID),
                data = trialdatEq,
                family = binomial(link = "logit"),
                nAGQ = 0) 

# model with an interaction
mod_eq_2 <- glmer(choice ~ infoCond*banditTrial + (1|PID),
                    data = trialdatEq,
                    family = binomial(link = "logit"),
                    nAGQ = 0)

lrtest(mod_eq_1, mod_eq_2)
# Likelihood ratio test
# 
# Model 1: choice ~ infoCond + banditTrial + (1 | PID)
# Model 2: choice ~ infoCond * banditTrial + (1 | PID)
# #Df  LogLik Df  Chisq Pr(>Chisq)
# 1   4 -4047.8                     
# 2   5 -4046.8  1 2.1566      0.142

# fetch wald stats from mod_eq_1 model
summary(mod_eq_1) ## infoCond sig (p=.049), banditTrial sig
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)      0.311677   0.112291   2.776  0.00551 **
#   infoCondnoninfo -0.277170   0.140880  -1.967  0.04913 * 
#   banditTrial      0.003188   0.001540   2.070  0.03847 * 

confint(pairs(emmeans(mod_eq_1, ~ infoCond,type="response")))
# contrast       odds.ratio    SE  df asymp.LCL asymp.UCL
# info / noninfo       1.32 0.186 Inf         1      1.74

### now for the optim cond ####
## relevel for analysis

trialdatOptim$infoCond <- as.factor(trialdatOptim$infoCond)
trialdatOptim$infoCond <-relevel(trialdatOptim$infoCond, ref = "noninfo") # set as noninfo for glm, info for emmeans

mod_optim_1 <- glmer(choice ~ infoCond + banditTrial + (1|PID),
                   data = trialdatOptim,
                   family = binomial(link = "logit"),
                   nAGQ = 0)

mod_optim_2 <- glmer(choice ~ infoCond*banditTrial + (1|PID),
                       data = trialdatOptim,
                       family = binomial(link = "logit"),
                       nAGQ = 0)


lrtest(mod_optim_1, mod_optim_2)

# Likelihood ratio test
# 
# Model 1: choice ~ infoCond + banditTrial + (1 | PID)
# Model 2: choice ~ infoCond * banditTrial + (1 | PID)
# #Df  LogLik Df Chisq Pr(>Chisq)
# 1   4 -3857.1                    
# 2   5 -3856.0  1 2.292       0.13

summary(mod_optim_1)

confint(pairs(emmeans(mod_optim_1, ~ infoCond,type="response")))
# contrast       odds.ratio    SE  df asymp.LCL asymp.UCL
# noninfo / info       1.01 0.159 Inf     0.741      1.37

### now for the sub cond ####

trialdatSub$infoCond <- as.factor(trialdatSub$infoCond)
trialdatSub$infoCond <-relevel(trialdatSub$infoCond, ref = "info") # set as noninfo for glm, info for emmeans


mod_sub_1 <- glmer(choice ~ infoCond + banditTrial + (1|PID),
                 data = trialdatSub,
                 family = binomial(link = "logit"),
                 nAGQ = 0)

mod_sub_2 <- glmer(choice ~ infoCond*banditTrial + (1|PID),
                   data = trialdatSub,
                   family = binomial(link = "logit"),
                   nAGQ = 0)

lrtest(mod_sub_1, mod_sub_2)
# Likelihood ratio test
# 
# Model 1: choice ~ infoCond + banditTrial + (1 | PID)
# Model 2: choice ~ infoCond * banditTrial + (1 | PID)
# #Df  LogLik Df Chisq Pr(>Chisq)
# 1   4 -3963.1                    
# 2   5 -3962.9  1 0.387     0.5339

summary(mod_sub_1)

# Value estimates from block 4 ####

block4Ests <- data %>% filter(!is.na(banditTrial)) %>%
  select(c(PID, infoCond, optimCond, 
           subStim, optStim, 
           
           block, banditTrial, choice, feedback,
           choiceName,
           leftOption, rightOption,
           leftInput, rightInput)) %>%
  filter(block == 4)

block4Ests <- block4Ests %>% # convert into long format
  pivot_longer(cols = leftInput:rightInput,
               names_to = "estimateFor") %>%
  mutate(estOption = case_when(
    estimateFor == "leftInput" ~ leftOption,
    estimateFor == "rightInput" ~ rightOption,
    TRUE ~ ""
  )) 


block4Ests <- block4Ests %>%
  mutate(
    optOptimality = case_when(estOption == optStim ~ "optim",
                              estOption == subStim ~ "sub",
                              TRUE ~ "NA")
  )


block4Ests <- block4Ests %>%
  select(c(PID, infoCond, optimCond, block, banditTrial, estOption, optOptimality, value))

choiceNameData$estOption <- choiceNameData$choiceName # use our choice name dataframe to mark estimated option as target or not

tmp <- full_join(block4Ests, choiceNameData, by = c("PID", "estOption")) %>%
  mutate(optionType = ifelse(choice == 1, "target", "non-target"))

block4Ests <- tmp %>%
  select(c(PID, infoCond, optimCond, block, banditTrial, estOption, optionType, optOptimality, value))

# need to clean some values as people accidentally put in apostrophe, etc

block4Ests$value <- gsub("%$","", block4Ests$value)
block4Ests$value <- gsub("`$","", block4Ests$value)
block4Ests$value <- gsub("'$","", block4Ests$value)
block4Ests$value <- str_replace(block4Ests$value, "o", "0") #replace os with 0s
block4Ests$value <- str_replace(block4Ests$value, "o", "0") #and again


block4Ests$value <- as.numeric(block4Ests$value)

block4Ests$condition <- paste0(block4Ests$infoCond, ", ", block4Ests$optimCond)

## Exclusion criteria  ####

### Reported in manuscript ####
# remove estimates with estimates 1000 or higher
block4Ests_postExcl <- block4Ests %>% filter(value < 1000) # remove estimates 1000 or over

# see how many estimates were removed
(nrow(block4Ests)-nrow(block4Ests_postExcl))/nrow(block4Ests)

# remove rows with 0 estimates
block4excl2 <- subset(block4Ests_postExcl, value == 0)$PID %>% unique() # identify these participants

block4Excl0Rows <- block4Ests_postExcl %>% filter(value == 0) 
block4Ests_postExcl <- block4Ests_postExcl %>% filter(value > 0) # for estimates of 0, remove that row only rather than whole subject

# see how many estimates were removed
(nrow(block4Ests)-nrow(block4Ests_postExcl))/nrow(block4Ests)

# save post-exclusion data
#write.csv(block4Ests_postExcl, here("data_tidy", "s2_block4Ests_postExcl.csv"), row.names = FALSE)

### SI Scenario 2 (strict exclusion) ####

# # Exclude participants who gave a response they could not have observed (this differs by condition)
# 
# ## Exclude participants in target & non-target not equal who gave estimates less than 70 
# ## or more than 130 
# ## For target & non target equal, exclude those who gave estimates less than 90 and more than 110
# block4Ests <- block4Ests %>% mutate(
#   minObs = case_when(optimCond == "eq" ~ 90,
#                      .default = 70),
#   maxObs = case_when(optimCond == "eq" ~ 110,
#                      .default = 130)
# )
# 
# block4Est_strictExcl1 <- subset(block4Ests, value<minObs | value>maxObs)$PID %>% unique()
# 
# # Exclude participants who gave ‘0’ responses 
# block4Est_strictExcl2 <- (block4Ests %>% filter(value == 0))$PID %>% unique()
# 
# # remove people 
# 
# block4Ests_postExcl_strict <- block4Ests %>% filter(!(PID %in% block4Est_strictExcl1)
#                                     & !(PID %in% block4Est_strictExcl2)) # for estimates over 1000, remove the whole subject
# 
# block4Ests_postExcl_strict$PID %>% unique()
# 
# # write.csv(block4Ests_postExcl_strict, here("data_tidy", "s2_block4Ests_postExcl_strict.csv"), row.names = FALSE)

## Analyse block 4 estimates ####

#block4Ests_postExcl <- read.csv(here("data_tidy", "s2_block4Ests_postExcl.csv")) # uncomment if running analyses with regular exclusions
# block4Ests_postExcl <- read.csv(here("data_tidy", "s2_block4Ests_postExcl_strict.csv")) # uncomment if running analyses with strict exclusions
#block4Ests_postExcl <- block4Ests # uncomment if running analyses with no exclusions; just using the original block4Ests dataframe

## analyse using lmer ####

library(lme4)
library(lmerTest)

block4Ests_postExcl$infoCond <- as.factor(block4Ests_postExcl$infoCond)
block4Ests_postExcl$infoCond <-relevel(block4Ests_postExcl$infoCond, ref = "noninfo")

### for eq optimCond ####

block4mod_eq <- lmer(value ~ infoCond*optionType + (1|PID),
                       data = subset(block4Ests_postExcl, optimCond == "eq"),
                       REML = FALSE)

summary(block4mod_eq)
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                     95.0134     4.9412  118.9174  19.229   <2e-16 ***
#   infoCondinfo                     5.4322     7.1771  119.5536   0.757    0.451    
# optionTypetarget                -0.4784     2.6717 2512.3200  -0.179    0.858    
# infoCondinfo:optionTypetarget    1.1773     3.8732 2511.4278   0.304    0.761   

### for noneq optimCond ####

#### for opt optimCond ####

block4mod_opt <- lmer(value ~ infoCond*optionType + (1|PID),
                       data = subset(block4Ests_postExcl, optimCond == "optim"),
                       REML = FALSE)

summary(block4mod_opt)

# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                     89.902      6.773   57.580  13.273  < 2e-16 ***
#   infoCondinfo                     7.848      9.548   56.664   0.822  0.41454    
# optionTypetarget                25.221      3.297 2280.417   7.648 2.98e-14 ***
#   infoCondinfo:optionTypetarget  -12.964      4.544 2270.207  -2.853  0.00437 ** 

#### for sub optimCond ####
  
block4mod_sub <- lmer(value ~ infoCond*optionType + (1|PID),
                        data = subset(block4Ests_postExcl, optimCond == "sub"),
                        REML = FALSE)
summary(block4mod_sub)

# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                    114.958      4.453  146.774  25.817  < 2e-16 ***
#   infoCondinfo                    -9.235      6.307  147.823  -1.464   0.1452    
# optionTypetarget               -25.082      3.776 2336.106  -6.643 3.81e-11 ***
#   infoCondinfo:optionTypetarget   11.388      5.287 2331.637   2.154   0.0314 * 
  
##### get means

tmp<-subset(block4Ests_postExcl, optimCond == "sub" & optOptimality == "sub")
tmp %>% group_by(infoCond) %>% summarise(mean(value))

## now process data to be visualised ####
block4Ests_postExcl_backup <- block4Ests_postExcl

block4Ests_postExcl$optOptimality <- as.factor(block4Ests_postExcl$optOptimality)
block4Ests_postExcl$optionType <- as.factor(block4Ests_postExcl$optionType)
block4Ests_postExcl$infoCond <-relevel(block4Ests_postExcl$infoCond, ref = "info")
block4Summary_noEq <- block4Ests_postExcl %>% # subject level averaging
  filter(optimCond != "eq") %>%
  group_by(PID, condition, optOptimality, infoCond, optimCond) %>%
  summarise(avgValue = mean(value))

block4Summary_noEq <- block4Summary_noEq %>% # group level averaging
  group_by(condition, optOptimality, infoCond, optimCond) %>%
  summarise(stdev = sd(avgValue, na.rm=TRUE),
            mean = mean(avgValue, na.rm=TRUE),
            n = n(),
            stderr = stdev/sqrt(n)) %>%
  ungroup()

block4Summary_eq <- block4Ests_postExcl %>% # subject level averaging
  filter(optimCond == "eq") %>%
  group_by(PID, condition, estOption, infoCond, optimCond, optionType) %>%
  summarise(avgValue = mean(value))

block4Summary_eq <- block4Summary_eq %>% # group level averaging
  group_by(infoCond, optimCond, optionType) %>%
  summarise(stdev = sd(avgValue, na.rm=TRUE),
            mean = mean(avgValue, na.rm=TRUE),
            n = n(),
            stderr = stdev/sqrt(n)) %>%
  ungroup()

### plots ####

block4Summary_noEq$optOptimality <- factor(block4Summary_noEq$optOptimality, levels = c("sub", "optim"),
                                           labels = c("Suboptimal Option (M = 80)", "Optimal Option (M = 120)") # TO DO: think of a better label as this doesn't make sense for eq
)

block4Summary_noEq_optim <- block4Summary_noEq %>% filter(optimCond == "optim")
block4Summary_noEq_sub<- block4Summary_noEq %>% filter(optimCond == "sub")


ggplot(block4Summary_noEq_optim, aes(x=infoCond, y=mean, col = infoCond)) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1, position = position_dodge(width = 1)) +
  theme_bw() +
  coord_cartesian(ylim = c(80,130))+
  labs(title = "Target optimal", y="Mean Estimate for 'Win' Trials") +
  scale_colour_colorblind(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  scale_x_discrete(name = "Informativeness Condition", labels = c("Informative", "Non-Informative"))+
  facet_grid(~optOptimality)

#ggsave(here("plots","s2_block4est_noEq_optim.jpg"), width = 6, height = 4)

ggplot(block4Summary_noEq_sub, aes(x=infoCond, y=mean, col = infoCond)) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1, position = position_dodge(width = 1)) +
  theme_bw() +
  coord_cartesian(ylim = c(80,130))+
  labs(title = "Target suboptimal", y="Mean Estimate for 'Win' Trials") +
  scale_colour_colorblind(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  scale_x_discrete(name = "Informativeness Condition", labels = c("Informative", "Non-Informative"))+
  facet_grid(~optOptimality)

#ggsave(here("plots","s2_block4est_noEq_sub.jpg"), width = 6, height = 4)


block4Summary_eq$optionType <- factor(block4Summary_eq$optionType, levels = c("non-target", "target"),
                                      labels = c("Non-Target Option (M = 100)", "Target Option (M = 100)") # TO DO: think of a better label as this doesn't make sense for eq
)

ggplot(block4Summary_eq, aes(x=infoCond, y=mean, col = infoCond)) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1, position = position_dodge(width = 1)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black") +  # Adding the dotted line
  theme_bw() +
  coord_cartesian(ylim = c(80,120))+
  labs(title="Target & non-target equal", y="Mean Estimate for 'Win' Trials", x = "Option") +
  scale_colour_colorblind(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  scale_x_discrete(name = "Informativeness Condition", labels = c("Informative", "Non-Informative"))+
  facet_grid(~optionType)

#ggsave(here("plots","s2_block4est_eq"), width = 7, height = 4)

# Value estimates from repeated estimates ####

repeatEsts <- data %>% filter(grepl("undefined", responses) & postQ != "") %>%
  select(c(PID, infoCond, optimCond, optStim, subStim, responses, postQ))

repeatEsts$condition <- paste0(repeatEsts$infoCond, ", ", repeatEsts$optimCond)

repeatEsts$responses <- as.numeric(gsub(".*?([0-9]+).*", "\\1", repeatEsts$responses))   #extract numbers only

# Mark whether or not the estimate option was a target or non-target

choiceNameData$postQ <- choiceNameData$choiceName # use our choice name df to mark estimated option as target or not

tmp <- full_join(repeatEsts, choiceNameData, by = c("PID", "postQ")) %>%
  mutate(optionType = ifelse(choice == 1, "target", "non-target"))

repeatEsts <- tmp %>%
  select(c(PID, infoCond, optimCond, condition, postQ, optionType, optStim, subStim, responses))

## Exclusions ####

### As reported in manuscript ####
# 1) exclude extremely high estimates

#repeatExcl1_ids <- subset(repeatEsts, responses >= 1000)$PID %>% unique() # just 1 person
repeatEsts_postExcl <- repeatEsts %>% filter(responses < 1000)

(nrow(repeatEsts)-nrow(repeatEsts_postExcl))/nrow(repeatEsts)

# 2) exclude people who didn't report 0s AND non-0s for either of the 2 options?

repeatEsts_postExcl$PID <- as.factor(repeatEsts_postExcl$PID) # change data type for grouping

repeatEsts_nZero <- repeatEsts_postExcl %>% 
  group_by(PID,postQ, .drop=FALSE) %>%
  count(responses == 0) %>%  # count number of 0 responses out of 10 estimates for each postQ (option); and do same for non-0s
  arrange(PID, postQ, `responses == 0`) %>%
  summarise(count = n()) # each participant should have count "2" for each postQ, if correctly following instructions to report 0s and non-0s

# PID   postQ                    `responses == 0`     n
# <fct> <chr>                    <lgl>            <int>
# 1 42    imgNWTK/FindOutNow.jpg   FALSE               10
# 2 42    imgNWTK/KeepItSecret.jpg FALSE                5
# 3 42    imgNWTK/KeepItSecret.jpg TRUE                 5

# # THEN WHEN SUMMARISING
# PID   postQ                    count
# <fct> <chr>                    <int>
# 1 42    imgNWTK/FindOutNow.jpg       1
# 2 42    imgNWTK/KeepItSecret.jpg     2

# participants who didn't report 0s AND non-0s for one or both of the 2 options
repeatExcl2 <- subset(repeatEsts_nZero, count < 2)$PID
repeatExcl2_ids <- repeatExcl2 %>% unique()

# now pick out only the unique ones

repeatExcl2_ids <- as.numeric(as.character(repeatExcl2_ids)) # convert variable type for subsetting later

repeatEsts_postExcl <- subset(repeatEsts_postExcl, !(PID %in% repeatExcl2_ids)) # remove these participants


nonDuplicates <- sapply(repeatExcl2_ids, function(x) x < 191293823) # remember that during preprocessing, we added 191293823 to PIDs of duplicate IDs
length(repeatExcl2_ids[nonDuplicates]) # 91 participants excluded due to not reporting both 0s and non-0s

repeatEsts_postExcl_ids <- repeatEsts_postExcl$PID %>% unique() # length 303; 162 actual participants

# Uncomment the below if running analyses with strict exclusion  
### Scenario 2: Strict exclusion ####
### SI Scenario 2 (strict exclusion) ####
# write.csv(block4Ests_postExcl_strict, here("data_tidy", "s2_block4Ests_postExcl_strict.csv"), row.names = FALSE)

# # Exclude participants who gave a response they could not have observed (this differs by condition)
# 
# ## Exclude participants in target & non-target not equal who gave estimates less than 70
# ## or more than 130
# ## For target & non target equal, exclude those who gave estimates less than 90 and more than 110
# repeatEsts <- repeatEsts %>% mutate(
#   minObs = case_when(optimCond == "eq" ~ 90,
#                      .default = 70),
#   maxObs = case_when(optimCond == "eq" ~ 110,
#                      .default = 130)
# )
# 
# repeatEsts_strictExcl1 <- subset(repeatEsts, (responses<minObs & responses != 0) | (responses>maxObs & responses != 0))$PID %>% unique()
# 
# # Exclude participants did NOT give 0 responses
# # run section on repeatExcl2_ids
# 
# repeatEsts$PID <- as.factor(repeatEsts$PID) # change data type for grouping
# 
# repeatEsts_nZero <- repeatEsts %>% 
#   group_by(PID,postQ, .drop=FALSE) %>%
#   count(responses == 0) %>%  # count number of 0 responses out of 10 estimates for each postQ (option); and do same for non-0s
#   arrange(PID, postQ, `responses == 0`) %>%
#   summarise(count = n()) # each participant should have count "2" for each postQ, if correctly following instructions to report 0s and non-0s
# repeatExcl2 <- subset(repeatEsts_nZero, count < 2)$PID
# repeatExcl2_ids <- repeatExcl2 %>% unique()
# 
# # now pick out only the unique ones
# 
# repeatExcl2_ids <- as.numeric(as.character(repeatExcl2_ids)) # convert variable type for subsetting later
# 
# 
# # remove people
# 
# repeatEsts_postExcl <- repeatEsts %>% filter(!(PID %in% repeatEsts_strictExcl1) & !(PID %in% repeatExcl2_ids)) # for estimates over 1000, remove the whole subject
# 
# repeatEsts_postExcl$PID %>% unique()

## Compute relative optimality of the given option ####

repeatEsts_postExcl <- repeatEsts_postExcl %>%
  mutate(
    optOptimality = case_when(postQ == optStim ~ "optim",
                              postQ == subStim ~ "sub",
                              TRUE ~ "NA")
  )


## Plotting ####
repeatEsts_summary_noEq <- repeatEsts_postExcl %>% # subject level averaging
  filter(optimCond != "eq") %>%
  group_by(PID, infoCond, optimCond, optOptimality) %>%
  summarise(stdev = sd(responses, na.rm=TRUE),
            mean = mean(responses, na.rm=TRUE),
            n = n(),
            stderr = stdev/sqrt(n)) %>%
  ungroup()


repeatEsts_summary_noEq <- repeatEsts_summary_noEq %>% # group level averaging
  group_by(infoCond, optimCond, optOptimality) %>%
  summarise(stdev = sd(mean, na.rm=TRUE),
            mean = mean(mean, na.rm=TRUE),
            n = n(),
            stderr = stdev/sqrt(n)) %>%
  ungroup()


repeatEsts_summary_eq <- repeatEsts_postExcl %>% # subject level averaging
  filter(optimCond == "eq") %>%
  group_by(PID, infoCond, optimCond, optOptimality, optionType) %>%
  summarise(stdev = sd(responses, na.rm=TRUE),
            mean = mean(responses, na.rm=TRUE),
            n = n(),
            stderr = stdev/sqrt(n)) %>%
  ungroup()

repeatEsts_summary_eq <- repeatEsts_summary_eq %>% # group level averaging
  group_by(infoCond, optimCond, optOptimality, optionType) %>%
  summarise(stdev = sd(mean, na.rm=TRUE),
            mean = mean(mean, na.rm=TRUE),
            n = n(),
            stderr = stdev/sqrt(n)) %>%
  ungroup()

repeatEsts_summary_noEq$optOptimality <- factor(repeatEsts_summary_noEq$optOptimality, levels = c("sub", "optim"),
                                                labels = c("Suboptimal Option (M = 40)", "Optimal Option (M = 60)") 
)

repeatEsts_summary_eq$optionType <- factor(repeatEsts_summary_eq$optionType, levels = c("non-target", "target"),
                                           labels = c("Non-Target Option (M = 50)", "Target Option (M = 50)") 
)


repeatEsts_summary_noEq_opt <- repeatEsts_summary_noEq %>% filter(optimCond == "optim")
repeatEsts_summary_noEq_sub <- repeatEsts_summary_noEq %>% filter(optimCond == "sub")

ggplot(repeatEsts_summary_noEq_opt, aes(x=infoCond, y=mean, col = infoCond)) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1, position = position_dodge(width = 1)) +
  coord_cartesian(ylim = c(35,65))+
  theme_bw() +
  labs(title = "Target optimal", y="Mean Estimate (Including Both 'Win' and 'No-Win' Trials)") +
  scale_colour_colorblind(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  scale_x_discrete(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  facet_grid(~optOptimality) 

#ggsave(here("plots","s2_repeatest_noeq_opt_strictExcl.jpg"), width = 6, height = 4)


ggplot(repeatEsts_summary_noEq_sub, aes(x=infoCond, y=mean, col = infoCond)) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1, position = position_dodge(width = 1)) +
  coord_cartesian(ylim = c(35,65))+
  theme_bw() +
  labs(title = "Target suboptimal", y="Mean Estimate (Including Both 'Win' and 'No-Win' Trials)") +
  scale_colour_colorblind(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  scale_x_discrete(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  facet_grid(~optOptimality) 

#ggsave(here("plots","s2_repeatest_noeq_sub.jpg"), width = 6, height = 4)


ggplot(repeatEsts_summary_eq, aes(x=infoCond, y=mean, col = infoCond)) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1, position = position_dodge(width = 1)) +
  coord_cartesian(ylim = c(40,60))+
  theme_bw() +
  labs(title="Target & non-target equal", y="Mean Estimate (Including Both 'Win' and 'No-Win' Trials)") +
  scale_x_discrete(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  scale_colour_colorblind(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  facet_grid(~optionType) 

#ggsave(here("plots","s2_repeatest_eq.jpg"), width = 7, height = 4)

## Analyse ####

### for eq optimCond ####

repeatEsts_inds_eq <- repeatEsts_postExcl %>% 
  filter(optimCond == "eq") 

repeatedEsts_eq <- lmer(responses ~ optionType*infoCond + (1|PID), data = repeatEsts_inds_eq, REML = FALSE)

summary(repeatedEsts_eq)

### for optim optimCond ####
repeatEsts_inds_optim <- repeatEsts_postExcl %>% 
  filter(optimCond == "optim") 

repeatedEsts_optim <- lmer(responses ~ optionType*infoCond + (1|PID), data = repeatEsts_inds_optim, REML = FALSE)

summary(repeatedEsts_optim)

#### means ####

### for sub optimCond ####
repeatEsts_inds_sub <- repeatEsts_postExcl %>% 
  filter(optimCond == "sub") 

repeatedEsts_sub <- lmer(responses ~ optionType*infoCond + (1|PID), data = repeatEsts_inds_sub, REML = FALSE)

summary(repeatedEsts_sub)

# Assessing relationship between people's estimates and their choices in the task ####

## Relationship between block 4 estimates and target choices ####

## first compute the means for each participant
block4Ests_subj_means <- block4Ests_postExcl %>%
  group_by(PID, condition, optionType) %>%
  summarise(meanEstimate = mean(as.numeric(value)))

## compute the difference in estimates for target vs non-target
block4Ests_difference <- pivot_wider(block4Ests_subj_means, values_from = meanEstimate, names_from = optionType) %>%
  mutate(difference = target - `non-target`)

## calculate mean preference for FON/target
propFON_avg_PID <- propFON_blocks %>%
  group_by(PID) %>%
  summarise(P_FON = mean(percent_FON))

block4ests_choiceFON <- merge(block4Ests_difference, propFON_avg_PID)

# note that here, the data has two copies of the noninformative, unequal participants due to
# the dummy participants (identifiable via their IDs over 191293823) who we added in for
# between-group comparisons!
# remove them from this analysis
block4ests_choiceFON <- block4ests_choiceFON %>% filter(PID < 191293823)

### Main paper plot ####

block4_choice_plot_data <- block4ests_choiceFON %>%
  rename(P_Target = P_FON)


block4ets_choice_plot <- ggplot(block4_choice_plot_data, aes(x = difference, y = P_Target, col = condition))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()+
  labs(x = "Target Estimate - NonTarget Estimate", y = "Target Choices %")+
  theme(axis.title = element_text(size = 18, face = "bold"), legend.title = element_blank()); block4ets_choice_plot

block4ets_choice_plot

##lm
block4ests_choiceFON$condition <- as.factor(block4ests_choiceFON$condition)
block4ests_choiceFON$condition<-relevel(block4ests_choiceFON$condition, ref = 'noninfo, eq') # make non-info eq the reference

tmp_lm_block4 <- lm(P_FON ~ difference + 1, block4ests_choiceFON)
summary(tmp_lm_block4)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 53.51233    1.01642  52.648  < 2e-16 ***
#   difference   0.14641    0.03224   4.542 8.69e-06 ***
  

## Relationship between repeated post-test estimates and target choices ####

# compute subject means from posttest estimates for each of the two items
repeatEsts_subj_means <- repeatEsts_postExcl %>%
  group_by(PID, condition, optionType) %>%
  summarise(estimate_mean = mean(responses)) %>%
  filter(!is.na(optionType))

# compute difference in estimates for target vs nontarget for each participant
repeatEsts_avg_difference <- pivot_wider(repeatEsts_subj_means, values_from = estimate_mean, names_from = optionType) %>%
  mutate(difference = `target` - `non-target`) 

# now merge into same dataframe as popFON_avg_PID which contains info about prop target choices during task
repeatEsts_choice <- merge(repeatEsts_avg_difference, propFON_avg_PID) %>%
  filter(!is.na(difference))

repeatEsts_choice$PID<-as.numeric((as.character(repeatEsts_choice$PID)))

# note that here, the data has two copies of the noninformative, unequal participants due to
# the dummy participants (identifiable via their IDs over 191293823) who we added in for
# between-group comparisons!
# remove them from this analysis
repeatEsts_choice <- repeatEsts_choice %>% filter(PID < 191293823)

## plot
repeatEsts_choice$PID <- as.factor(as.character(repeatEsts_choice$PID))
repeatEsts_choice$condition <- as.factor(repeatEsts_choice$condition)
repeatEsts_choice$condition<-relevel(repeatEsts_choice$condition, ref = 'noninfo, eq') # make non-info eq the reference

repeatEsts_choice_plot <- ggplot(repeatEsts_choice, aes(x = difference, y = P_FON, col = condition))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()+
  labs(x = "Target Estimate - NonTarget Estimate", y = "Target Choices %")+
  theme(axis.title = element_text(size = 18, face = "bold"), legend.title = element_blank()); repeatEsts_choice_plot

repeatEsts_choice_plot

##lm

repeatEsts_lm <- lm(P_FON ~ difference + 1, data= repeatEsts_choice)
repeatEsts_choice$PID 

summary(repeatEsts_lm)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  51.9749     1.3092   39.70  < 2e-16 ***
#   difference    0.2191     0.0524    4.18 4.78e-05 ***

# Supps: Value estimates from final estimates ####

singleEst <- data %>% filter(singleEst != "")

singleEst <- singleEst %>%
  mutate(
    optOptimality = case_when(singleEst == optStim ~ "optim",
                              singleEst == subStim ~ "sub",
                              TRUE ~ "NA")
  ) %>%
  select(c(PID, infoCond, optimCond, condition, optStim, subStim, responses, singleEst, optOptimality))

singleEst$responses <- as.numeric(gsub(".*?([0-9]+).*", "\\1", singleEst$responses))   #extract numbers only

# Mark whether or not the estimate option was a target or non-target

choiceNameData$singleEst <- choiceNameData$choiceName # use our choice name df to mark estimated option as target or not

tmp <- full_join(singleEst, choiceNameData, by = c("PID", "singleEst")) %>%
  mutate(optionType = ifelse(choice == 1, "target", "non-target"))

singleEst <- tmp %>%
  select(c(PID, infoCond, optimCond, condition, singleEst, optionType, optOptimality, responses))


## Exclude people who got estimates over 1000

singleEstExcl <- subset(singleEst, responses < 1000 & responses > 0 )

singleEstExcl_unique <- singleEstExcl %>% filter(PID < 191293823)
singleEst_unique <- singleEst %>% filter(PID < 191293823)

1- nrow(singleEstExcl_unique)/nrow(singleEst_unique)

# note that PIDs with values under 191293823 are the number of ACTUAL unique participants

singleEst_summary_noEq <- singleEstExcl %>%
  filter(optimCond != "eq") %>%
  group_by(infoCond, optimCond, optOptimality) %>%
  summarise(stdev = sd(responses, na.rm=TRUE),
            mean = mean(responses, na.rm=TRUE),
            n = n(),
            stderr = stdev/sqrt(n)) %>%
  ungroup()

singleEst_summary_eq <- singleEstExcl %>%
  filter(optimCond == "eq") %>%
  group_by(infoCond, optimCond, optionType, optOptimality) %>%
  summarise(stdev = sd(responses, na.rm=TRUE),
            mean = mean(responses, na.rm=TRUE),
            n = n(),
            stderr = stdev/sqrt(n)) %>%
  ungroup()

singleEst_summary_noEq$optOptimality <- factor(singleEst_summary_noEq$optOptimality, levels = c("sub", "optim"),
                                               labels = c("Suboptimal Option (M = 80)", "Optimal Option (M = 120)") 
)


singleEst_summary_eq$optionType <- factor(singleEst_summary_eq$optionType, levels = c("non-target", "target"),
                                          labels = c("Non-Target Option (M = 100)", "Target Option (M = 100)") 
)

ggplot(singleEst_summary_noEq, aes(x=optimCond, y=mean, col = infoCond)) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1, position = position_dodge(width = 1)) +
  theme_bw() +
  labs(y="Single Estimate for 'Win' Trials") +
  scale_colour_colorblind(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  scale_x_discrete(name = "Optimality Condition", labels = c("Target Optimal", "Target Suboptimal")) +
  facet_grid(~optOptimality)

#ggsave(here("plots","s2_finalEst_noeq.jpg"), width = 7, height = 3.4)

ggplot(singleEst_summary_eq, aes(x=optimCond, y=mean, col = infoCond)) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1, position = position_dodge(width = 1)) +
  coord_cartesian(ylim=c(80,120)) +
  theme_bw() +
  labs(y="Final Estimate for 'Win' Trials") +
  scale_x_discrete(name = "Optimality Condition", labels = c("Target & Non-Target Equal")) +
  scale_colour_colorblind(name = "Informativeness Condition", labels = c("Informative", "Non-Informative")) +
  facet_grid(~optionType)

#ggsave(here("plots","s2_finalEst_eq.jpg"), width = 7, height = 3.4)

## Analysis ####

### for eq optimCond
singleEst_eq <- singleEst %>% filter(optimCond != "eq")

singleEst_eq_mod <- lmer(responses ~ infoCond*optionType + (1|PID), data = singleEst_eq)
summary(singleEst_eq_mod)

### for optim optimCond
singleEst_optim <- singleEst %>% filter(optimCond == "optim")

t.test(responses ~ infoCond, var.equal = TRUE, data = subset(singleEst_optim, optOptimality == "sub")) # for the worse option
t.test(responses ~ infoCond, var.equal = TRUE, data = subset(singleEst_optim, optOptimality == "optim")) # for the better option

### for sub optimCond
singleEst_sub <- singleEst %>% filter(optimCond == "sub")

t.test(responses ~ infoCond, var.equal = TRUE, data = subset(singleEst_sub, optOptimality == "sub")) # for the worse option
t.test(responses ~ infoCond, var.equal = TRUE, data = subset(singleEst_sub, optOptimality == "optim")) # for the better option

#################################
## Win-Stay Lose-Shift Behav ####
#################################
library(lme4)

exp2_trialdat_wsls <- trialdat %>%
  mutate(switch = ifelse(banditTrial == 1, "NA",
                         ifelse(choice == lag(choice), 0, 1))) %>%
  mutate(prevFeedback = ifelse(banditTrial == 1, "NA", lag(feedback)))


exp2_trialdat_wsls2 <- exp2_trialdat_wsls %>%
  filter(switch != "NA") %>%
  select(PID, condition, infoCond, optimCond, banditTrial, choice, feedback, switch, prevFeedback)


exp2_trialdat_wsls2$switch <- as.numeric(exp2_trialdat_wsls2$switch)
exp2_trialdat_wsls2$prevFeedback <- as.numeric(exp2_trialdat_wsls2$prevFeedback)
exp2_trialdat_wsls2$PID <- as.numeric(exp2_trialdat_wsls2$PID)

## datatset which does contain duplicates

exp2_trialdat_wsls2_dupes <- exp2_trialdat_wsls2

## removing the duplicates
exp2_trialdat_wsls2 <- exp2_trialdat_wsls2 %>%
  filter(PID < 191293823)

length(unique(exp2_trialdat_wsls2$PID))

exp2_trialdat_wsls2$PID <- as.factor(exp2_trialdat_wsls2$PID)


#### collapsing across optimality ####

exp2_wsls_feedback_all <- glmer(switch ~ 1 + prevFeedback*infoCond + (1|PID),
                                family = binomial(link = "logit"),
                                data = exp2_trialdat_wsls2)

summary(exp2_wsls_feedback_all)

# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -0.1939393  0.0558925  -3.470 0.000521 ***
# prevFeedback                 -0.0016621  0.0004292  -3.873 0.000108 ***
# infoCondnoninfo               0.0040538  0.0786047   0.052 0.958870    
# prevFeedback:infoCondnoninfo -0.0004800  0.0006028  -0.796 0.425905    

#### old school WSLS model #####

##### finding out the parms and putting them in a dataframe for each individual

## turning data into an array
exp2_trialdat_wsls2$PID <- as.factor(exp2_trialdat_wsls2$PID)
levels(exp2_trialdat_wsls2$PID) <- 1:length(unique(exp2_trialdat_wsls2$PID))
exp2_trialdat_wsls2$PID <- as.numeric(exp2_trialdat_wsls2$PID)

exp2_trialdat_wsls_array <- array(0, dim = c(59, ncol(exp2_trialdat_wsls2), length(unique(exp2_trialdat_wsls2$PID))))

for(i in 1:length(unique(exp2_trialdat_wsls2$PID))){
  for(j in 1:59){
    for(k in 1:ncol(exp2_trialdat_wsls2)){
      exp2_trialdat_wsls_array[j,k,i] <- exp2_trialdat_wsls2[exp2_trialdat_wsls2$PID == i & exp2_trialdat_wsls2$banditTrial == j +1, k]
    }
  }
}

tmp <- colnames(exp2_trialdat_wsls2)

colnames(exp2_trialdat_wsls_array) <- tmp

#######

exp2_parmDeterminer <- data.frame(PID = 1:length(unique(exp2_trialdat_wsls2$PID)),
                                  p_winstay = 0,
                                  p_loseshift = 0)


## col 8 switch (where switch == 1)
## col 9 is prevfeedback

for(j in 1:length(unique(exp2_trialdat_wsls2$PID))){
  switchCount_lose <- 0
  stayCount_win <- 0
  loseTotalCounter <- 0
  winTotalCounter <- 0
  for(i in 1:59) {
    if(exp2_trialdat_wsls_array[i, "prevFeedback", j] == 0){
      loseTotalCounter <- loseTotalCounter + 1
      if(exp2_trialdat_wsls_array[i, "switch", j] == 1) {
        switchCount_lose <- switchCount_lose + 1
      }
    } else if(exp2_trialdat_wsls_array[i, "prevFeedback", j] > 0) {
      winTotalCounter <- winTotalCounter + 1
      if(exp2_trialdat_wsls_array[i, "switch", j] == 0) {
        stayCount_win <- stayCount_win + 1
      }
    }
  }
  exp2_parmDeterminer[j,3] <-  switchCount_lose/loseTotalCounter
  exp2_parmDeterminer[j,2] <-  stayCount_win/winTotalCounter
}


mean(exp2_parmDeterminer[,2])
mean(exp2_parmDeterminer[,3])


### tells which condition they were in
PID_infoCond <- exp2_trialdat_wsls2 %>%
  filter(banditTrial == 2 & infoCond == "info") %>%
  select(PID) %>%
  ungroup() %>% unlist() %>% unname()


## adding a col to parmDeterminer

exp2_parmDeterminer <- as.data.frame(exp2_parmDeterminer)

exp2_parmDeterminer2 <- exp2_parmDeterminer %>%
  mutate(condition = ifelse(PID %in% PID_infoCond, "Info", "NonInfo"))

parmSummary2 <- exp2_parmDeterminer2 %>%
  group_by(condition) %>%
  summarise(mean_winstay = mean(p_winstay), sd_winstay = sd(p_winstay),
            mean_loseshift = mean(p_loseshift), sd_loseshift = sd(p_loseshift))



#########################################
### Reanalysing the equal condition #####
###          R1 suggestion.         #####
#########################################

exp2_eq_rerandomise <- trialdat %>%
  filter(optimCond == "eq" & infoCond == "noninfo")

exp2_eq_rerandomise_infoCond <- trialdat %>%
  filter(optimCond == "eq" & infoCond == "info")

PID_list <- as.data.frame(unique(exp2_eq_rerandomise$PID))

PID_list$target <- NA

colnames(PID_list) <- c("PID", "target")

for(i in 1:nrow(PID_list)){
  PID_list[i,2] <-   sample(c("triangle", "square"), 1)
}

exp2_eq_rerandomise_done <- merge(exp2_eq_rerandomise,PID_list)
exp2_eq_rerandomise_done$target <- as.character(exp2_eq_rerandomise_done$target)
exp2_eq_rerandomise_done$choiceName <- as.character(exp2_eq_rerandomise_done$choiceName)

exp2_eq_rerandomise_done <- exp2_eq_rerandomise_done %>%
  mutate(choice = ifelse(str_detect(choiceName, target), 1, 0))

exp2_eq_rerandomise_done_dropTarget <- exp2_eq_rerandomise_done %>%
  select(-target)

exp2_eq_rerandomise_all <- rbind(exp2_eq_rerandomise_infoCond, exp2_eq_rerandomise_done_dropTarget)

#### the below is the same as the analyses conducted above
## Analyse ####

## relevel for analysis

exp2_eq_rerandomise_all$infoCond <- as.factor(exp2_eq_rerandomise_all$infoCond)

redo_mod_eq <- glmer(choice ~ infoCond+banditTrial + (1|PID),
                     data = exp2_eq_rerandomise_all,
                     family = binomial(link = "logit"),
                     nAGQ = 0) 

redo_mod_eq_full <- glmer(choice ~ infoCond*banditTrial + (1|PID),
                          data = exp2_eq_rerandomise_all,
                          family = binomial(link = "logit"),
                          nAGQ = 0) 

summary(redo_mod_eq) 

anova(redo_mod_eq, redo_mod_eq_full)

confint(pairs(emmeans(redo_mod_eq, ~ infoCond,type="response")))

###############
## Checking only those who showed a preference for the optimal option -- as suggested by R2

optimPrefOnly_PIDs <- trialdatOptim %>%
  group_by(PID) %>%
  summarise(meanPref = mean(as.numeric(choice)) - 1) %>%
  filter(meanPref > .6) %>%
  select(PID) %>% ungroup() %>% unlist() %>% unname()

optimPrefOnly_trialdatOptim <- trialdatOptim %>%
  filter(PID %in% optimPrefOnly_PIDs)

## summary for plotting
optimPrefOnly_trialdatOptim$choice <- as.numeric(optimPrefOnly_trialdatOptim$choice) - 1

tmp_summary <- optimPrefOnly_trialdatOptim %>%
  group_by(infoCond, block) %>%
  summarise(meanPref = mean(as.numeric(choice)), se = sd(choice)/sqrt(length(choice) - 1))



ggplot(tmp_summary, aes(x = block, y = meanPref, col = infoCond, group = infoCond))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = meanPref - se, ymax = meanPref + se))+
  theme_bw()


length(unique(optimPrefOnly_trialdatOptim$PID))

## analysis
optimPrefOnly_trialdatOptim$choice <- as.factor(optimPrefOnly_trialdatOptim$choice)
optimPrefOnly_trialdatOptim$PID <- as.factor(optimPrefOnly_trialdatOptim$PID)


tmp <- glmer(choice ~ infoCond + (1|PID), 
             family = binomial(link = "logit"),
             nAGQ = 0,
             data = optimPrefOnly_trialdatOptim)

summary(tmp)
