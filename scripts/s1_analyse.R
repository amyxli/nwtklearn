# learning NWTK Exp 1
# created axl 7/9/22
# last updated axl 12/12/23

# load the libraries
library(tidyverse)
library(here)
library(data.table)
library(ggthemes)
library(emmeans)
library(lmtest)

# load the data
data <- read.csv(here("data_tidy", "s1_data_cleaned.csv"))
data$choice <- as.factor(data$choice)

# Save trial Data 
trialdat <- data %>% filter(!is.na(banditTrial)) %>%
  select(c(PID, banditTrial, choice, cue, feedback, magnit)) %>%
  rename(condition = magnit) %>%
  arrange(PID, banditTrial)

# Overall KIS vs FON choices: proportions ####

# cue: 1 is KIS, 2 is FON

trialdat <- trialdat %>%
  mutate(block = case_when(
    banditTrial <= 15 ~ 1,
    banditTrial <= 30 ~ 2,
    banditTrial <= 45 ~ 3,
    TRUE ~ 4
  ))

## visualise prop FON over blocks

trialdat$block <- as.factor(trialdat$block)

glimpse(trialdat)

#write.csv(trialdat, here("data_tidy", "s1_trialdat.csv"), row.names = FALSE)

# Overall FON percentage across blocks ####

propFON_blocks <- trialdat %>%
  group_by(PID, block, choice, .drop=FALSE) %>%
  summarise(n_FON = n()) %>% # chose FON
  filter(choice == 1)

conditionInfo <- trialdat %>% select(c(PID, condition)) %>% unique()

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

ggplot(propFON_summary,
       aes(block, mean, group = condition, col = condition)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin=mean - stderr, ymax = mean + stderr), width = .1) +
  theme_bw() +
  labs(x="Block", y="Mean % Suboptimal Choices", col = "Condition") +
  scale_colour_colorblind(labels = c("Informative", "Non-Informative")) 

#ggsave(here("plots","s1_FON_blocks.jpg"), width = 6, height = 4.2)

# Predictors of KIS/FON choice: analyse using glmer ####
library(lme4)

trialdat$condition <- as.factor(trialdat$condition)

trialdat$condition <-relevel(trialdat$condition, ref = "NonInfo")

choice_mod_0 <- glmer(choice ~ (1|PID),
                      data = trialdat,
                      family = binomial(link = "logit"))
choice_mod_1 <- glmer(choice ~ condition+banditTrial + (1|PID),
             data = trialdat,
             family = binomial(link = "logit"))
choice_mod_2 <- glmer(choice ~ condition*banditTrial + (1|PID),
                      data = trialdat,
                      family = binomial(link = "logit"))

lrtest(choice_mod_0, choice_mod_1, choice_mod_2)

# Likelihood ratio test
# 
# Model 1: choice ~ (1 | PID)
# Model 2: choice ~ condition + banditTrial + (1 | PID)
# Model 3: choice ~ condition * banditTrial + (1 | PID)
# #Df  LogLik Df   Chisq Pr(>Chisq)    
# 1   2 -3477.9                          
# 2   4 -3435.3  2 85.1368     <2e-16 ***
# 3   5 -3435.3  1  0.1971      0.657    

summary(choice_mod_1)
confint(pairs(emmeans(choice_mod_1, ~ condition,type="response"))) 
# use 1/OR from the confint output to compute info/noninfo ratio, and 1/LCL and 1/UCL to compute CIs for that OR

# Single value estimate at end of task ####

estimates <- data %>% 
  dplyr::filter(grepl("undefined", responses)) %>%
  dplyr::filter(trial_index != 3) %>% # not the age question
  dplyr::filter(!(grepl("A", responses))) # not MTURK IDs

estimates <- estimates %>% 
  dplyr::select(PID, trial_index, condition, responses) %>%
  mutate(condition = ifelse(condition == 0, "info", "noninfo"))

estimates$responses <- as.numeric(gsub(".*?([0-9]+).*", "\\1", estimates$responses))   #extract numbers only

# exclude if people gave improbable estimates 
exclude_IDs <- subset(estimates, responses== 0 | responses >= 1000)$PID %>% unique()# participant IDs to exclude based on criteria
#Comment the above line, and uncomment the below line, to run the analyses with strict exclusion criteria; comment both lines to run analyses without any exclusion criteria
#exclude_IDs <- subset(estimates, responses < 70 | responses > 130)$PID %>% unique() # strict exclusion criteria

## results in exclusion of 19 people

estimates <- subset(estimates, !(PID %in% exclude_IDs)) # filter these out of the dataset. left with 138 estimates (ie 69 participants)
#Comment out the above line to run analyses without any exclusion criteria

estimates$PID %>% unique() # n=80 # n=43 with strict exclusions

# estimates are always suboptimal, optimal
estimates <-estimates %>% arrange(PID,trial_index) # make sure data is chronological
estimates$actual <- rep(c(80,120), length.out = nrow(estimates)) # make actual column with repeating 80, 120, 80, 120 etc going down
estimates$item <- rep(c("sub","optim"), length.out = nrow(estimates)) # make actual column with repeating 80, 120, 80, 120 etc going down


estimates <- estimates %>% select(c(PID, condition, item, responses, actual))

## plot

estimates$item <- factor(estimates$item, levels = c("sub", "optim"),
                         labels = c("Suboptimal Option (M = 80)", "Optimal Option (M = 120)")
)


estimates_summary <- estimates %>%
  group_by(condition, item) %>%
  summarise(mean = mean(responses),
            stdev = sd(responses),
            n = n(),
            stderr = stdev/sqrt(n()))

ggplot(estimates_summary,
       aes(condition, mean, colour = condition, group = condition)) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin = mean - stderr, ymax = mean + stderr), data = estimates_summary, width=.25) +
  theme_bw() +
  facet_wrap(~item) +
  scale_x_discrete(labels = c("Informative", "Non-Informative")) +
  labs(x="Condition", y="Estimated Value") +
  scale_colour_colorblind(labels = c("Informative", "Non-Informative"))

#ggsave(here("plots","s1_estimates_plot.jpg"), width = 5.4, height = 3.8)

## Analyse post-test estimates with mixed 2x2 anova (option type (within subjects) by condition (between-subjects)) ####

library(rstatix) # for anova

## check normality assumption
car::qqPlot(estimates$responses) # qqPlot not looking too good... apply a log transform

est_transformed <- log(estimates$responses)
#est_transformed <- log(estimates$responses + 0.000000000000000001) # uncomment this line & comment out previous line if running without exclusion criteria
# this is because zeros are not excluded when no exclusion criteria are applied

car::qqPlot(est_transformed) # looks better

estimates$est_transformed <- est_transformed

# Run the anova

res.aov <- anova_test(
  data = estimates, dv = est_transformed, wid = PID,
  between = condition, within = item
)
get_anova_table(res.aov)

estimates_raw_summary <- estimates %>% group_by(condition, item) %>% get_summary_stats(responses)
estimates_raw_summary

### ANOVA results with exclusions reported in manuscript ####
# ANOVA Table (type III tests)
# 
#       Effect DFn DFd      F     p p<.05      ges
# 1      condition   1  78  0.463 0.498       0.005000
# 2           item   1  78 10.293 0.002     * 0.022000
# 3 condition:item   1  78  0.404 0.527       0.000901

### ANOVA results without exclusions ####
# ANOVA Table (type III tests)
# 
#           Effect DFn DFd     F     p p<.05   ges
# 1      condition   1  97 0.978 0.325       0.005
# 2           item   1  97 1.770 0.186       0.009
# 3 condition:item   1  97 2.090 0.152       0.010

### ANOVA results with strict exclusions ####
# ANOVA Table (type III tests)
# 
#           Effect DFn DFd       F        p p<.05      ges
# 1      condition   1  41   2.111 1.54e-01       0.028000
# 2           item   1  41 102.350 1.04e-12     * 0.523000
# 3 condition:item   1  41   0.046 8.30e-01       0.000497


######################################
###### Win Stay, Lose Shift ##########
######    R1 Suggestion     ##########
######################################


# use a mixed model to assess 

trialdat_wsls <- trialdat %>%
  mutate(switch = ifelse(banditTrial == 1, "NA",
                         ifelse(choice == lag(choice), 0, 1))) %>%
  mutate(prevFeedback = ifelse(banditTrial == 1, "NA", lag(feedback)))


trialdat_wsls2 <- trialdat_wsls %>%
  filter(switch != "NA")

trialdat_wsls2$switch <- as.numeric(trialdat_wsls2$switch)
trialdat_wsls2$prevFeedback <- as.numeric(trialdat_wsls2$prevFeedback)
trialdat_wsls2$PID <- as.factor(trialdat_wsls2$PID)



## baseline model

wsls_baseline <- glmer(switch ~ 1 + (1|PID),
                       family = binomial(link = "logit"),
                       data = trialdat_wsls2,
                       nAGQ = 0)

## feedback model

wsls_feedback <- glmer(switch ~ 1 + prevFeedback + (1|PID),
                       family = binomial(link = "logit"),
                       data = trialdat_wsls2,
                       nAGQ = 0)


## feedback + condition model

wsls_feedback_cond <- glmer(switch ~ condition + prevFeedback + (1|PID),
                            family = binomial(link = "logit"),
                            data = trialdat_wsls2,
                            nAGQ = 0)

## interaction model

wsls_interaction <- glmer(switch ~ condition*prevFeedback + (1|PID),
                          family = binomial(link = "logit"),
                          data = trialdat_wsls2,
                          nAGQ = 0)

summary(wsls_interaction)

# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                       -5.153e-01  1.517e-01  -3.398 0.000679 ***
# conditionInformative              -7.368e-02  2.199e-01  -0.335 0.737624    
# prevFeedback                      -4.189e-03  7.646e-04  -5.479 4.28e-08 ***
# conditionInformative:prevFeedback -7.734e-05  1.140e-03  -0.068 0.945903    

# > exp(-4.189e-03)
# [1] 0.9958198

anova(wsls_baseline, wsls_feedback, wsls_feedback_cond, wsls_interaction)

# Models:
# wsls_baseline: switch ~ 1 + (1 | PID)
# wsls_feedback: switch ~ 1 + prevFeedback + (1 | PID)
# wsls_feedback_cond: switch ~ condition + prevFeedback + (1 | PID)
# wsls_interaction: switch ~ condition * prevFeedback + (1 | PID)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# wsls_baseline         2 6911.0 6924.4 -3453.5   6907.0                          
# wsls_feedback         3 6856.1 6876.1 -3425.0   6850.1 56.9665  1  4.433e-14 ***
# wsls_feedback_cond    4 6857.9 6884.6 -3425.0   6849.9  0.1363  1     0.7120    
# wsls_interaction      5 6859.9 6893.3 -3425.0   6849.9  0.0046  1     0.9462   

###### (feedback is bimodally distributed in a weird way) ############

# old school WSLS model #####

##### finding out the parms and putting them in a dataframe for each individual

## turning data into an array
trialdat_wsls2$PID <- as.factor(trialdat_wsls2$PID)
levels(trialdat_wsls2$PID) <- 1:length(unique(trialdat_wsls2$PID))
trialdat_wsls2$PID <- as.numeric(trialdat_wsls2$PID)

trialdat_wsls_array <- array(0, dim = c(59, ncol(trialdat_wsls2), length(unique(trialdat_wsls2$PID))))

for(i in 1:length(unique(trialdat_wsls2$PID))){
  for(j in 1:59){
    for(k in 1:ncol(trialdat_wsls2)){
      trialdat_wsls_array[j,k,i] <- trialdat_wsls2[trialdat_wsls2$PID == i & trialdat_wsls2$banditTrial == j +1, k]
    }
  }
}

trialdat_wsls_array[,,5]

#######

parmDeterminer <- data.frame(PID = 1:99,
                             p_winstay = 0,
                             p_loseshift = 0)


## col 8 switch (where switch == 1)
## col 9 is prevfeedback

for(j in 1:99){
  switchCount_lose <- 0
  stayCount_win <- 0
  loseTotalCounter <- 0
  winTotalCounter <- 0
  for(i in 1:59) {
    if(trialdat_wsls_array[i, 9, j] == 0){
      loseTotalCounter <- loseTotalCounter + 1
      if(trialdat_wsls_array[i, 8, j] == 1) {
        switchCount_lose <- switchCount_lose + 1
      }
    } else if(trialdat_wsls_array[i, 9, j] > 0) {
      winTotalCounter <- winTotalCounter + 1
      if(trialdat_wsls_array[i, 8, j] == 0) {
        stayCount_win <- stayCount_win + 1
      }
    }
  }
  parmDeterminer[j,3] <-  switchCount_lose/loseTotalCounter
  parmDeterminer[j,2] <-  stayCount_win/winTotalCounter
}


mean(parmDeterminer[,2])
mean(parmDeterminer[,3])


### tells which condition they were in
PID_infoCond<- trialdat_wsls2 %>%
  filter(banditTrial == 2 & condition == "Informative") %>%
  select(PID) %>%
  ungroup() %>% unlist() %>% unname()

PID_nonInfoCond<- trialdat_wsls2 %>%
  filter(banditTrial == 2 & condition == "NonInfo") %>%
  select(PID) %>%
  ungroup() %>% unlist() %>% unname()

## adding a col to parmDeterminer

parmDeterminer <- as.data.frame(parmDeterminer)

parmDeterminer2 <- parmDeterminer %>%
  mutate(condition = ifelse(PID %in% PID_infoCond, "Info", "NonInfo"))

parmSummary <- parmDeterminer2 %>%
  group_by(condition) %>%
  summarise(mean_winstay = mean(p_winstay), sd_winstay = sd(p_winstay),
            mean_loseshift = mean(p_loseshift), sd_loseshift = sd(p_loseshift))

# > parmSummary
# # A tibble: 2 × 5
# condition mean_winstay sd_winstay mean_loseshift sd_loseshift
# <chr>            <dbl>      <dbl>          <dbl>        <dbl>
# 1 Info             0.697      0.226          0.381        0.203
# 2 NonInfo          0.677      0.210          0.395        0.221
