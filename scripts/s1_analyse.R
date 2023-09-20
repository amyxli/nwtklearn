# learning NWTK Exp 1
# created axl 7/9/22
# last updated axl 23/8/23

# load the libraries
library(tidyverse)
library(here)
library(data.table)
library(ggthemes)
library(emmeans)

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

mod <- glmer(choice ~ condition+banditTrial + (1|PID),
             data = trialdat,
             family = binomial(link = "logit"))

summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: choice ~ condition + banditTrial + (1 | PID)
# Data: trialdat
# 
# AIC      BIC   logLik deviance df.resid 
# 6878.7   6905.5  -3435.3   6870.7     5936 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -5.7342 -0.7329 -0.3387  0.8687  3.3408 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# PID    (Intercept) 1.689    1.3     
# Number of obs: 5940, groups:  PID, 99
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       0.488224   0.202840   2.407   0.0161 *  
#   conditionNonInfo -0.562584   0.269865  -2.085   0.0371 *  
#   banditTrial      -0.015523   0.001734  -8.950   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) cndtNI
# condtnNnInf -0.702       
# banditTrial -0.260  0.006

confint(pairs(emmeans(mod, ~ condition,type="response")))

# contrast              odds.ratio    SE  df asymp.LCL asymp.UCL
# Informative / NonInfo       1.76 0.474 Inf      1.03      2.98
# 
# Confidence level used: 0.95
# Intervals are back-transformed from the log odds ratio scale

# is there an effect of an interaction?
mod2 <- glmer(choice ~ condition*banditTrial + (1|PID),
              data = trialdat,
              family = binomial(link = "logit"))

anova(mod,mod2,test="Chisq")

# > anova(mod,mod2,test="Chisq")
# Data: trialdat
# Models:
#   mod: choice ~ condition + banditTrial + (1 | PID)
# mod2: choice ~ condition * banditTrial + (1 | PID)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# mod     4 6878.7 6905.5 -3435.3   6870.7                     
# mod2    5 6880.5 6913.9 -3435.3   6870.5 0.1971  1      0.657

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
qqPlot(estimates$responses) # qqPlot not looking too good... apply a log transform

est_transformed <- log(estimates$responses)
#est_transformed <- log(estimates$responses + 0.000000000000000001) # uncomment this line & comment out previous line if running without exclusion criteria
# this is because zeros are not excluded when no exclusion criteria are applied

qqPlot(est_transformed) # looks better

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
