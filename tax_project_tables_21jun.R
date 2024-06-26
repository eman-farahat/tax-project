library(dplyr)
library(broom)
library(lmtest)
library(sandwich)
library(stargazer)
library(haven)
library(tidyr)

tanzania_followup_may2024_v7 <- read_dta("~/Desktop/World Bank embed/Tax Project/DATA/tanzania_followup_may2024_v7.dta")

#View(tanzania_followup_may2024_v7)




##### 1) Regression table: The Effect of TRA Agent Presence on Tax Morale

variables <- c("direct", "not_punishable", "avoid_paying", "refuse", "own_index")

results <- list()
models <- list()
num_obs <- numeric(length(variables))
control_means <- numeric(length(variables))
clustered_se_list <- list()

# Loop through each variable and run the regression
for (i in 1:length(variables)) {
  var <- variables[i]
  formula <- as.formula(paste(var, "~ treatment_direct_ownrecall"))
  model <- lm(formula, data = tanzania_followup_may2024_v7)
  
  # Calc clustered standard errors
  vcov_cluster <- vcovHC(model, type = "HC0", cluster = ~ ward)
  clustered_se <- sqrt(diag(vcov_cluster))
  
  # Store the result
  results[[var]] <- coeftest(model, vcov_cluster)
  models[[i]] <- model
  clustered_se_list[[i]] <- clustered_se
  
  num_obs[i] <- nobs(model)
  
  control_means[i] <- mean(tanzania_followup_may2024_v7[[var]][tanzania_followup_may2024_v7$treatment_direct_ownrecall == 0], na.rm = TRUE)
}

####### 1) latex table
texreg(
  models,
  caption = "The Effect of TRA Agent Presence on Tax Morale",
  custom.model.names = c("Direct", "Not Punishable", "Avoid Paying", "Refuse", "Own Index"),
  custom.coef.names = c("Intercept", "Direct Treatment"),
  caption.above = TRUE,
  stars = c(0.01, 0.05, 0.1),
  se = clustered_se_list,
  omit.coef = "Intercept",
  custom.note = "Notes: %stars. The dependent variables are based on surveys, are all Likert scale variables based on the following statements: (i) 'Please tell me whether the following is true or not: My firm does not pay all the [Direct taxes] it is required to pay.', (ii) 'Please tell me whether you think that the following action is not wrong at all, wrong but understandable, or wrong and punishable: Firms not paying the taxes they owe to the government.', (iii) 'Please state your level of agreement with the following statements: If I was sure I would not get caught, I would not pay all the taxes that I owe.', (iv) 'Please state your level of agreement with the following statements: Firms should refuse to pay taxes until they get better services from the national government.' and (v) The mean of the aforementioned 4 variables. The independent variable is one for any respondent who were treated themselves in the first survey wave in December 2022, and 0 for everyone else. We cluster standard errors at the ward level.",
  threeparttable = TRUE,
  booktabs = TRUE,
  include.rsquared = FALSE,
  custom.gof.rows = list("Control Group Mean" = sprintf("%.2f", control_means)),
  custom.gof.names = c("Adjusted R$^2$", "Number of Observations"),
  reorder.gof = c(3,2,1),
  digits = 3,
  file = "tables/tax_morale.tex",
  use.packages = FALSE
)

###################################################

##### 2) Regression table: The Effect of TRA Agent Presence on Perceptions of Fairness and Deception
variables_2 <- c("q24_treat_fair", "q26_see_TRA", "q28_feel_deceived")

results_2 <- list()
models_2 <- list()
num_obs_2 <- numeric(length(variables_2))
control_means_2 <- numeric(length(variables_2))
clustered_se_list_2 <- list()

# Loop through each variable and run the regression
for (i in 1:length(variables_2)) {
  var <- variables_2[i]
  formula <- as.formula(paste(var, "~ treatment_direct_ownrecall"))
  model <- lm(formula, data = tanzania_followup_may2024_v7)
  
  # Calc clustered standard errors
  vcov_cluster <- vcovHC(model, type = "HC0", cluster = ~ ward)
  clustered_se <- sqrt(diag(vcov_cluster))
  
  # Store the result
  results_2[[var]] <- coeftest(model, vcov_cluster)
  models_2[[i]] <- model
  clustered_se_list_2[[i]] <- clustered_se
  
  num_obs_2[i] <- nobs(model)
  
  control_means_2[i] <- mean(tanzania_followup_may2024_v7[[var]][tanzania_followup_may2024_v7$treatment_direct_ownrecall == 0], na.rm = TRUE)
}

####### 2) latex table
texreg(
  models_2,
  caption = "The Effect of TRA Agent Presence on Perceptions of Fairness and Deception",
  custom.model.names = c("Fair Treatment", "Sight of TRA Sights", "Feel Deceived"),
  custom.coef.names = c("Intercept", "Direct Treatment"),
  caption.above = TRUE,
  stars = c(0.01, 0.05, 0.1),
  se = clustered_se_list_2,
  omit.coef = "Intercept",
  custom.note = "Notes: %stars. The dependent variables are based on actual or hypothetical scenarios and they are (i) a 3-scale variable based on the question 'Did you feel treated respectfully and fairly?' during the treatment (the presence of the TRA during the survey), (ii) a 5-scale variable based on the question 'How did you feel at the sights of the TRA vehicles moving around in your area' and (iii) a 4-scale variable based on the question 'Did you feel deceived?' with the presence of the TRA during the survey. The independent variable is one for any respondent who were treated themselves in the first survey wave in December 2022, and 0 for everyone else. We cluster standard errors at the ward level.",
  threeparttable = TRUE,
  booktabs = TRUE,
  include.rsquared = FALSE,
  custom.gof.rows = list("Control Group Mean" = sprintf("%.2f", control_means_2)),
  custom.gof.names = c("Adjusted R$^2$", "Number of Observations"),
  reorder.gof = c(3,2,1),
  digits = 3,
  file = "tables/fairness_deception.tex",
  use.packages = FALSE
)

###################################################

##### 3) Regression Table: The Effect of TRA Agent Presence on beliefs about risk of evasion, and tax beliefs and public services
variables_3 <- c("C3g2", "C3h2", "C3i2")

results_3 <- list()
models_3 <- list()
num_obs_3 <- numeric(length(variables_3))
control_means_3 <- numeric(length(variables_3))
clustered_se_list_3 <- list()

# Loop through each variable and run the regression
for (i in 1:length(variables_3)) {
  var <- variables_3[i]
  formula <- as.formula(paste(var, "~ treatment_direct_ownrecall"))
  model <- lm(formula, data = tanzania_followup_may2024_v7)
  
  # Calc clustered standard errors
  vcov_cluster <- vcovHC(model, type = "HC0", cluster = ~ ward)
  clustered_se <- sqrt(diag(vcov_cluster))
  
  # Store the result
  results_3[[var]] <- coeftest(model, vcov_cluster)
  models_3[[i]] <- model
  clustered_se_list_3[[i]] <- clustered_se
  
  num_obs_3[i] <- nobs(model)
  
  control_means_3[i] <- mean(tanzania_followup_may2024_v7[[var]][tanzania_followup_may2024_v7$treatment_direct_ownrecall == 0], na.rm = TRUE)
}

####### 3) latex table
texreg(
  models_3,
  caption = "The Effect of TRA Agent Presence on beliefs about risk of evasion, and tax beliefs and public services",
  custom.model.names = c("(1)", "(2)", "(3)"),
  custom.coef.names = c("Intercept", "Direct Treatment"),
  caption.above = TRUE,
  stars = c(0.01, 0.05, 0.1),
  se = clustered_se_list_3,
  omit.coef = "Intercept",
  custom.note = "Notes: %stars. The dependent variables are based on surveys, are all Likert scale variables based on the following statements: (i) 'The risk of legal action for firms evading or underpaying taxes in Tanzania is high', (ii) 'The public services provided by the government justify the taxes imposed on firms', and (iii) 'Additional services benefiting firms would justify higher taxes'. The independent variable is one for any respondent who were treated themselves in the first survey wave in December 2022, and 0 for everyone else. We cluster standard errors at the ward level.",
  threeparttable = TRUE,
  booktabs = TRUE,
  include.rsquared = FALSE,
  custom.gof.rows = list("Control Group Mean" = sprintf("%.2f", control_means_3)),
  custom.gof.names = c("Adjusted R$^2$", "Number of Observations"),
  reorder.gof = c(3,2,1),
  digits = 3,
  file = "tables/tra_belief.tex",
  use.packages = FALSE
)

###################################################

##### 4) Regression Table: The Effect of TRA Agent Presence on interactions with the TRA

variables_4 <- c("C6a2", "C6b2", "C6d2", "C82", "C3r2", "q13_TRA_fair2")

results_4 <- list()
models_4 <- list()
num_obs_4 <- numeric(length(variables_4))
control_means_4 <- numeric(length(variables_4))
clustered_se_list_4 <- list()

# Loop through each variable and run the regression
for (i in 1:length(variables_4)) {
  var <- variables_4[i]
  formula <- as.formula(paste(var, "~ treatment_direct_ownrecall"))
  model <- lm(formula, data = tanzania_followup_may2024_v7)
  
  # Calc clustered standard errors
  vcov_cluster <- vcovHC(model, type = "HC0", cluster = ~ ward)
  clustered_se <- sqrt(diag(vcov_cluster))
  
  # Store the result
  results_4[[var]] <- coeftest(model, vcov_cluster)
  models_4[[i]] <- model
  clustered_se_list_4[[i]] <- clustered_se
  
  num_obs_4[i] <- nobs(model)
  
  control_means_4[i] <- mean(tanzania_followup_may2024_v7[[var]][tanzania_followup_may2024_v7$treatment_direct_ownrecall == 0], na.rm = TRUE)
}

####### 4) latex table
texreg(
  models_4,
  caption = "The Effect of TRA Agent Presence on interactions with the TRA",
  custom.model.names = c("(1)", "(2)","(3)", "(4)", "(5)", "(6)"),
  custom.coef.names = c("Intercept", "Direct Treatment"),
  caption.above = TRUE,
  stars = c(0.01, 0.05, 0.1),
  se = clustered_se_list_4,
  omit.coef = "Intercept",
  custom.note = "Notes: %stars. The dependent variables are based on surveys, are all Likert scale variables based on the following statements: (i) 'In the last six months, how many times (if at all) did a tax inspector or tax agent from the TRA visit your business to ask you to pay taxes?', (ii) 'Was the interaction with the TRA friendly?', (iii) 'Which of the following best describes how you view these visits?', (v) 'C8. Was an informal gift or payment ever expected or requested during these visits?', (vi) 'TRA makes it easy for you to pay your taxes', and (vii) 'TRA treats all taxpayers fairly'. The independent variable is one for any respondent who were treated themselves in the first survey wave in December 2022, and 0 for everyone else. We cluster standard errors at the ward level.",
  threeparttable = TRUE,
  booktabs = TRUE,
  include.rsquared = FALSE,
  custom.gof.rows = list("Control Group Mean" = sprintf("%.2f", control_means_4)),
  custom.gof.names = c("Adjusted R$^2$", "Number of Observations"),
  reorder.gof = c(3,2,1),
  digits = 3,
  file = "tables/tra_interactions.tex", 
  use.packages = FALSE
)

###################################################

## Summary table

##### Prepare tables

# B5: Age
age_summary <- tanzania_followup_may2024_v7 %>%
  summarize(
    Minimum = round(min(B5, na.rm = TRUE)),
    Maximum = round(max(B5, na.rm = TRUE)),
    Average = round(mean(B5, na.rm = TRUE))) %>% 
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
  mutate(Category = "Age Summary") %>%
  select(Category, Statistic, Value)

# B6: Gender
gender <- tanzania_followup_may2024_v7 %>% 
  group_by(B6) %>% 
  summarize(Frequency = n()) %>% 
  rename(Statistic = B6, Value = "Frequency") %>%
  mutate(Category = "Gender Distribution") %>%
  select(Category, Statistic, Value)

# B7: Number of employees
B7_employees_order <- c("1 worker", "2-4 workers", "5-9 workers", "10-19 workers", "20-49 workers", "50 or more", "Don't Know/Refused to answer")

employees_freq <- tanzania_followup_may2024_v7 %>%
  mutate(B7 = ifelse(B7 %in% c("Don't know", "Refused to answer"), "Don't Know/Refused to answer", B7)) %>%
  group_by(B7) %>%
  summarize(Frequency = n()) %>%
  mutate(B7 = factor(B7, levels = B7_employees_order)) %>%
  arrange(B7) %>%
  #rename("Total number of employees at the firm" = B7)
  rename(Statistic = B7, Value = "Frequency") %>%
  mutate(Category = "Total number of employees at the firm") %>%
  select(Category, Statistic, Value)

# B4: Primary Economic Activity of the business
industry_freq <- tanzania_followup_may2024_v7 %>%
  mutate(B4 = ifelse(B4 %in% c("Don't know", "Prefer not to say"), "Don't Know/Prefer not to say", B4)) %>%
  mutate(B4 = recode(B4, 
                     "Accommodation and food service activities (including tourism related activities)" = "Accommodation and food service activities", 
                     "Other service activities (including real estate services)" = "Other service activities", 
                     "Professional services (scientific and technical)" = "Professional services", 
                     "Transportation & storage (land, sea & ocean, air, courier services, postal and support activities, warehouse and stor…" = "Transportation and storage")) %>%
  group_by(B4) %>%
  summarize(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  #rename("Primary Economic Activity of the Business" = B4)
  rename(Statistic = B4, Value = "Frequency") %>%
  mutate(Category = "Primary Economic Activity of the Business") %>%
  select(Category, Statistic, Value)

# B8: Firm's Turnover in Feb 2024
turnover_freq <- tanzania_followup_may2024_v7 %>%
  group_by(B8) %>%
  summarize(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  #rename("Firm's Turnover in Feb 2024" = B8)
  rename(Statistic = B8, Value = "Frequency") %>%
  mutate(Category = "Firm's Turnover in Feb 2024") %>%
  select(Category, Statistic, Value)

# B9: Percentage of suppliers providing fiscal receipts for transactions
suppliers_receipt_freq <- tanzania_followup_may2024_v7 %>%
  group_by(B9) %>%
  summarize(Frequency = n()) %>%
  #rename("Percentage of suppliers providing fiscal receipts for transactions" = B9)
  rename(Statistic = B9, Value = "Frequency") %>%
  mutate(Category = "Percentage of suppliers providing fiscal receipts for transactions") %>%
  select(Category, Statistic, Value)

# 
age_summary
gender
employees_freq
industry_freq
turnover_freq
suppliers_receipt_freq

# Combine data frames
combined_table <- bind_rows(age_summary,gender,employees_freq,industry_freq,turnover_freq,suppliers_receipt_freq)


# Generate latex table with labels

kbl(combined_table %>% select(-Category), format = "latex", booktabs = TRUE, linesep = "", caption = "Summary Table") %>%
  kable_styling(latex_options = "hold_position") %>%
  #add_header_above(c(" " = 1, "Statistics" = 1, "Values" = 1)) %>%
  pack_rows("Age Summary", 1, nrow(age_summary)) %>%
  pack_rows("Gender Distribution", nrow(age_summary) + 1, nrow(age_summary) + nrow(gender)) %>%
  pack_rows("Total number of employees at the firm", nrow(age_summary) + nrow(gender) + 1, nrow(age_summary) + nrow(gender) + nrow(employees_freq)) %>%
  pack_rows("Primary Economic Activity of the Business", nrow(age_summary) + nrow(gender) + nrow(employees_freq) + 1, nrow(age_summary) + nrow(gender) + nrow(employees_freq) + nrow(industry_freq)) %>%
  pack_rows("Firm's Turnover in Feb 2024", nrow(age_summary) + nrow(gender) + nrow(employees_freq) + nrow(industry_freq) + 1, nrow(age_summary) + nrow(gender) + nrow(employees_freq) + nrow(industry_freq) + nrow(turnover_freq)) %>%
  pack_rows("Percentage of suppliers providing fiscal receipts for transactions", nrow(age_summary) + nrow(gender) + nrow(employees_freq) + nrow(industry_freq) + nrow(turnover_freq) + 1, nrow(combined_table)) %>%
  #cat()
  save_kable(file = "tables/summary_stats.tex")

###################################################








###################################################
# DISCARD

##### 8) Regression Results for treatment by region (interaction)

## For 1 dependent variable

treatment_region <- lm(q24_treat_fair ~ treatment_direct_ownrecall * region, data = tanzania_followup_may2024_v7) 
summary(treatment_region)



####### 8) latex table

## For 1 dependent variable

stargazer(treatment_region, type = 'latex', out = "table8.tex", header = FALSE,
          title = "Regression Results with Clustered Standard Errors",
          dep.var.caption = "", # Removes the "Dependent variable" caption
          omit.stat = c("rsq", "f", "ser"), # Omits R-squared, F-statistic, and Residual Std. Error
          dep.var.labels.include = FALSE, # Removes "Dependent variable" label
          notes.append = FALSE, # Removes default note
          keep.stat = c("n", "adj.rsq"), # Keeps the number of observations and Adjusted R-squared
          omit = "Constant", # Omits the constant term
          no.space = TRUE # Reduces extra space
)
