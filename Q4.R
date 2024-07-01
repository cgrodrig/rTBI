library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(MASS)
library(sjPlot)
library(car)
library(glmnet)
library(stats)
library(knitr)
library(mgcv)
library(broom)
library(purrr)
library(openxlsx)
library(randomForest)
library(gbm)

# LOAD DATA
innlagdir <-
  read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
             sheet = "Innlagdir af BMT")

pop <-
  read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
             sheet = "Sheet1")
pop$Population <- 10 ^ (pop$`log10(allir)`)




innlagdir_1 <- innlagdir %>%
  mutate(Date = `Dagsetning innskriftar komu`) %>%
  group_by(ID) %>%
  arrange(ID, Date) %>%
  mutate(Visit_Number = row_number()) %>%
  rename(Year = `ar innskriftar`,
         Gender = `kodi kyns`, 
         Urgency = `Forgangsflokkun a bradamottoku`) %>%
  mutate(Year = as.numeric(Year))  %>%
  filter(Gender != 0, 
         !is.na(Urgency))


highrisk_inn <- innlagdir_1 %>%
  group_by(ID) %>%
  summarise(highrisk_index = max(Visit_Number))  
  
innlagdir_hs <- innlagdir_1 %>%
  left_join(highrisk_inn, by="ID")


pop <- pop %>%
  rename(Year = `ar innskriftar`) %>%
  mutate(Year = as.numeric(Year))

# Merge visit data with population data
data <- innlagdir_hs %>%
  inner_join(pop, by = c("Landsvaedi", "Year"))

data <- data %>%
  mutate(Population = ifelse(Landsvaedi == 0, 100000, Population), 
         Year_Since_2010 = Year - 2010, 
         Landsvaedi3 = ifelse(Landsvaedi == 1, "Capital",
                              ifelse(Landsvaedi == 1.5, "Surrounding Area", 
                                     "Countryside"))) %>%
  ungroup() #  not bound anymore by each group (ID)
save(data, file = "data_q4.RData")
aggregate(data = data, Population ~ Landsvaedi3, mean)

# is averkaskor getting more severe over time?
lm_averkaskor <- lm(averkaskor ~ Year, data = data)
summary(lm_averkaskor)

set.seed(123)
sample_size <- 650 # random sample
sample_data <- data %>%
  sample_n(size = sample_size)
lm_sample <- lm(averkaskor ~ Year, data = sample_data)
summary(lm_sample)


# multiple linear regression 
lm_model <- lm(averkaskor ~ Year_Since_2010 + as.factor(aldurshop) +
                 as.factor(Gender) + Urgency +
                 as.factor(Landsvaedi3) + log(Population) + highrisk_index,
               data = data)
summary(lm_model)

#  improved multiple linear regression model with interaction terms
lm_model_improved <- lm(averkaskor ~ Year_Since_2010 * Urgency +
                          as.factor(aldurshop) +
                          as.factor(Gender)
                        + Urgency + as.factor(Landsvaedi3) +
                          log(Population) + highrisk_index,
                        data = data)

summary(lm_model_improved)

par(mfrow = c(2, 2))
plot(lm_model_improved, which = 1)
plot(lm_model_improved, which = 2)
plot(lm_model_improved, which = 3)
plot(lm_model_improved, which = 5)

# negative binomial regression model
nb_model <- glm.nb(averkaskor ~ Year + as.factor(aldurshop) +
                     as.factor(Gender) + Urgency +
                     as.factor(Landsvaedi3) + log(Population) + highrisk_index,
                   data = data)
summary(nb_model)








# random forest
data <- data %>%
  dplyr::select(ID, Year, Landsvaedi3, Land,  
                averkaskor, Visit_Number, aldurshop, Gender, Urgency) %>%
  mutate(Year = as.numeric(Year))%>%
  filter(!is.na(aldurshop))

# remember NO categorical variables, encode them
data <- data %>%
  mutate(Landsvaedi3 = as.factor(Landsvaedi3)) 



# understanding patterns specific to individuals 
# tracking changes over time for specific persons
set.seed(3008)
rf_model <- randomForest(averkaskor ~ ., data = data, importance = TRUE)
print(rf_model) 
# how well does the rfm fit the data? mse & # of trees
importance(rf_model)
# which variables are most imp in predicting averkaskor?
varImpPlot(rf_model) 
# higher MSE means the var is more important 
# should I have removed ID?
















# THE ICD QUESTION
# any columns that start with ICD and has a - in the cell will turn into an NA
innlagdir_sub <- innlagdir %>%
  mutate_at(vars(starts_with("ICD")), ~na_if(., "-"))

# extract ICD columns and find unique first letters
ICD_columns <- dplyr::select(innlagdir, starts_with("ICD")) %>%
  unlist() %>%
  na.omit()
# 16*76854 = 1229664
# a single vector of 1229664 ICD codes, most are NA
# 1579 unique ICD codes(?)

first_letters <- substr(ICD_columns, 1, 1) # extracrt first letter
unique_first_letters <- unique(first_letters) # only unique first letter

# FUCNTION: count occurrences of ICD codes starting with a specific letter
count_ICD_codes <- function(df, letter) {
  total_counts <- rowSums(sapply(df, function(col) grepl(paste0("^", letter), col)))
  return(total_counts)
}

# create columns for each unique ICD using the FUNCTION above
# each row can possible have more than 1 S ICD code for ex
# these "total" columns are totaling the unique letters in each row 
for (letter in unique_first_letters) {
  innlagdir_sub <- innlagdir_sub %>%
    mutate(!!paste0(letter, "_total") := count_ICD_codes(dplyr::select(., starts_with("ICD")), letter))
}

# total amount of times each unique ICD letter appeared in this dataset
# represented in a dataframe
total_columns <- grep("_total$", names(innlagdir_sub), value = TRUE)
column_sums <- colSums(innlagdir_sub[total_columns], na.rm = TRUE)
total_sums <- data.frame(column_names = names(column_sums), total_sum = column_sums)
total_sums


# ICD CODES OVER TIME
icd_totals_by_year <- innlagdir_sub %>%
  rename(Year = `ar innskriftar`, 
         Gender = `kodi kyns`, 
         Urgency = `Forgangsflokkun a bradamottoku`) %>%
  filter(Gender != 0, 
         !is.na(Urgency)) %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::select(Year, ends_with("_total")) %>%
  group_by(Year) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(-Year, names_to = "Category", values_to = "Total")
icd_totals_by_year

ggplot(icd_totals_by_year, aes(x = Year, y = Total, color = Category)) +
  geom_line() +
  labs(title = "Trends of ICD Code Categories Over Time", 
       x = "Year", 
       y = "Total ICD Codes",
       color = "ICD Category") +
  theme_minimal()
#save(icd_totals_by_year, file = "icd_totals_by_yearQ4.RData")
#write.xlsx(icd_totals_by_year, "icd_totals_by_yearQ4.xlsx")




## ICD QUESTION
# quantify the rate of change for each ICD code category
# performing linear regression for each ICD code 
#  is there a statistically significant trend in the frequency of each category over time?
run_lm <- function(data) {
  lm_result <- lm(Total ~ Year, data = data)
  return(tidy(lm_result))
}

# split by category and run linear regression
lm_results <- icd_totals_by_year %>%
  group_by(Category) %>%
  nest() %>%
  mutate(lm = map(data, run_lm)) %>%
  unnest(lm)

lm_results

significant_trends <- lm_results %>%
  filter(term == "Year" & p.value < 0.05)
significant_trends

# trends for significant categories
ggplot(icd_totals_by_year %>% filter(Category %in% significant_trends$Category), aes(x = Year, y = Total, color = Category)) +
  geom_line() +
  labs(title = "Trends of ICD Code Categories Over Time", 
       x = "Year", 
       y = "Total ICD Codes",
       color = "ICD Category") +
  theme_minimal()


# diagnostics
# Extract residuals and fitted values for significant models
extract_diagnostics <- function(data) {
  lm_model <- lm(Total ~ Year, data = data)
  augment(lm_model)
}

# Apply to significant models
significant_diagnostics <- icd_totals_by_year %>%
  filter(Category %in% significant_trends$Category) %>%
  group_by(Category) %>%
  nest() %>%
  dplyr::mutate(diagnostics = map(data, extract_diagnostics)) %>%
  unnest(diagnostics)

# Check residuals plot for significant models
ggplot(significant_diagnostics, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Category, scales = "free") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Check linearity
ggplot(significant_diagnostics, aes(x = Year, y = .fitted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Category, scales = "free") +
  labs(title = "Linearity Check: Fitted Values vs Year",
       x = "Year",
       y = "Fitted Values") +
  theme_minimal()

# Extract R-squared and F-statistics for significant models
summary_stats <- significant_trends %>%
  group_by(Category) %>%
  summarise(
    R_squared = summary(lm(Total ~ Year, data = icd_totals_by_year %>% filter(Category == unique(Category))))$r.squared,
    F_statistic = summary(lm(Total ~ Year, data = icd_totals_by_year %>% filter(Category == unique(Category))))$fstatistic[1]
  )

summary_stats

# homoscedasticity
ggplot(significant_diagnostics, aes(x = .fitted, y = sqrt(abs(.resid)))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Category, scales = "free") +
  labs(title = "Scale-Location Plot",
       x = "Fitted Values",
       y = "Sqrt(|Residuals|)") +
  theme_minimal()

# normality of residuals
ggplot(significant_diagnostics, aes(sample = .resid)) +
  stat_qq(alpha = 0.5) +
  stat_qq_line() +
  facet_wrap(~ Category, scales = "free") +
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_minimal()



