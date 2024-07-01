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

# LOAD DATA
BMT_bara_TBI <-
  read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
             sheet = "BMT bara TBI")

innlagdir <-
  read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
             sheet = "Innlagdir af BMT")

pop <-
  read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
             sheet = "Sheet1")
pop$Population <- 10 ^ (pop$`log10(allir)`)


# Q3
BMT_bara_TBI_1 <- BMT_bara_TBI %>%
  mutate(Date = `Dagsetning innskriftar komu`) %>%
  group_by(ID) %>%
  arrange(ID, Date) %>%
  mutate(Visit_Number = row_number()) %>%
  #filter(Visit_Number == 1)  %>%
  #dplyr::select(ID, `ar innskriftar`, Landsvaedi, averkaskor, Visit_Number, Aldurshopar, `kodi kyns`, `Forgangsflokkun a bradamottoku`) %>%
  rename(Year = `ar innskriftar`, 
         Gender = `kodi kyns`, 
         Urgency = `Forgangsflokkun a bradamottoku`) %>%
  filter(Gender != 0, 
         !is.na(Urgency)) %>%
  mutate(Year = as.numeric(Year))

highrisk_ds <- BMT_bara_TBI_1 %>%
  group_by(ID) %>%
    summarise(highrisk_index = max(Visit_Number))

# Merge visit data with highrisk value
BMT_bara_TBI_hs <- BMT_bara_TBI_1 %>%
  left_join(highrisk_ds, by="ID")

#cor(BMT_bara_TBI_hs$averkaskor, BMT_bara_TBI_hs$highrisk_index)
# low -0.01588711

pop <- pop %>%
  rename(Year = `ar innskriftar`) %>%
  mutate(Year = as.numeric(Year))

# Merge visit data with population data
data <- BMT_bara_TBI_hs %>%
  inner_join(pop, by = c("Landsvaedi", "Year"))

data <- data %>%
  mutate(Population = ifelse(Landsvaedi == 0, 100000, Population), 
         Year_Since_2010 = Year - 2010, 
         Landsvaedi3 = ifelse(Landsvaedi == 1, "Capital",
                                ifelse(Landsvaedi == 1.5, "Surrounding Area", 
                                       "Countryside"))) %>%
  ungroup() #  not bound anymore by each group (ID)
save(data, file = "data_q3.RData")

aggregate(data = data, Population ~ Landsvaedi3, mean)



# is averkaskor getting more severe over time?
lm_averkaskor <- lm(averkaskor ~ Year, data = data)
summary(lm_averkaskor)
# Results indicated a slight but statistically significant increase
# in averkaskor over time (Estimate: 0.004384, p-value: 0.0212)
# On average, averkaskor scores have increased by 
# ~ 0.004384 units / year
# HOWEVER
# R2 was extremely low (7.28e-05)
# year alone explains a very small proportion of the var in averkaskor 
# given the large sample size of 72927 visits 
# larger sample sizes tend to produce smaller standard errors 
# which in turn can inflate the F-statistic 
# leading to statistically significant p-values even for negligible effect sizes 
# the practical significance of this trend remains questionable



ggplot(data, aes(x = Year_Since_2010, y = averkaskor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Trend of Averkaskor Over Time", x = "Year_Since_2010", y = "Averkaskor") +
  theme_minimal()


# re-examine the significance of the trend in averkaskor scores over time 
# using a random sample of 5000 (10K and 25K also tried) observations from
set.seed(123)
sample_size <- 10000 # random sample
sample_data <- data %>%
  sample_n(size = sample_size)
lm_sample <- lm(averkaskor ~ Year, data = sample_data)
summary(lm_sample)
# in the random sample the year coeff is not statistically sig
# the r2 remains low 
# the f statistic is not significant indicating no evidence of a relationship
# between averkaskor and year 
# The critical value from the F-distribution table at a given significance level (alpha) 
# and degrees of freedom determines the threshold beyond which we 
# consider the F-statistic to be statistically significant
# since the f statisitc is not significant the model does not provide sufficient 
# evidence to conclude that the predictors collectively explain 
#variation in the dependent variable beyond what would be expected by chance
df_regression <- 1  # numerator df (predictors in the model)
df_residual <- length(lm_sample$residuals) - df_regression - 1  # denominator df 
f_statistic <- summary(lm_sample)$fstatistic[1]
alpha <- 0.05
f_critical <- qf(1 - alpha, df1 = df_regression, df2 = df_residual)

curve(df(x, df1 = df_regression, df2 = df_residual), from = 0, to = 5,
      xlab = "F-value", ylab = "Density", main = "F-distribution for lm_sample")
abline(v = f_statistic, col = "blue", lwd = 2, lty = 2)
abline(v = f_critical, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("F-statistic", "Critical Value"), 
       col = c("blue", "red"), lty = 2, lwd = 2)
# If the blue line is to the right of the red line, it suggests that the 
# relationship observed in your model is unlikely to be due to random chance 
# alone, indicating statistical significance
# The curve itself represents the pdf of the F-distribution
# It shows how likely different values of the F-statistic are under the null 
# hypothesis (no relationship between predictors and response)


# multiple linear regression 
lm_model <- lm(averkaskor ~ Year_Since_2010 + as.factor(Aldurshopar) +
                  as.factor(Gender) + Urgency +
                  as.factor(Landsvaedi3) + log(Population) + highrisk_index,
                data = data)
summary(lm_model)

#  improved multiple linear regression model with interaction terms
lm_model_improved <- lm(averkaskor ~ Year_Since_2010 * Urgency +
                           as.factor(Aldurshopar) +
                           as.factor(Gender)
                         + Urgency + as.factor(Landsvaedi3) +
                           log(Population) + highrisk_index,
                         data = data)

summary(lm_model_improved)

# diagnostic measures
par(mfrow = c(2, 2)) 
plot(lm_model_improved, which = 1) # Res vs Fitted
# res are obs value minus predicted value (so negative values are ok here)
# trend line: negative slope suggests that as the fitted values increase, the res 
# tend to decrease slightly, indicating potential heteroscedasticity
# points with high residuals, particularly those beyond 20 on the vertical axis
# indicate instances where the model predictions significantly differ from the obs data
plot(lm_model_improved, which = 2) # qq plot
# while the residuals follow a roughly normal distribution in the central range 
# they deviate in the tails
# showing more variability in the tails than expected 
# under the assumption of normality
plot(lm_model_improved, which = 3) # spread-location
# heteroscedastic
plot(lm_model_improved, which = 5) # cooks distance
# 3 points labeled but they have low leverage so minimal influence on the model 





# negative binomial regression model
nb_model <- glm.nb(averkaskor ~ Year + as.factor(Aldurshopar) +
                     as.factor(Gender) + Urgency +
                     as.factor(Landsvaedi3) + log(Population) + highrisk_index,
                   data = data)




# any columns that start with ICD and has a - in the cell will turn into an NA
BMT_sub <- BMT_bara_TBI %>%
  mutate_at(vars(starts_with("ICD")), ~na_if(., "-"))

# extract ICD columns and find unique first letters
ICD_columns <- dplyr::select(BMT_bara_TBI, starts_with("ICD")) %>%
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
  BMT_sub <- BMT_sub %>%
    mutate(!!paste0(letter, "_total") := count_ICD_codes(dplyr::select(., starts_with("ICD")), letter))
}

# total amount of times each unique ICD letter appeared in this dataset
# represented in a dataframe
total_columns <- grep("_total$", names(BMT_sub), value = TRUE)
column_sums <- colSums(BMT_sub[total_columns], na.rm = TRUE)
total_sums <- data.frame(column_names = names(column_sums), total_sum = column_sums)
total_sums


# ICD CODES OVER TIME
icd_totals_by_year <- BMT_sub %>%
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
#save(icd_totals_by_year, file = "icd_totals_by_year.RData")
#write.xlsx(icd_totals_by_year, "icd_totals_by_year.xlsx")




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


























plot(data$Year_Since_2010, data$averkaskor)
plot(data$Population, data$averkaskor)
plot(log(data$Population), data$averkaskor)

mean(data$averkaskor==0)
# almost 40% of the averkaskor is 0
# so maybe zeroinflated poisson 
# but there are no NAs?
# are the 0 actually NAs?
# maybe negative binomial?

lm_model <- lm(averkaskor ~ Year + as.factor(Landsvaedi) + log(Population), data = data)
summary(lm_model)
ggplot(data, aes(x = Year, y = averkaskor, color = as.factor(Landsvaedi))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Averkaskor by Year and Geographical Zone", x = "Year", y = "Averkaskor", color = "Landsvaedi") +
  theme_minimal()
# there are significant relationships between averkaskor and the predictors 
# Year, Landsvaedi, and log(Population)
# However, the low R-squared value suggests that these predictors explain
# only a small portion of the variance in averkaskor



lm_model_k <- lm(averkaskor ~ Year + log(Population), data = data)
summary(lm_model_k)

# predict(lm_model)-data$averkaskor
# commented out because it is too long in the console
# doing good but giving decimals of everything
# 15.4 when it should be 15 



pois_glm_model <- glm(averkaskor ~ Year + as.factor(Landsvaedi) + log(Population), 
               data = data, family= poisson)
# predict(pois_glm_model, type="response") - data$averkaskor
# commented out because it is too long in the console


rpois(20, 1.34)

which(data$averkaskor>20)
predict(pois_glm_model, type="response")[51320:51330]
data$averkaskor[51320:51330]
# so the poisson reg model is not accurately predicting 
sqrt(mean((predict(pois_glm_model, type='response') - data$averkaskor)^2))
sqrt(mean((predict(lm_model) - data$averkaskor)^2))
# both model performing roughly equally 


# Non linearity maybe?
gam_model <- gam(averkaskor ~ s(Year) + as.factor(Landsvaedi) + s(log(Population)), data = data)
summary(gam_model)
plot(gam_model, pages = 1)

# Res vs Fitted 
plot(gam_model$fitted.values, resid(gam_model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")


