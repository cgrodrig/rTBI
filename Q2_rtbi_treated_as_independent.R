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

# LOAD DATA
innlagdir <-
  read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
             sheet = "Innlagdir af BMT")

pop <-
  read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
             sheet = "Sheet1")
pop$Population <- 10 ^ (pop$`log10(allir)`)


# QUESTION 2
# Understand the significance of the patient count with TBI per year
# and geographical location while controlling for population growth
# recurrent TBI

# summarize the visit counts by year and landsvaedid
visit_counts <- innlagdir %>%
  dplyr::select("ID", `ar innskriftar`, "Landsvaedi")%>%
  group_by(`ar innskriftar`, Landsvaedi) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  rename(Year = `ar innskriftar`) %>%
  mutate(Year = as.numeric(Year))

pop <- pop %>%
  rename(Year = `ar innskriftar`) %>%
  mutate(Year = as.numeric(Year)) %>%
  dplyr::select(!`log10(allir)`) %>%
  filter(Landsvaedi != 9) # remove the total column

# ensure all combinations of Year and Landsvaedi are represented in visit_counts
all_combinations <- expand.grid(Year = unique(visit_counts$Year),
                                Landsvaedi = unique(visit_counts$Landsvaedi))

visit_counts <- all_combinations %>%
  left_join(visit_counts, by = c("Year", "Landsvaedi")) %>%
  replace_na(list(Count = 0))  # replace NA counts with 0


data <- visit_counts %>%
  left_join(pop, by = c("Landsvaedi", "Year")) 
#data should have 126 rows

# There are 5 specific Year-Landsvaedi combos that have zeroes in it
# They should not be removed from the dataset but should instead retain the value 0
# table(visit_counts$Year, visit_counts$Landsvaedi)

# recode years variable as years since 2010
data$Year_Since_2010 <- data$Year - 2010
data <- data[, !(names(data) == "Year")]

# Regression
# Log-linear regression model
data$Landsvaedi <- as.factor(data$Landsvaedi)
data$Landsvaedi <- relevel(data$Landsvaedi, ref = "1")
data$log_Count = log(data$Count)
data$log_Count = log(data$Count + 0.1)
log_linear_model <- lm(log_Count ~ Year_Since_2010 + Landsvaedi + offset(log(Population)), data = data)
summary(log_linear_model)
vif(log_linear_model)
sjPlot::tab_model(log_linear_model)

# Plot residuals against fitted values
plot(residuals(log_linear_model) ~ fitted(log_linear_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")

# Normal Q-Q plot
qqnorm(residuals(log_linear_model))
qqline(residuals(log_linear_model), col = "red")

# Shapiro-Wilk test for normality
shapiro.test(residuals(log_linear_model))
# Kolmogorov-Smirnov test for normality
ks.test(residuals(log_linear_model), "pnorm", mean(residuals(log_linear_model)), sd(residuals(log_linear_model)))


