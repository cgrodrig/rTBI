# Q1 
# Log Linear Model 
# repeated head injuries treated as independent 

library(readxl) 
library(dplyr) 
library(lubridate) 
library(tidyr) 
library(ggplot2) 
library(MASS)
library(stats)
library(knitr)

# EDA
library(skimr)

# Distribution fitting models
library(VGAM) # pareto

# regression
library(glmnet)

# Summary of Regression
library(sjPlot)
library(car) # VIF

# QUESTION 1
# Understand the significance of the patient count with TBI per year
# and geographical location while controlling for population growth
# recurrent TBI

# LOAD DATA
BMT_bara_TBI <-
  read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
             sheet = "BMT bara TBI")

# innlagdir <-
#   read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
#              sheet = "Innlagdir af BMT")

pop <-
  read_excel("unprotected_english_5436_RANN_Hofudaverkar.xlsx",
             sheet = "Sheet1")
pop$Population <- 10 ^ (pop$`log10(allir)`)

# Inclusion-Exclusion Criteria
# No participants should be removed 

# Variable Selection 
BMT_bara_TBI_1 <- BMT_bara_TBI %>%
  mutate(Date = `Dagsetning innskriftar komu`) %>%
  group_by(ID) %>%
  arrange(ID, Date) %>%
  dplyr::select("ID", `ar innskriftar`, "Landsvaedi")


# summarize the visit counts by year and landsvaedid
visit_counts <- BMT_bara_TBI_1 %>%
  group_by(`ar innskriftar`, Landsvaedi) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  rename(Year = `ar innskriftar`) %>%
  mutate(Year = as.numeric(Year))

pop <- pop %>%
  rename(Year = `ar innskriftar`) %>%
  mutate(Year = as.numeric(Year)) %>%
  dplyr::select(!`log10(allir)`) %>%
  filter(Landsvaedi != 9) # remove the total column

# Merge BMT dataset with population data
data <- visit_counts %>%
  left_join(pop, by = c("Landsvaedi", "Year")) %>%
  # create a new column to recategorize the landsvaedid into just 3 categories
  # in hope of better analysis
  mutate(Landsvaedi3 = ifelse(Landsvaedi == 1, "Capital",
                              ifelse(Landsvaedi == 1.5, "Surrounding Area", 
                                     "Other"))) %>%
  mutate(Landsvaedi3 = factor(Landsvaedi3))


# recode years variable as years since 2010
data$Year_Since_2010 <- data$Year - 2010

data <- data[, !(names(data) == "Year")]

save(data, file = "data_q1.RData")


# EDA
summary(data)
str(data)
skim(data)
# 14 years of data from 2010 to 2023
# 10 landsvaedid categories





# FIT A STATISTICAL MODEL TO THE DATA 
# the outcome variable = count (# of tbi per year & per landsvaedid)
# this count data is discrete data with non-negative integer values
# the count data can be expressed as rate data, since the number of times 
# a tbi occurs within a year in a certain landsvaedid can be expressed 
# as a raw count
# the counts are ASSUMED to be all independent from each other 
# for those reasons, should we use poisson regression to model the probability
# of a tbi occuring within the specific timeframe 
# Well, does the response variable follow a poisson distribution?
ggplot(data, aes(x = Count)) +
  geom_histogram(binwidth = 1,
                 fill = "blue",
                 color = "black") +
  labs(title = "Histogram of TBI Counts", x = "Count", y = "Frequency")
# yes the distribution looks like a poisson distribution with lambda eq. to 1
# but maybe there are too many bins
# trying again
ggplot(data, aes(x = Count)) +
  geom_histogram(binwidth = 25,
                 fill = "blue",
                 color = "black") +
  labs(title = "Histogram of TBI Counts", x = "Count", y = "Frequency")


# Overlay Poisson distribution on histogram
# Poisson distribution with lambda = (try several)
lambda_p <- mean(data$Count)
counts <- 0:max(data$Count)  # from 0 to maxcount in data
poisson_pmf <- dpois(counts, lambda_p)
# lambda represents mean of the poisson dist 
# so we will use the mean of the count variable to estimate lambda
ggplot(data, aes(x = Count)) +
  geom_histogram(binwidth = 25, fill = "blue", color = "black") +
  geom_line(data = data.frame(counts = counts, poisson_pmf = poisson_pmf), aes(x = counts, y = poisson_pmf), linetype = "dashed", color = "red") +
  labs(title = "Histogram of TBI Counts with Poisson Distribution", x = "Count", y = "Frequency") +
  theme_bw()

# maybe the distribution looks a bit more like a negative binomail distribution
# The negative binomial is similar to the Poisson model, but
# incorporates an additional term to account for the excess variance
# like poisson, NB characterizes
# count data where the majority of data points are clustered
# toward lower values of a variable

# lets check if the mean and variance of the count data are approx. equal
mean_count <- mean(data$Count)
variance_count <- var(data$Count)
mean_count
variance_count
# the mean and var differ by a lot.
# mean is 436 and variance is 598789
# NB model can be used when variance is substantially higher than the mean
# BDA textbook pg 437


# Overlay Neg Bin dist on histogram
mu = 100 # set to 100, bc this is the avg count expected
p = .75 # indicates the prob of getting a count in a single trial
r <- mu * p / (1-p) # function of mean & prob of success in the negbin dist
counts <- 0:max(data$Count)  # all possible counts from the data
nb_pmf <- dnbinom(counts, size = r, prob = p) # pmf for each count in the ccounts vector

ggplot(data, aes(x = Count)) +
  geom_histogram(aes(y=..count.. / sum(..count..)), binwidth = 25, fill = "blue", color = "black", ) +
  geom_line(data = data.frame(counts = counts, nb_pmf = nb_pmf), aes(x = counts, y = nb_pmf), linetype = "dashed", color = "red") +
  labs(title = "Histogram of TBI Counts with Negative Binomial Distribution", x = "Count", y = "Frequency") +
  theme_bw()

# the neg bin fits the data sort of but maybe we can improve on it?

# Perhaps the distribution of the count data is closer to the 
# log-Normal pdf
log_counts <- log(data$Count)
log_normal_mean <- mean(log_counts)
log_normal_sd <- sd(log_counts)
counts <- seq(0, max(data$Count), by = 1) # all possible counts 
log_normal_pmf <- dlnorm(counts, meanlog = log_normal_mean, sdlog = log_normal_sd)
# pmf of the lognorm dist for each count in the counts vector 

# normalize the PDF to visually match the scaling on the histogram
log_normal_pmf <- log_normal_pmf * length(data$Count) * 25

# ovevrlay log normal 
ggplot(data, aes(x = Count)) +
  geom_histogram(binwidth = 25, fill = "blue", color = "black") +
  geom_line(data = data.frame(counts = counts, log_normal_pmf = log_normal_pmf), aes(x = counts, y = log_normal_pmf), linetype = "dashed", color = "red") +
  labs(title = "Histogram of TBI Counts with Log-Normal Distribution", x = "Count", y = "Frequency") +
  theme_bw()



# Pareto distribution requires counts > 0
# pareto PDF 
# manual estimation of Pareto parameters
min_count <- min(data$Count[data$Count > 0])  
pareto_shape <- 1 + length(data$Count) / sum(log(data$Count/min_count))
# scale using the minimum count as a base
pareto_scale <- min_count / (pareto_shape - 1)
# Pareto PDF
counts <- seq(min_count, max(data$Count), by = 1)
pareto_pmf <- VGAM::dpareto(counts, scale = pareto_scale, shape = pareto_shape)

# Normalize the PDF to match the histogram
pareto_pmf <- pareto_pmf * length(data$Count) * 25

# Pareto overlay
ggplot(data, aes(x = Count)) +
  geom_histogram(binwidth = 25, fill = "blue", color = "black") +
  geom_line(data = data.frame(counts = counts, pareto_pmf = pareto_pmf), aes(x = counts, y = pareto_pmf), linetype = "dashed", color = "red") +
  labs(title = "Histogram of TBI Counts with Pareto Distribution", x = "Count", y = "Frequency") +
  theme_bw()

# Log norm underestimates the peak and pareto overetimates it
# which is better?
# use AIC
# log likelihood of log normal

# Log-Likelihood for Log-Normal and pareto
n <- length(data$Count)
log_ll <- -n/2 * log(2 * pi) - n * log(log_normal_sd) - 1/(2 * log_normal_sd^2) * sum((log(data$Count) - log_normal_mean)^2)
pareto_ll <- length(data$Count) * log(pareto_shape) + length(data$Count) * pareto_shape * log(pareto_scale) - (pareto_shape + 1) * sum(log(data$Count))

# AIC comparison
# Poisson Model
poisson_model <- glm(Count ~ Year_Since_2010 + Landsvaedi + offset(log(Population)), 
                     family = "poisson", data = data)
poisson_aic <- AIC(poisson_model)
neg_bin_model <- glm.nb(Count ~ Year_Since_2010 + Landsvaedi + offset(log(Population)), data = data)
neg_bin_aic <- AIC(neg_bin_model)

log_normal_aic <- 2 * 2 - 2 * log_ll  # 2 param (mean and sd)
pareto_aic <- 2 * 2 - 2 * pareto_ll  # 2 param (shape and scale)
log_normal_aic
pareto_aic
aic_values <- data.frame(Model = c("Log-Normal", "Pareto", "Poisson", "Negative Binomial"),
                         AIC = c(log_normal_aic, pareto_aic, poisson_aic, neg_bin_aic))
aic_values

# lognorm has a lower aic so it is the better fit to the data 

# Regression
# Log-linear regression model
data$Landsvaedi <- as.factor(data$Landsvaedi)
data$Landsvaedi <- relevel(data$Landsvaedi, ref = "1")
data$log_Count = log(data$Count)
log_linear_model <- lm(log_Count ~ Year_Since_2010 + Landsvaedi + offset(log(Population)), data = data)
summary(log_linear_model)
vif(log_linear_model)
sjPlot::tab_model(log_linear_model)


# Compare this model log_linear_model to a model that accounts for the non-independent nature of tbi
# ONLY SELECT THE 1st TIME VISITS
BMT_bara_TBI_2 <- BMT_bara_TBI %>%
   mutate(Date = `Dagsetning innskriftar komu`) %>%
   group_by(ID) %>%
   arrange(ID, Date) %>%
   mutate(Visit_Number = row_number()) %>%
   filter(Visit_Number == 1)  %>%
   dplyr::select("ID", `ar innskriftar`, "Landsvaedi")

visit_counts2 <- BMT_bara_TBI_2 %>%
  group_by(`ar innskriftar`, Landsvaedi) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  rename(Year = `ar innskriftar`) %>%
  mutate(Year = as.numeric(Year))

# Merge BMT dataset with population data
data2 <- visit_counts2 %>%
  left_join(pop, by = c("Landsvaedi", "Year")) %>%
  # create a new column to recategorize the landsvaedid into just 3 categories
  # in hope of better analysis
  mutate(Landsvaedi3 = ifelse(Landsvaedi == 1, "Capital",
                              ifelse(Landsvaedi == 1.5, "Surrounding Area", 
                                     "Other"))) %>%
  mutate(Landsvaedi3 = factor(Landsvaedi3))
data2$Year_Since_2010 <- data2$Year - 2010
data2 <- data2[, !(names(data) == "Year")]


data2$Landsvaedi <- as.factor(data2$Landsvaedi)
data2$Landsvaedi <- relevel(data2$Landsvaedi, ref = "1")
data2$log_Count = log(data2$Count)
log_linear_model2 <- lm(log_Count ~ Year_Since_2010 + as.factor(Landsvaedi) + offset(log(Population)), data = data2)
summary(log_linear_model2)
vif(log_linear_model2)
sjPlot::tab_model(log_linear_model2)

mean(data$Count)
mean(data2$Count)


coef1 <- summary(log_linear_model)$coefficients
coef2 <- summary(log_linear_model2)$coefficients
differences <- coef1[, "Estimate"] - coef2[, "Estimate"]
se_diff <- sqrt(coef1[, "Std. Error"]^2 + coef2[, "Std. Error"]^2)
t_values <- differences / se_diff
n1 <- summary(log_linear_model)$df[2]  # Degrees of freedom Residual from model 1
n2 <- summary(log_linear_model2)$df[2] # Degrees of freedom Residual from model 2
df <- (se_diff^4) / ((coef1[, "Std. Error"]^4 / (n1 - 1)) + (coef2[, "Std. Error"]^4 / (n2 - 1)))
p_values <- 2 * pt(-abs(t_values), df)

results <- data.frame(
  Coefficient = rownames(coef1),
  Estimate1 = coef1[, "Estimate"],
  StdError1 = coef1[, "Std. Error"],
  Estimate2 = coef2[, "Estimate"],
  StdError2 = coef2[, "Std. Error"],
  Difference = differences,
  T_value = t_values,
  Degrees_of_Freedom = df,
  P_value = p_values
)
results








# MODEL 1 PREDICTS THE LANDSVAEDI UNEVENLY ACCURATE
predict(log_linear_model)-data$Count
# the predictions are not perfect
# large residuals are present and indicate poor model fit for certain observations

# lets analyze bias for specific Landsvaedid
bias <- exp(predict(log_linear_model))-data$Count
bias0 <- bias[data$Landsvaedi=="0"]
bias1 <- bias[data$Landsvaedi=="1"]
# region 1 is sometimes off by a few hundred because the
# counts are in the thousands
bias1_5<-bias[data$Landsvaedi=="1.5"]
bias2 <- bias[data$Landsvaedi=="2"]
bias3 <- bias[data$Landsvaedi=="3"]
bias4 <- bias[data$Landsvaedi=="4"]
bias5 <- bias[data$Landsvaedi=="5"]
bias6 <- bias[data$Landsvaedi=="6"]
bias7 <- bias[data$Landsvaedi=="7"]
bias8 <- bias[data$Landsvaedi=="8"]

RSS <- sum((exp(predict(log_linear_model2)) - data$Count)^2)
TSS <- sum((data$Count - mean(data$Count))^2)
R_squared <- 1 - RSS/TSS
R_squared


# Bias over time plot
# first need a table to plot 
bias_list <- list(
  bias0 = bias[data$Landsvaedi=="0"],
  bias1 = bias[data$Landsvaedi=="1"],
  bias1_5 = bias[data$Landsvaedi=="1.5"],
  bias2 = bias[data$Landsvaedi=="2"],
  bias3 = bias[data$Landsvaedi=="3"],
  bias4 = bias[data$Landsvaedi=="4"],
  bias5 = bias[data$Landsvaedi=="5"],
  bias6 = bias[data$Landsvaedi=="6"],
  bias7 = bias[data$Landsvaedi=="7"],
  bias8 = bias[data$Landsvaedi=="8"]
)


start_year <- 2010
bias_data <- data.frame(Landsvaedi = character(), Year = integer(), Bias = numeric())

# loop through each landsvaedi, create a dataframe, and bind it to the main dataframe
for (i in names(bias_list)) {
  # extract landsvaedi #
  landsvaedi <- gsub("bias", "", i)
  landsvaedi <- gsub("_", ".", landsvaedi)
  

  temp_df <- data.frame(
    Landsvaedi = as.factor(rep(landsvaedi, length(bias_list[[i]]))),
    Year = start_year + (0:(length(bias_list[[i]]) - 1)),  # Mapping index to year
    Bias = bias_list[[i]]
  )
  
  
  bias_data <- rbind(bias_data, temp_df)
}

ggplot(bias_data, aes(x = Year, y = Bias, group = Landsvaedi, color = Landsvaedi)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Bias over Time by Landsvaedi",
       x = "Year",
       y = "Bias",
       color = "Landsvaedi")

# You can see that model 1 predicts the count inaccurately for Landsvaedi 1 & 1.5
# and for a few specific years 
# The bias is highest for landsvaedi 1 and 1.5 for the year 2020
# COVID?
# Would be better if the landsvaedi was more divided for the largest categories 1 and 1.5




# fit the model with 1 and 1.5 alone and then the rest in another model
# would the coefficients change?  

data3 <- data %>% 
  filter(Landsvaedi %in% c("0","1", "1.5"))
data3$log_Count = log(data3$Count)
log_linear_model3 <- lm(log_Count ~ Year_Since_2010 + as.factor(Landsvaedi) + offset(log(Population)), data = data3)
summary(log_linear_model3)
vif(log_linear_model3)
sjPlot::tab_model(log_linear_model3)


data4 <- data %>% 
  filter(!Landsvaedi %in% c( "1", "1.5"))
data4$log_Count = log(data4$Count)
log_linear_model4 <- lm(log_Count ~ Year_Since_2010 + as.factor(Landsvaedi) + offset(log(Population)), data = data4)
summary(log_linear_model4)
vif(log_linear_model4)
sjPlot::tab_model(log_linear_model4)
