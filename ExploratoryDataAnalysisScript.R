# LIBRARY
#### dataload
library(readxl)
library(dplyr)
library(openxlsx)
#### datawrangling
library(stringr)
library(lubridate)
#### visualization
library(ggplot2)
#### EDA
library(tidyr)
library(DataExplorer)
library(broom) # for OR
library(car) # for vif
library(tseries)
library(forcats) # for fct_other

#### Other
library(htmltools)
# Clustering
library(stats)
library(factoextra)
library(caret)
#### Modeling
library(glmnet)
library(randomForest)
#### Analysis
library(pROC) 


load("data_large.RData")
dim(data_large)
load("data_small.RData")
dim(data_small)

# Exploratory Data Analysis
#2D
#plot_missing(data_large)

### time 
hist(data_large$staylength.t, main = "Stay Length Distribution", xlab = "Stay Length")

data_plot <- data.frame(monthyear = data_large$monthyear, response = data_large$response)

ggplot(data_plot, aes(x = monthyear, fill = factor(response))) +
  geom_bar() +
  labs(x = "Month-Year", y = "Frequency", fill = "Response") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


data_plot <- data.frame(QuarterYear = data_large$quarteryear, response = data_large$response)

ggplot(data_plot, aes(x = QuarterYear, fill = factor(response))) +
  geom_bar() +
  labs(x = "Quarter-Year", y = "Frequency", fill = "Response") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### age
age_response_counts <- table(data_large$age.a, data_large$response)
barplot(age_response_counts, beside=TRUE, legend=TRUE, main="Response Count by Age Category", xlab="Age Category", ylab="Count")
age_response_counts

# The 65+ age group have the least number of patients yet they are more likely to die within 90 days. 

ggplot(data_large, aes(x = age.a, fill = factor(response))) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  ggtitle("Proportion of Response = 1 by Age Category")


### sex
sex_response_counts <- table(data_large$sex.s, data_large$response)
barplot(sex_response_counts, beside=TRUE, legend=TRUE, main="Response Count by Sex Category", xlab="Sex Category", ylab="Count")
sex_response_counts


### arrival
arrival_response_counts <- table(data_large$arrival.h, data_large$response)
barplot(arrival_response_counts, beside=TRUE, legend=TRUE, main="Response Count by Arrival Category", xlab="Arrival Category", ylab="Count")
arrival_response_counts


### cause
cause_response_counts <- table(data_large$cause.r, data_large$response)
barplot(cause_response_counts, beside=TRUE, legend=TRUE, main="Response Count by Cause Category", xlab="Cause Category", ylab="Count")
cause_response_counts


### place
place_response_counts <- table(data_large$place.w, data_large$response)
barplot(place_response_counts, beside=TRUE, legend=TRUE, main="Response Count by Place Category", xlab="Place Category", ylab="Count")
place_response_counts



### carepathways
carepathway_response_counts <- table(data_large$carepathway.c, data_large$response)
barplot(carepathway_response_counts, beside=TRUE, legend=TRUE, main="Response Count by Carepathway Category", xlab="Carepathway Category", ylab="Count")
carepathway_response_counts


### time
boxplot(data_large$staylength.t ~ data_large$response, 
        main="Stay Length by Response", 
        xlab="Response", 
        ylab="Stay Length", 
        names=c("Category 0", "Category 1"))




# 3D
# Y = Proportion of the Response  + 2 parameters
# faceted
ggplot(data_large, aes(x=age.a, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Age & Sex")


ggplot(data_large, aes(x=monthyear, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Monthyear & Sex")

ggplot(data_large, aes(x=quarteryear, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Quarteryear & Sex")

ggplot(data_large, aes(x=postalcode, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Postalcode & Sex")

ggplot(data_large, aes(x=arrival.h, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Arrival & Sex")

ggplot(data_large, aes(x=place.w, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Place & Sex")


ggplot(data_large, aes(x=carepathway.c, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Carepathway & Sex")

ggplot(data_large, aes(x= missing_GCS, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Missing GCS & Sex")


ggplot(data_large, aes(x= missing_cause, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Missing Cause & Sex")

ggplot(data_large, aes(x= missing_arrival, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Missing Arrival & Sex")

ggplot(data_large, aes(x= missing_carepathway, fill=response)) +
  geom_bar(position = "fill") + 
  facet_wrap(~sex.s) + 
  labs(y = "Proportion", title="Proportion of Response by Missing Carepathway & Sex")


ggplot(data_large, aes(x=age.a, fill=response)) + 
  geom_bar(position="fill") + 
  facet_grid(sex.s ~ carepathway.c) + 
  labs(y="Proportion", title = "Response by Age, Sex, and Carepathway") + 
  theme_minimal()

ggplot(data_large, aes(x=age.a, fill=response)) + 
  geom_bar(position="fill") + 
  facet_grid(sex.s ~ carepathway.c) + 
  labs(y="Proportion", title = "Response by Age, Sex, and Carepathway") + 
  theme_minimal()


# The above EDA has lead to some changes:
# 1. There are 2 deaths for the age group 0-17 so this age group will be removed from the model
table(data_large$age.a, data_large$response)
data_large <- data_large[data_large$age.a != "0-17", ]
data_small <- data_small[data_small$age.a != "0-17", ]
data_large$age.a <- droplevels(data_large$age.a)
data_small$age.a <- droplevels(data_small$age.a)

# Before GLM
# Check the str
str(data_large)
str(data_small)


# postalcode update as well since there are less than 50 in some categories now
# postal_code_counts <- table(data_large$postalcode)
# less_than_50 <- names(postal_code_counts[postal_code_counts < 50])
# data_large$postalcode <- ifelse(data_large$postalcode %in% less_than_50, ">50", data_large$postalcode)
# table(data_large$postalcode)
postal_code_counts <- table(data_large$postalcode)
less_than_50 <- names(postal_code_counts[postal_code_counts < 50])
data_large <- data_large %>%
  mutate(postalcode = ifelse(postalcode %in% less_than_50, ">50", as.character(postalcode))) %>%
  mutate(postalcode = factor(postalcode, levels = unique(c(as.character(postalcode), ">50"))))
table(data_large$postalcode)
str(data_large$postalcode)

# DO THE SAME FOR data_small? ONLY CHOOSE THE EXACT POSTALCODES THE REST ARE "OTHER"
keep_codes <- c("110", "203", "170", "109", "105", "221", "210", "113",
                "108", "104", "270", "112", ">50", "200", "101", "107", "220", "201")

data_small <- data_small %>%
  mutate(postalcode = fct_other(postalcode, keep = keep_codes, other_level = ">50"))
table(data_small$postalcode)


# GLM 1
str(data_large)
model_glm <- glm(response ~  postalcode + age.a + sex.s + arrival.h + 
cause.r + place.w  + staylength.t + missing_GCS, 
 data = data_large, family = binomial)

summary(model_glm)
sjPlot::tab_model(model_glm)
car::vif(model_glm)

# 2
# The ambulance variable is the only category of the arrival methods that have any significance, 
# but the family category is showing some significance, too so let's try having just 3 groups
# family, ambulance, and other
data_small$arrival.h <- as.character(data_small$arrival.h)
data_small <- data_small %>%
  mutate(arrival.h = case_when(
    arrival.h == "Ambulance Service" ~ "Ambulance",
    arrival.h == "Family" ~ "Family",
    TRUE ~ "Other"
  ))
data_small$arrival.h <- as.factor(data_small$arrival.h)


data_large$arrival.h <- as.character(data_large$arrival.h)
data_large <- data_large %>%
  mutate(arrival.h = case_when(
    arrival.h == "Ambulance Service" ~ "Ambulance",
    arrival.h == "Family" ~ "Family",
    TRUE ~ "Other"
  ))
data_large$arrival.h <- as.factor(data_large$arrival.h)


# GLM2
model_glm <- glm(response ~  postalcode + age.a + sex.s + arrival.h + 
                   cause.r + place.w  + staylength.t + missing_GCS, 
                 data = data_large, family = binomial)

summary(model_glm)
sjPlot::tab_model(model_glm)
car::vif(model_glm)

# 3 The cause variable Aand the place variable are correlated with each other, so 1 needs to be removed. 
table(data_large$cause.r)
table(data_large$place.w)

# The unknown category in the place variable is almost 3x less than the cause parameter 
# so we will keep place due to less missing data. 

model_glm <- glm(response ~  postalcode + age.a + sex.s + arrival.h + 
                   place.w  + staylength.t + missing_GCS, 
                 data = data_large, family = binomial)

summary(model_glm)
sjPlot::tab_model(model_glm)
car::vif(model_glm)

#4 The place variable shows no signs of improvement. There is no significance coming from that parameter. 
# It is best to remove it. 
model_glm <- glm(response ~  postalcode + age.a + sex.s + arrival.h  + staylength.t + missing_GCS, 
                 data = data_large, family = binomial)

summary(model_glm)
sjPlot::tab_model(model_glm)
car::vif(model_glm)
coefficients1 <- coef(model_glm)
names(coefficients1) <- names(coef(model_glm))
se_large <- summary(model_glm)$coefficients[, "Std. Error"]

## EXTRACT THESE FOR THE RJAGS MODEL
coefficients <- coef(summary(model_glm))
means <- coefficients[, "Estimate"]
scales <- coefficients[, "Std. Error"] * 2  # Scale is twice the standard error
save(means, file = "means.RData")
save(scales, file = "scales.RData")
## 


# data_small
# Previously we were using data_large for the analysis without the variable carepathway which has a lot of missing data. 
# The reason for doing this was to get the estimates of the coeffients from the GLM as starting values for the parameters
# in the Bayesian model 
# It was better to have a large dataset to get more accurate coefficient estimates
# however, we do need an estimate for the carepathway variable so I will run a few more GLMs below to get those values


model_glm <- glm(response ~  postalcode + age.a + sex.s + arrival.h  + staylength.t + missing_GCS, 
                 data = data_small, family = binomial)

summary(model_glm)
sjPlot::tab_model(model_glm)
car::vif(model_glm)
coefficients2 <- coef(model_glm)
names(coefficients2) <- names(coef(model_glm))
se_small <- summary(model_glm)$coefficients[, "Std. Error"]


# comparison tables of the last two glms
# Estimates
df_large <- data.frame(Coefficient = coefficients1, Predictor = names(coefficients1))
df_small <- data.frame(Coefficient = coefficients2, Predictor = names(coefficients2))
comparison_data <- full_join(df_large, df_small, by = "Predictor", suffix = c("_large", "_small"))
comparison_data$Predictor <- gsub("postalcode", "", comparison_data$Predictor)

comparison_data <- comparison_data %>% 
   replace(is.na(.), 0)  # Replace NA with 0 or another appropriate value

fig1 <- ggplot(comparison_data, aes(x = Coefficient_large, y = Coefficient_small, label = Predictor)) +
   geom_point() +
  geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 0.07, size = 5) + 
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
   labs(x = "Coefficients from data_large", y = "Coefficients from data_small",
        title = "Comparison of GLM Coefficients") +
   theme_minimal()
fig1

# Main take aways: 
# The scatterplot compares the coefficients from 2 different models by plotting them 
# against each other. Each point represents a parameter with x being the data_large 
# coeff and y being the data_small coeff
# The red line would be the point where the coeff from both models are equal
# despite the difference in data size or missingness
#  The cluster around the red line shows that these estimates are relatively
# similar across both datasets. These findings are stable estimates and indicate
# that the findings are robust to changes in the dataset size 
# However the cluster up in the top, indicate parameters with substantial
# discrepancies in their estimates between the two datasets. 

# Standard Error
se_large_df <- data.frame("Standard Error" = se_large, Predictor = names(coefficients1))
se_small_df <- data.frame("Standard Error" = se_small, Predictor = names(coefficients2))
se_comparison_data <- full_join(se_large_df, se_small_df, by = "Predictor", suffix = c("_large", "_small"))
se_comparison_data$Predictor <- gsub("postalcode", "", se_comparison_data$Predictor)

se_comparison_data <- se_comparison_data %>% 
  replace(is.na(.), 0)  # Replace NA with 0 or another appropriate value

fig2 <- ggplot(se_comparison_data, aes(x = Standard.Error_large, y = Standard.Error_small, label = Predictor)) +
  geom_point() +
  geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 0.07) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Standard Errors from data_large", y = "Standard Errors from data_small",
       title = "Comparison of Standard Errors") +
  theme_minimal()
fig2

# PRINTING
ggsave("comparison_coeff_estimates.png", plot = fig1)
ggsave("comparison_standard_errors.png", plot = fig2)
write.csv(se_comparison_data, "standard_errors_comparison.csv", row.names = FALSE)
write.csv(comparison_data, "coefficients_comparison.csv", row.names = FALSE)



str(data_large)
levels(data_large$postalcode)
levels(data_large$age.a)
levels(data_large$sex.s)
levels(data_large$arrival.h)
levels(data_large$missing_GCS)

#### Carepathway estimate is taken from this model

model_glm <- glm(response ~  postalcode + age.a + carepathway.c + sex.s + arrival.h  + staylength.t + missing_GCS, 
                 data = data_small, family = binomial)

summary(model_glm)
sjPlot::tab_model(model_glm)
car::vif(model_glm)
coefficients3 <- coef(model_glm)



# Incorporating the info from the GLMs into the priors. 
# inform the choice of the shape and scale of the prior distributions 
# Use the estimates from the GLM as starting values for the parameters in the prior distributions 
# Most priors in the model are tdistributed
# The parameters that need to be decided apon for the priors are mean, scale, and df

# PLAN: 
# Mean: Use the coeff est from data_large glm since it is a more representative dataset
# Scale: Use the standard errors from data_large glm since this parameter 
# represents the uncertainty around the estimate
# df: controls the heavyness of the tails (moderate tail weight is 3-7); 
# increase tail weight if outliers are expected or if the sample size is small 





save(data_large, file = "data_large.RData")
save(data_small, file = "data_small.RData")
















# Validate the assumption of using an AR1 for the monthyear effects in the model 
# autocorrelation in the residuals of the original data
# any correlation between observations at different time lags?

# expectation: the autocorrelation at lag1 should be significant far beyond the threshold and 
# gradual decrease as the lags increase further validates this assumption to use AR1
# spikes at higher lags would indicate seasonal effects (if there is a regular pattern) or other types of 
# autocorrelation that an AR1 is not capturing

residuals_glm <- residuals(model_glm, type = "response")
acf(residuals_glm, main = " ACF of Residuals", lag.max = 168)


# Validate the assumption of using a RANDOMWALK for the monthyear effects in the model 
# RandomWalk implies non-stationary process, where changes from 1 period to the next are important
# characterized by nonstationarity 
# each value typically differes from the previous by a random error
# so, I need to look at the 1st differncce of the residuals
difference_residuals <- diff(residuals_glm)
# now check if the series is stationary using the dickey fuller test 
adf_test <- adf.test(difference_residuals, alternative = 
                       "stationary")
adf_test

# we need the non differenced data to be nonstationary and the difference data to be stationary
# the dickey fuller test shows strong evidence that the residuals are stationary
# because of the -27.422 DF statistics & small pvalue
# this means the differenced residuals do not have a unit root so they are stationary
# we could use a random walk model for the monthyear effects in the data ACF plot plays out 

acf(difference_residuals, main = "ACF of Difference residuals")

#The acf plot should show white noise. All lags should have 0 autocorrelation showing
#THis would prove that the terms in the series is largely independent of the others. 
# THAT IS NOT THE CASE HERE!
# We see instead a significant lag at 1