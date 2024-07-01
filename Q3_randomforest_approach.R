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
library(randomForest)
library(gbm)

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
  dplyr::select(ID, `ar innskriftar`, Landsvaedi, Land,  
                averkaskor, Visit_Number, Aldurshopar, `kodi kyns`, `Forgangsflokkun a bradamottoku`) %>%
  rename(Year = `ar innskriftar`, 
         Gender = `kodi kyns`, 
         Urgency = `Forgangsflokkun a bradamottoku`) %>%
  filter(Gender != 0, 
         !is.na(Urgency)) %>%
  mutate(Year = as.numeric(Year))%>%
  filter(!is.na(Aldurshopar))

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
  mutate(Year = as.numeric(Year)) %>%
  dplyr::select(!`log10(allir)`)

# Merge visit data with population data
data <- BMT_bara_TBI_hs %>%
  inner_join(pop, by = c("Landsvaedi", "Year"))

# remember NO categorical variables, encode them
data <- data %>%
  mutate(
    Population = ifelse(Landsvaedi == 0, 100000, Population),
    Year_Since_2010 = Year - 2010,
    Landsvaedi3 = ifelse(
      Landsvaedi == 1,
      "Capital",
      ifelse(Landsvaedi == 1.5, "Surrounding Area", "Countryside")
    )
  ) %>%
  dplyr::select(!c(Landsvaedi, Year)) %>%
  mutate(Landsvaedi3 = as.factor(Landsvaedi3)) 
# save(data, file = "data_q3.RData")


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



# patterns across visits rather than individual-level effects if we remove the ID?
# data2 <- data %>%  
#   ungroup() %>%
#   dplyr::select(!ID)
# 
# set.seed(3008)
# rf_model2 <- randomForest(averkaskor ~ ., data = data2, importance = TRUE)
# print(rf_model2) 
# importance(rf_model2) 
# varImpPlot(rf_model2) 





# remember (pg. 340 txtbk ISLR)
# trees can be very non-robust
# by aggregating many decision trees, using methods like bagging,
# random forests, and boosting, the predictive performance of trees can be
# substantially improved


# bagging


# boosting 
# how gradient boosting compares to rf in predicting averkaskor?

# partial dependence plots (pg. 360)



# bayesian additive regression tree 