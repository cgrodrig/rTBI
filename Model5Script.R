library(rjags)
library(INLA)
library(coda)
library(dplyr)
library(ggplot2)
library(tidyr)

load("data_large.RData")


#Set Up

# convert factors to numeric (should consider onehot encoding later)
data_large$response <- as.integer(as.character(data_large$response))
data_large$age.a <- as.integer(data_large$age.a)
data_large$sex.s <- as.integer(data_large$sex.s)
data_large$arrival.h <- as.integer(data_large$arrival.h)
data_large$carepathway.c <- as.integer(data_large$carepathway.c)
data_large$monthyear <- as.integer(data_large$monthyear)
data_large$missing_GCS <- as.integer(data_large$missing_GCS)

# randomeffects
num_monthyears <- length(unique(data_large$monthyear))-1
num_postalcodes <- length(unique(data_large$postalcode))-1

unique_postalcodes <- sort(unique(data_large$postalcode))
postal_code_index <- match(data_large$postalcode, unique_postalcodes)

N = nrow(data_large)
iterations = 40000
chains = 4
burnin = 10000



-----------------------------------------
  # WORKING RANDOM EFFECTS MODEL
  system.time({
    modelString <- "
model {
  for (i in 1:N) {
    logit(p[i]) <- alpha + beta_a[age.a[i]] + beta_s[sex.s[i]] + beta_h[arrival.h[i]]
                 + beta_c[carepathway.c[i]]+  beta_mg[missing_GCS[i]]
                  + beta_t * staylength.t[i] + u_postal[postal_code[i]]

    response[i] ~ dbern(p[i])
  }


  #PRIORS
  alpha ~ dnorm(-3.904641784, 0.01) 
  #not too informative
  # It implies a variance of 100 (1 / 0.01). 
  # a low precision/high variance 
  # not overly constrain the model with strong prior belief
  
  beta_a[1] <- 0  # Reference category for 18-65
  #  assume these categories have no effect relative to other categories
  beta_a[2] ~ dt(1.917917860 , 1 / (0.46512853 ^ 2), 3)
  # hardcode these instead of extracting because of time
  # scale factor is already multiplied by 2 before inserting
  
  beta_s[1] <- 0  # Reference category for female
  beta_s[2] ~ dt(0.678610150, 1 / (0.42736793 ^ 2), 3)
  
  beta_h[1] <- 0 # Reference category for ambulance
  beta_h[2] ~ dt(-2.928134722, 1 / (1.43919384 ^ 2), 3) # fmaily
  beta_h[3] ~ dt(-2.262671348, 1 / (0.80273865 ^ 2), 3) # other
  
  beta_c[1] <- 0  # ER is level 1
  beta_c[2] ~ dt(-0.65809, 1 / (0.63896 ^ 2), 3)
  beta_c[3] ~ dt(-16.95895, 1 / (4803.20459 ^ 2), 3)
  beta_c[4] ~ dt(0, 1, 7)

  beta_mg[2] <- 0 # GCS missing = 0
  beta_mg[1] ~ dt(0.440645471, 1 / (1.23439354 ^ 2), 3)
  
  beta_t ~ dnorm(0, 0.01)

  # Random effects for postal codes
  for (p in 1:num_postalcodes) {
    u_postal[p] ~ dnorm(0, tau_postal)
  }

  tau_postal ~ dgamma(0.01, 0.01)
  

 
}
"
    
    data_jags <- list(
      N = nrow(data_large),
      response = data_large$response,
      age.a = data_large$age.a,
      sex.s = data_large$sex.s,
      arrival.h = data_large$arrival.h,
      carepathway.c = data_large$carepathway.c,
      staylength.t = data_large$staylength.t,
      postal_code = postal_code_index,
      missing_GCS = data_large$missing_GCS,
      num_postalcodes = length(unique_postalcodes)
      
    )
    params <- c("alpha", "beta_a[2]", "beta_s[2]", 
                "beta_h[2]", "beta_h[3]", "beta_c[2]", 
                "beta_c[3]", "beta_mg[1]",
                "beta_t", "u_postal", "tau_postal")
    
    model5 <- jags.model(textConnection(modelString), data = data_jags, n.chains = chains)
    update(model5, burnin)
    
    samples_model5 <- coda.samples(model5, variable.names = params, n.iter = iterations)
    
    
    
    summary(samples_model5)
    saveRDS(samples_model5, "samples_model5_1.rds")
    
    samples_combined_model5 <- rbind(samples_model5[[1]], samples_model5[[2]], samples_model5[[3]], samples_model5[[4]])
    samples_4chains_combined_model5 <- saveRDS(samples_combined_model5, "samples_model5_4bind.rds")
    
  })


samples1 <- readRDS("samples_model5_1.RDS")
samples <- readRDS("samples_model5_4bind.rds")

alpha <- samples[,1]
beta_a2 <- samples[,2]
beta_c2 <- samples[,3]
beta_c3 <- samples[,4]
beta_h2 <- samples[,5]
beta_h3  <- samples[,6]  
beta_mg1 <- samples[,7]  
beta_s2  <- samples[,8] 
beta_t  <- samples[,9]
tau_postal <- samples[,10]  
u_postal1  <- samples[,11]
u_postal2  <- samples[,12]
u_postal3<- samples[,13]
u_postal4<- samples[,14]
u_postal5<- samples[,15]
u_postal6<- samples[,16]
u_postal7<- samples[,17]
u_postal8<- samples[,18]
u_postal9<- samples[,19]
u_postal10<- samples[,20]
u_postal11  <- samples[,21]
u_postal12  <- samples[,22]
u_postal13<- samples[,23]
u_postal14<- samples[,24]
u_postal15<- samples[,25]
u_postal16<- samples[,26]
u_postal17<- samples[,27]
u_postal18<- samples[,28]
u_postal19<- samples[,29]
u_postal20<- samples[,30]

summary_alpha <- c(mean = mean(alpha),
                          sd = sd(alpha),
                          median = median(alpha),
                          CI_2.5 = quantile(alpha, probs = 0.025),
                          CI_97.5 = quantile(alpha, probs = 0.975))
summary_beta_a2 <- c(mean = mean(beta_a2),
                   sd = sd(beta_a2),
                   median = median(beta_a2),
                   CI_2.5 = quantile(beta_a2, probs = 0.025),
                   CI_97.5 = quantile(beta_a2, probs = 0.975))

summary_beta_c2 <- c(mean = mean(beta_c2),
                   sd = sd(beta_c2),
                   median = median(beta_c2),
                   CI_2.5 = quantile(beta_c2, probs = 0.025),
                   CI_97.5 = quantile(beta_c2, probs = 0.975))

summary_beta_c3 <- c(mean = mean(beta_c3),
                          sd = sd(beta_c3),
                          median = median(beta_c3),
                          CI_2.5 = quantile(beta_c3, probs = 0.025),
                          CI_97.5 = quantile(beta_c3, probs = 0.975))

summary_beta_h2 <- c(mean = mean(beta_h2),
                   sd = sd(beta_h2),
                   median = median(beta_h2),
                   CI_2.5 = quantile(beta_h2, probs = 0.025),
                   CI_97.5 = quantile(beta_h2, probs = 0.975))

summary_beta_h3 <- c(mean = mean(beta_h3),
                   sd = sd(beta_h3),
                   median = median(beta_h3),
                   CI_2.5 = quantile(beta_h3, probs = 0.025),
                   CI_97.5 = quantile(beta_h3, probs = 0.975))

summary_beta_mg1 <- c(mean = mean(beta_mg1),
                          sd = sd(beta_mg1),
                          median = median(beta_mg1),
                          CI_2.5 = quantile(beta_mg1, probs = 0.025),
                          CI_97.5 = quantile(beta_mg1, probs = 0.975))

summary_beta_s2 <- c(mean = mean(beta_s2),
                   sd = sd(beta_s2),
                   median = median(beta_s2),
                   CI_2.5 = quantile(beta_s2, probs = 0.025),
                   CI_97.5 = quantile(beta_s2, probs = 0.975))

summary_beta_t <- c(mean = mean(beta_t),
                   sd = sd(beta_t),
                   median = median(beta_t),
                   CI_2.5 = quantile(beta_t, probs = 0.025),
                   
                   CI_97.5 = quantile(beta_t, probs = 0.975))

summary_tau_postal <- c(mean = mean(tau_postal),
                          sd = sd(tau_postal),
                          median = median(tau_postal),
                          CI_2.5 = quantile(tau_postal, probs = 0.025),
                          CI_97.5 = quantile(tau_postal, probs = 0.975))

summary_u_postal1 <- c(mean = mean(u_postal1),
                   sd = sd(u_postal1),
                   median = median(u_postal1),
                   CI_2.5 = quantile(u_postal1, probs = 0.025),
                   CI_97.5 = quantile(u_postal1, probs = 0.975))
summary_u_postal2 <- c(mean = mean(u_postal2),
                   sd = sd(u_postal2),
                   median = median(u_postal2),
                   CI_2.5 = quantile(u_postal2, probs = 0.025),
                   CI_97.5 = quantile(u_postal2, probs = 0.975))
summary_u_postal3 <- c(mean = mean(u_postal3),
                          sd = sd(u_postal3),
                          median = median(u_postal3),
                          CI_2.5 = quantile(u_postal3, probs = 0.025),
                          CI_97.5 = quantile(u_postal3, probs = 0.975))

summary_u_postal4 <- c(mean = mean(u_postal4),
                   sd = sd(u_postal4),
                   median = median(u_postal4),
                   CI_2.5 = quantile(u_postal4, probs = 0.025),
                   CI_97.5 = quantile(u_postal4, probs = 0.975))

summary_u_postal5 <- c(mean = mean(u_postal5),
                   sd = sd(u_postal5),
                   median = median(u_postal5),
                   CI_2.5 = quantile(u_postal5, probs = 0.025),
                   CI_97.5 = quantile(u_postal5, probs = 0.975))

summary_u_postal6 <- c(mean = mean(u_postal6),
                          sd = sd(u_postal6),
                          median = median(u_postal6),
                          CI_2.5 = quantile(u_postal6, probs = 0.025),
                          CI_97.5 = quantile(u_postal6, probs = 0.975))

summary_u_postal7 <- c(mean = mean(u_postal7),
                   sd = sd(u_postal7),
                   median = median(u_postal7),
                   CI_2.5 = quantile(u_postal7, probs = 0.025),
                   CI_97.5 = quantile(u_postal7, probs = 0.975))

summary_u_postal8<- c(mean = mean(u_postal8),
                   sd = sd(u_postal8),
                   median = median(u_postal8),
                   CI_2.5 = quantile(u_postal8, probs = 0.025),
                   CI_97.5 = quantile(u_postal8, probs = 0.975))

summary_u_postal9 <- c(mean = mean(u_postal9),
                          sd = sd(u_postal9),
                          median = median(u_postal9),
                          CI_2.5 = quantile(u_postal9, probs = 0.025),
                          CI_97.5 = quantile(u_postal9, probs = 0.975))

summary_u_postal10 <- c(mean = mean(u_postal10),
                   sd = sd(u_postal10),
                   median = median(u_postal10),
                   CI_2.5 = quantile(u_postal10, probs = 0.025),
                   CI_97.5 = quantile(u_postal10, probs = 0.975))

summary_u_postal11 <- c(mean = mean(u_postal11 ),
                   sd = sd(u_postal11 ),
                   median = median(u_postal11 ),
                   CI_2.5 = quantile(u_postal11 , probs = 0.025),
                    CI_97.5 = quantile(u_postal11 , probs = 0.975))

summary_u_postal12 <- c(mean = mean(u_postal12 ),
                        sd = sd(u_postal12 ),
                        median = median(u_postal12 ),
                        CI_2.5 = quantile(u_postal12 , probs = 0.025),
                        CI_97.5 = quantile(u_postal12 , probs = 0.975))

summary_u_postal13 <- c(mean = mean(u_postal13 ),
                        sd = sd(u_postal13 ),
                        median = median(u_postal13 ),
                        CI_2.5 = quantile(u_postal13 , probs = 0.025),
                        CI_97.5 = quantile(u_postal13 , probs = 0.975))

summary_u_postal14 <- c(mean = mean(u_postal14  ),
                        sd = sd(u_postal14  ),
                        median = median(u_postal14  ),
                        CI_2.5 = quantile(u_postal14  , probs = 0.025),
                        CI_97.5 = quantile(u_postal14  , probs = 0.975))


summary_u_postal15 <- c(mean = mean(u_postal15 ),
                        sd = sd(u_postal15 ),
                        median = median(u_postal15 ),
                        CI_2.5 = quantile(u_postal15 , probs = 0.025),
                        CI_97.5 = quantile(u_postal15 , probs = 0.975))

summary_u_postal16 <- c(mean = mean(u_postal16 ),
                        sd = sd(u_postal16 ),
                        median = median(u_postal16 ),
                        CI_2.5 = quantile(u_postal16 , probs = 0.025),
                        CI_97.5 = quantile(u_postal16 , probs = 0.975))

summary_u_postal17 <- c(mean = mean(u_postal17 ),
                        sd = sd(u_postal17 ),
                        median = median(u_postal17 ),
                        CI_2.5 = quantile(u_postal17 , probs = 0.025),
                        CI_97.5 = quantile(u_postal17 , probs = 0.975))

summary_u_postal18 <- c(mean = mean(u_postal18 ),
                        sd = sd(u_postal18 ),
                        median = median(u_postal18 ),
                        CI_2.5 = quantile(u_postal18 , probs = 0.025),
                        CI_97.5 = quantile(u_postal18 , probs = 0.975))

summary_u_postal19 <- c(mean = mean(u_postal19 ),
                        sd = sd(u_postal19 ),
                        median = median(u_postal19 ),
                        CI_2.5 = quantile(u_postal19 , probs = 0.025),
                        CI_97.5 = quantile(u_postal19 , probs = 0.975))

summary_u_postal20 <- c(mean = mean(u_postal20 ),
                        sd = sd(u_postal20 ),
                        median = median(u_postal20 ),
                        CI_2.5 = quantile(u_postal20 , probs = 0.025),
                        CI_97.5 = quantile(u_postal20 , probs = 0.975))


summaries <- data.frame(
  Parameter = c("alpha","beta_a2","beta_c2","beta_c3","beta_h2",
                "beta_h3","beta_mg1","beta_s2" ,"beta_t",   
                "u_postal1",  "u_postal2",  "u_postal3",  "u_postal4",  
                "u_postal5",  "u_postal6" ,
                 "u_postal7",  "u_postal8",  "u_postal9" , 
                "u_postal10", "u_postal11", "u_postal12" ,"u_postal13", "u_postal14",
                "u_postal15", "u_postal16", "u_postal17", "u_postal18", "u_postal19", "u_postal20"),
  Mean = c(summary_alpha["mean"], summary_beta_a2["mean"], summary_beta_c2["mean"], summary_beta_c3["mean"], summary_beta_h2["mean"],
           summary_beta_h3["mean"],summary_beta_mg1["mean"],summary_beta_s2["mean"] ,summary_beta_t["mean"],   
           summary_u_postal1["mean"],  summary_u_postal2["mean"],  summary_u_postal3["mean"],  summary_u_postal4["mean"],  
           summary_u_postal5["mean"],  summary_u_postal6["mean"] ,
           summary_u_postal7["mean"],  summary_u_postal8["mean"],  summary_u_postal9["mean"] , 
           summary_u_postal10["mean"], summary_u_postal11["mean"], summary_u_postal12["mean"] 
           , summary_u_postal13["mean"], summary_u_postal14["mean"],
           summary_u_postal15["mean"], summary_u_postal16["mean"], 
           summary_u_postal17["mean"], summary_u_postal18["mean"], summary_u_postal19["mean"], u_postal20["mean"]),
  SD = c(summary_alpha["sd"], summary_beta_a2["sd"], summary_beta_c2["sd"], summary_beta_c3["sd"], summary_beta_h2["sd"],
         summary_beta_h3["sd"],summary_beta_mg1["sd"],summary_beta_s2["sd"] ,summary_beta_t["sd"],   
         summary_u_postal1["sd"],  summary_u_postal2["sd"],  summary_u_postal3["sd"],  summary_u_postal4["sd"],  
         summary_u_postal5["sd"],  summary_u_postal6["sd"] ,
         summary_u_postal7["sd"],  summary_u_postal8["sd"],  summary_u_postal9["sd"] , 
         summary_u_postal10["sd"], summary_u_postal11["sd"], summary_u_postal12["sd"] , summary_u_postal13["sd"], summary_u_postal14["sd"],
         summary_u_postal15["sd"], summary_u_postal16["sd"], summary_u_postal17["sd"], summary_u_postal18["sd"], summary_u_postal19["sd"], summary_u_postal20["sd"]),
  Median = c(summary_alpha["median"], summary_beta_a2["median"], summary_beta_c2["median"], summary_beta_c3["median"], summary_beta_h2["median"],
             summary_beta_h3["median"],summary_beta_mg1["median"],summary_beta_s2["median"] ,summary_beta_t["median"],  
             summary_u_postal1["median"],  summary_u_postal2["median"],  summary_u_postal3["median"],  summary_u_postal4["median"],  
             summary_u_postal5["median"],  summary_u_postal6["median"] ,
             summary_u_postal7["median"],  summary_u_postal8["median"],  summary_u_postal9["median"] , 
             summary_u_postal10["median"], summary_u_postal11["median"], summary_u_postal12["median"] , 
             summary_u_postal13["median"], summary_u_postal14["median"],
             summary_u_postal15["median"], summary_u_postal16["median"], 
             summary_u_postal17["median"], summary_u_postal18["median"], summary_u_postal19["median"], summary_u_postal20["median"]),
  CI_2.5 = c(summary_alpha["CI_2.5.2.5%"], summary_beta_a2["CI_2.5.2.5%"], summary_beta_c2["CI_2.5.2.5%"], 
             summary_beta_c3["CI_2.5.2.5%"], summary_beta_h2["CI_2.5.2.5%"],
             summary_beta_h3["CI_2.5.2.5%"],summary_beta_mg1["CI_2.5.2.5%"],summary_beta_s2["CI_2.5.2.5%"] ,
             summary_beta_t["CI_2.5.2.5%"],   
             summary_u_postal1["CI_2.5.2.5%"],  summary_u_postal2["CI_2.5.2.5%"],  summary_u_postal3["CI_2.5.2.5%"],  summary_u_postal4["CI_2.5.2.5%"],  
             summary_u_postal5["CI_2.5.2.5%"],  summary_u_postal6["CI_2.5.2.5%"] ,
             summary_u_postal7["CI_2.5.2.5%"],  summary_u_postal8["CI_2.5.2.5%"],  summary_u_postal9["CI_2.5.2.5%"] , 
             summary_u_postal10["CI_2.5.2.5%"], summary_u_postal11["CI_2.5.2.5%"], summary_u_postal12["CI_2.5.2.5%"] , 
             summary_u_postal13["CI_2.5.2.5%"], summary_u_postal14["CI_2.5.2.5%"],
             summary_u_postal15["CI_2.5.2.5%"], summary_u_postal16["CI_2.5.2.5%"], summary_u_postal17["CI_2.5.2.5%"], 
             summary_u_postal18["CI_2.5.2.5%"], summary_u_postal19["CI_2.5.2.5%"], summary_u_postal20["CI_2.5.2.5%"]),
  CI_97.5 = c(summary_alpha["CI_97.5.97.5%"], summary_beta_a2["CI_97.5.97.5%"], summary_beta_c2["CI_97.5.97.5%"], 
              summary_beta_c3["CI_97.5.97.5%"], beta_h2["CI_97.5.97.5%"],
              summary_beta_h3["CI_97.5.97.5%"],summary_beta_mg1["CI_97.5.97.5%"],summary_beta_s2["CI_97.5.97.5%"] ,summary_beta_t["CI_97.5.97.5%"],
              summary_u_postal1["CI_97.5.97.5%"], summary_u_postal2["CI_97.5.97.5%"],  summary_u_postal3["CI_97.5.97.5%"],  
              summary_u_postal4["CI_97.5.97.5%"],  
              summary_u_postal5["CI_97.5.97.5%"],  summary_u_postal6["CI_97.5.97.5%"] ,
              summary_u_postal7["CI_97.5.97.5%"],  summary_u_postal8["CI_97.5.97.5%"],  summary_u_postal9["CI_97.5.97.5%"] , 
              summary_u_postal10["CI_97.5.97.5%"], summary_u_postal11["CI_97.5.97.5%"], summary_u_postal12["CI_97.5.97.5%"] , 
              summary_u_postal13["CI_97.5.97.5%"], 
              summary_u_postal14["CI_97.5.97.5%"],
              summary_u_postal15["CI_97.5.97.5%"], summary_u_postal16["CI_97.5.97.5%"], 
              summary_u_postal17["CI_97.5.97.5%"], summary_u_postal18["CI_97.5.97.5%"], summary_u_postal19["CI_97.5.97.5%"], summary_u_postal20["CI_97.5.97.5%"])
)
summaries
load("data_large.rdata")
model_glm <- glm(response ~  postalcode + age.a + sex.s + arrival.h  + carepathway.c + staylength.t + missing_GCS, 
                 data = data_large, family = binomial)

summary(model_glm)
sjPlot::tab_model(model_glm)
car::vif(model_glm)

glm_odds_df <- data.frame(
  Parameter = c("alpha","beta_a2","beta_c2","beta_c3","beta_h2",
                "beta_h3","beta_mg1","beta_s2" ,"beta_t",   
                "u_postal1",  "u_postal2",  "u_postal3",  "u_postal4",  
                "u_postal5",  "u_postal6" ,
                "u_postal7",  "u_postal8",  "u_postal9" , 
                "u_postal10", "u_postal11", "u_postal12" ,"u_postal13", 
                "u_postal14","u_postal15", "u_postal16", "u_postal17",
                "u_postal18", "u_postal19", "u_postal20"), 
  OddsRatio = c(0.02, 6.91, 0.53, 0, 0.05, 0.10, 1.56, 2.00, 1.00, 0.32, 0.43, 0.17,
                0.22, 0.43, 0.77, 0.12, 0, 0.76, 0.43, 0.67, 0.65, 0.27, 0.20, 
                0.23, 0.58, 0.46, 0.47, 0.50, 0.09),  
  LowerCI = c(0.00, 4.42, 0.16, 0.00, 0.01, 0.04, 0.54, 1.32, 0.98, 0.02, 0.11, 
              0.01, 0.01, 0.12, 0.27, 0.01, 0, 0.23, 0.06, 0.21, 0.20,0.04, 0.05, 
              0.06, 0.19, 0.11, 0.14, 0.13, 0),
  
  UpperCI = c(0.01, 11.05, 1.61, 8.72, 0.17, 0.21, 6.66, 3.06, 1.01, 2.28, 1.72, 
              1.16, 1.62,1.62, 2.57, 0.82, 0, 2.77, 2.29, 2.40, 2.31, 1.42, 0.85, 
              0.92, 2.04, 1.83, 1.70, 1.93, 0.63),
  Type = "GLM"
)

bayesian_odds_df <- data.frame(
  Parameter = c("alpha","beta_a2","beta_c2","beta_c3","beta_h2",
                "beta_h3","beta_mg1","beta_s2" ,"beta_t",   
                "u_postal1",  "u_postal2",  "u_postal3",  "u_postal4",  
                "u_postal5",  "u_postal6" ,
                "u_postal7",  "u_postal8",  "u_postal9" , 
                "u_postal10", "u_postal11", "u_postal12" ,"u_postal13", 
                "u_postal14","u_postal15", "u_postal16", "u_postal17",
                "u_postal18", "u_postal19", "u_postal20"), 
  OddsRatio = exp(summaries$Median),  
  LowerCI = exp(summaries$CI_2.5),
  UpperCI = exp(summaries$CI_97.5),
  Type = "GLM"
)


bayesian_odds_df <- bayesian_odds_df %>%
  mutate(Type = "Bayesian")

combined_df <- rbind(
  glm_odds_df,
  bayesian_odds_df %>% select(Parameter, OddsRatio, LowerCI, UpperCI, Type)
)


ggplot(combined_df, aes(x = Parameter, y = OddsRatio, color = Type)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1, position = position_dodge(width = 0.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Parameter", y = "Odds Ratio", title = "Comparison of Bayesian and GLM Estimates") +
  scale_color_manual(values = c("blue", "red"))