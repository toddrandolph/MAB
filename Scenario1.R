### Thompson Sampling -- Scenario 1 ###

library(tidyverse)

# assume we know the real CTRs for each ad
actual_ctr <- seq(0.01, 0.13, by = 0.03)

# number of trails (impressions)
n <- 5000

# starting lists (starting w/ equal priors)
priors <- rep(list(c(1,1)),5) # c(alpha,beta)
impressions <- rep(list(0),5)
clicks <- rep(list(0),5)


## run simulation ##

for(i in 1:n){
  
  # randomly sample an exp. CTR from each prior
  exp_CTR <- sapply(priors, function(x) rbeta(1, x[1], x[2]))
  
  # select ad with largest exp. CTR
  ad <- which(exp_CTR == max(exp_CTR))
  
  # add an impression
  impressions[[ad]] <- impressions[[ad]] + 1
  
  # randomly select click or no click
  click <- sample(c(1,0), 1, prob = c(actual_ctr[ad], 1-actual_ctr[ad]))
  
  #update prior
  if(click == 1){
    priors[[ad]][1] <- priors[[ad]][1] + 1
    clicks[[ad]] <- clicks[[ad]] + 1
  } else {
    priors[[ad]][2] <- priors[[ad]][2] + 1
  }    
  
  impressions[[ad]] <- impressions[[ad]] + 1
  
}


Ind_Results <- data.frame(Ad = c(1:5),
                          Clicks = unlist(clicks), 
                          Impressions = unlist(impressions)) %>% 
  mutate(CTR = Clicks/Impressions,
         ShowPrct = Impressions/sum(Impressions))

Total_Results <- Ind_Results %>% 
  summarize(Total_Clicks = sum(Clicks),
            Total_Impressions = sum(Impressions)) %>% 
  mutate(Overall_CTR = Total_Clicks/Total_Impressions)


## Distributions Plots ##
x <- seq(0,.4, length = 100)

standardized_priors <- lapply(priors, function(y) dbeta(x, y[1], y[2])/max(dbeta(x, y[1], y[2])))

plot(x, standardized_priors[[1]], 
     ylab = "probability density", type = "l", col = 1)

for(i in 2:length(priors)){
  
  lines(x, standardized_priors[[i]],
        type = "l", col = i)
  
}
