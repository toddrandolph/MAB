### MAB - Thompson Sampling ###

library(tidyverse)

# number of creatives
c <- 5

# assume we know the real CTRs for each ad
actual_ctr <- seq(0.01, 0.13, by = 0.03)

# number of trails (impressions)
n <- 5000

# starting lists (starting w/ equal priors)
priors <- rep(list(c(1,1)),c) # c(alpha,beta)
impressions <- rep(list(0),c)
clicks <- rep(list(0),c)
ctr <- rep(list(c(rep(0, n))),c)
regrets <- c()

## run simulation ##

for(i in 1:n){
  
  # randomly sample an exp. CTR from each prior
  exp_CTR <- sapply(priors, function(x) rbeta(1, x[1], x[2]))
  
  # select ad with largest exp. CTR
  ad <- which(exp_CTR == max(exp_CTR))
  
  # add an impression
  impressions[[ad]] <- impressions[[ad]] + 1
  
  # update regrets
  regret <- max(actual_ctr) - actual_ctr[[ad]]
  
  if(i == 1){
    regrets[i] <- regret
  } else {
    regrets[i] <- regrets[i-1] + regret
  }
  
  # randomly select click or no click
  click <- sample(c(1,0), 1, prob = c(actual_ctr[ad], 1-actual_ctr[ad]))
  
  #update prior
  if(click == 1){
    
    priors[[ad]][1] <- priors[[ad]][1] + 1
    clicks[[ad]] <- clicks[[ad]] + 1
    
  } else {
    
    priors[[ad]][2] <- priors[[ad]][2] + 1
  }    
  
  ctr[[ad]][i:n] <- clicks[[ad]]/impressions[[ad]]
  
}




Ind_Results <- data.frame(Ad = c(1:c),
                          Clicks = unlist(clicks), 
                          Impressions = unlist(impressions)) %>% 
  mutate(CTR = Clicks/Impressions,
         ShowPrct = Impressions/sum(Impressions))

Total_Results <- Ind_Results %>% 
  summarize(Total_Clicks = sum(Clicks),
            Total_Impressions = sum(Impressions)) %>% 
  mutate(Overall_CTR = Total_Clicks/Total_Impressions)


## plots ##
x1 <- seq(1,n,1)

# regret
qplot(x1, regrets, facets = T, xlab = "Trial", ylab = "Regret",
      main = "Cumulative Algorithm Regret", geom = "line")


# ctr
plot(x1, ctr[[1]], ylab = "CTR Values",
     xlab = "Trial", type = "l", col = 1,
     main = "Simulated CTR", ylim = c(0,.5))

for(i in 2:length(ctr)){
  
  lines(x1, ctr[[i]],
        type = "l", col = i)
  
}

legend(4200,.48, legend = paste("Ad", seq(1:c)), fill = seq(1:c))


# distributions
x2 <- seq(0,.25, length = 100)

standardized_priors <- lapply(priors, function(y) dbeta(x, y[1], y[2])/max(dbeta(x, y[1], y[2])))

plot(x2, standardized_priors[[1]], xlab = "CTR Values",
     ylab = "Probability Density", type = "l", col = 1,
     main = "Beta Distributions")

for(i in 2:length(priors)){
  
  lines(x2, standardized_priors[[i]],
        type = "l", col = i)
  
}

legend(.20,.95, legend = paste("Ad", seq(1:c)), fill = seq(1:c))
