### Dummy Population ###
library(tidyverse)

# Set up the population & run logistic regression to get realistic estimates of the parameters

# number of arms/creatives
k <- 2

# population size
N <- 1000000

# number of features
X <- 1

# range of values for each feature
X1 <- c(0,1)

                 #arm1 #arm2
probs <- matrix(c(0.2, 0.05,  # X1 = 0
                  0.05, 0.2), # X1 = 1
                nrow = length(X1),
                ncol = k, byrow = TRUE)

Pop <- data.frame(User = 1:N, Arm = sample(factor(rep(1:k, N/k)))) %>%
  group_by(Arm) %>%
  mutate(Feat1 = sample(X1, size = N/k, replace = T)) %>%
  ungroup

SubPop1 <- Pop %>%
  filter(Arm == 1) %>%
  mutate(Reward = ifelse(Feat1 == 0,
                         sample(c(1,0), size = n(), replace = T, prob = c(probs[1,1], 1 - probs[1,1])),
                         sample(c(1,0), size = n(), replace = T, prob = c(probs[2,1], 1 - probs[2,1])))) %>% 
  select(User, Reward, Feat1)

SubPop2 <- Pop %>%
  filter(Arm == 2) %>%
  mutate(Reward = ifelse(Feat1 == 0,
                         sample(c(1,0), size = n(), replace = T, prob = c(probs[1,2], 1 - probs[1,2])),
                         sample(c(1,0), size = n(), replace = T, prob = c(probs[2,2], 1 - probs[2,2])))) %>% 
  select(User, Reward, Feat1)

# rn logistic regression on SubPops
Ad1_model <- glm(Reward ~ Feat1, family = binomial, data = SubPop1) 
Ad2_model <- glm(Reward ~ Feat1, family = binomial, data = SubPop2)