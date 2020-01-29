### Dummy Population ###
library(tidyverse)

# Set up the population & run logistic regression to get realistic estimates of the parameters

# number of arms/creatives
bandits <- 3

# population size
size <- 1000000

# number of features
feats <- 4

# names of features 
feats_df <- data.frame(matrix(0, nrow = size, ncol = feats))
colnames(feats_df) <- paste0("X", rep(1:feats), separate = "")

# range of values for each feature
X1 <- c(0,1)

#Discrete Data            #arm1 #arm2
discrete_probs <- matrix(c(0.2, 0.05, 0.01,  # X1 = 0
                           0.05, 0.2, 0.01), # X1 = 1
                         nrow = length(X1),
                         ncol = k, byrow = TRUE)

#Cont Data
cont_probs

feat_value <- list(X1 = c(0,1),
                   X2 = c(0,1,2),
                   X3 = c("green", "red"),
                   X4 = c("male", "female"))

test <- apply(feats_df, 2, function(x) sample())

Pop <- data.frame(User = 1:size, k = sample(1:bandits, size = size, replace = T))

test <- function(x){ sample(feat_value[[x]], size = n(), replace = T) }

Pop <- cbind(Pop, feats_df) %>% 
          group_by(k) %>%
          mutate_at(vars(colnames(feats_df)), test(.))
          ungroup

SubPop1 <- Pop %>%
  filter(Arm == 1) %>%
  mutate(Reward = ifelse(Feat1 == 0,
                         sample(c(1,0), size = n(), replace = T, prob = c(probs[1,1], 1 - probs[1,1])),
                         sample(c(1,0), size = n(), replace = T, prob = c(probs[2,1], 1 - probs[2,1]))))

SubPop2 <- Pop %>%
  filter(Arm == 2) %>%
  mutate(Reward = ifelse(Feat1 == 0,
                         sample(c(1,0), size = n(), replace = T, prob = c(probs[1,2], 1 - probs[1,2])),
                         sample(c(1,0), size = n(), replace = T, prob = c(probs[2,2], 1 - probs[2,2]))))

ResultsPop <- bind_rows(SubPop1, SubPop2)

# rn logistic regression on SubPops
Ad1_model <- glm(Reward ~ Feat1, family = binomial, data = SubPop1)
Ad2_model <- glm(Reward ~ Feat1, family = binomial, data = SubPop2)