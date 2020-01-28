### Simulation Framework ###

# set up 'real' parameters

                    #B0     #B1
bandits <- list(c(-1.385, -1.571), # arm 1
                c(-2.926, 1.550))  # arm 2



## DRAW FUNCTION (inputs the parameters of both bandits, and external variable/feature x)

draw <- function(weights, k, x){

  # to keep track of reward probs (prob of reward for each bandit at a given trial)
  prob_dict = list()
  
  for(i in 1:length(weights)){

    # create the linear function for each bandit (B0 + B1*x)
    f_x <- function(x) {weights[[i]][1] + weights[[i]][2]*x}
    
    # generate reward probability given by the log link
    probability <- 1/(1 + exp(-f_x(x)))
  
    # append to dict (adds prob of click for each arm)
    prob_dict[[i]] <- probability
    
  }
  
  reward <- sample(c(0,1), size = 1, prob = c(1-prob_dict[[k]], prob_dict[[k]]))
  regret <-  max(unlist(prob_dict)) - prob_dict[[k]]
  theta <- prob_dict[[k]]
  
  info <- list(reward, regret, theta)
  
  return(info)
  
}


## DEMONSTRATION: sample

X <- c(0,1) # two options for X

bandit_probs <- list(x = NULL,
                     band1 = NULL,
                     band2 = NULL)

for(i in 1:length(X)){

  bandit_probs$x[i] <- X[i]
  bandit_probs$band1[i] <- draw(bandits, 1, X[i])[[3]]
  bandit_probs$band2[i] <- draw(bandits, 2, X[i])[[3]]
 
}


plot(x = bandit_probs$x, y = bandit_probs$band1, col = 2, type = "l", ylim = c(0,.75))
lines(x = bandit_probs$x, y = bandit_probs$band2, col = 3,  type = "l")



### simulator needs to take in data, and convert it to 