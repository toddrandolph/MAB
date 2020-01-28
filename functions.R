### Bayesian Logistic Regression

# fits the model, outputs m and q
fitBLR <- function(data, equation, location = 0, scale = 2.5, prior = "student_t", iters = 5000, chains = 2){
  
  require(rstanarm)
  
  # establish prior distributions
  if(prior == "normal") {
    prior_dist <- normal(location, scale)
  } else if(prior == "cauchy") {
    prior_dist <- cauchy(location, scale)
  } else {
    prior_dist <- student_t(df = 7, location, scale)
  }
  
  # fit model
  fit <- stan_glm(formula = as.formula(equation),
                  data = data,
                  family = binomial,
                  prior = prior_dist,
                  prior_intercept = prior_dist,
                  algorithm = "sampling",
                  iter = iters,
                  chains = chains)
  
  parameters <- rbind(fit$coefficients, fit$ses)
  
  # add fail safe for non-convergence
  
}

# samples from posterior (after fitBLR) and outputs predicted probability
# x can be MULTIPLE values (for more x variables)
predictProb <- function(posterior, x, alpha = 1, mode = "sample"){
  
  require(pracma)
  
  # fit the model, returning posterior parameterse
  # posterior <- fitBLR(data = data, equation = equation)
  
  if(mode == "sample"){
    coeffs <- apply(posterior, 2, function(x) rnorm(1,posterior[1], alpha*posterior[2]))  
  } else {
    coeffs <- posterior[1,]  
  }
  
  # predict the probability
  model <- coeffs[1] + pracma::dot(coeffs[2:length(coeffs)], x)
  prob <- unname(1/(1 + exp(-1*model)))
  
  return(c(prob, 1-prob))
  
}

# calls fitBLR and predictProb and outputs the results in a temp df
fitPredict <- function(data, x){
  
  equation <- paste(colnames(data[2]), paste(colnames(data[3:ncol(data)]), collapse = " + "), sep = " ~ ")
  
  fit <- fitBLR(data = data, equation = equation)
  prob <- predictProb(posterior = fit, x = x)[1]
  
  temp_df <- data.frame(prob = prob, params = as.list(fit))
  colnames(temp_df) <- c("prob", paste0(c("b", "sd"), rep(1:ncol(fit), each = 2), separate = ""))
  
  return(temp_df)
  
}

# calls fitPredict for each arm, outputing the arm w/ highest predicted prob
chooseBandit <- function(results_df, x, buffer_size = 400){
  
  df <- tail(results_df, n = buffer_size)
  size_check <- df %>% count(Arm, Reward)
  
  if(nrow(df) == k*2){
    # choose through TS
    ts_model_df <- df %>%
      group_by(Arm) %>%
      mutate(results = fitPredict(.,x)) %>%
      ungroup
    
    bandit <- ts_model_df %>% filter(which.max(prob))
    
  } else {
    # randomly choose
    bandit <- sample(1:k,1)
    
  }
  
  return(bandit)
}

# generates the "true" prob for each bandit and stores them in a temp prob_dict
# For the arm selected from chooseBandit, generates a reward based on the "true" probs
# generates the regret for this round
draw <- function(weights, k, x){
  
  # to keep track of reward probs (prob of reward for each bandit at a given trial)
  prob_dict = list()
  
  for(i in 1:length(weights)){
    
    # create the linear function for each bandit
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