### Bayesian Logistic Regression Function ###

library(rstanarm)

# sample data

samp <- SubPop1 %>% slice(1:500)



t_prior <- student_t(df = 7, location = 0, scale = 2.5)
normal_prior <- normal(0, 2.5)

comparison <- glm(formula = Reward ~ Feat1,
                  data = samp,
                  family = binomial)
  
test <- stan_glm(formula = Reward ~ Feat1,
                 data = samp,
                 family = binomial,
                 prior = t_prior,
                 prior_intercept = t_prior,
                 algorithm = "sampling",
                 iter = 5000,
                 chains = 2)

# point estimates
test$coefficients
#standard errors
test$ses
comparison$coefficients

#mean and stand. dev
df <- as.matrix(test)
colMeans(df) 
apply(df, 2, function(x) sqrt(var(x)))

# median (same as coeffs and )
apply(df, 2, median)



data = samp

equation <- paste(colnames(data[2]), paste(colnames(data[3:ncol(data)]), collapse = " + "), sep = " ~ ")

### function that fits the model, outputs m and q (fail safe this...if chain does not converge do __)
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

### function that samples and outputs predicted probability (include alpha here)
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

