### simulator needs to take in data, and convert it to

source("Updates/functions.R")

trials = 500
k = 2

bandit_list <- list()

results_df <- data.frame(matrix(0, nrow = trials, ncol = 6))
colnames(results_df) <- c("k", "x", "reward", "regret", "display_pct", "ctr")

for(i in trials){
  
  round_df <- results_df %>% select(k, x, reward)
  
  # sample from mock population and observe x values
  x <- 1.5
  
  # choose creative
  k <- chooseBandit()
  
  # display chosen creative and record results
  output <- draw(weights, k, x)
  
}
