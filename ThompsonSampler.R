### fit_predict function
  # just calls fitBLR and predictProbs

data = samp
x = 1.5

function(data, x){
  
  equation <- paste(colnames(data[2]), paste(colnames(data[3:ncol(data)]), collapse = " + "), sep = " ~ ")
  
  fit <- fitBLR(data = data, equation = equation)
  
  prob <- predictProb(posterior = fit, x = x)[1]
  
  # add way to put ALL coeffs and SDs in the df
  data.frame(prob = prob,
             )
  
}