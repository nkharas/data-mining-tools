###################################################
# Author : Nick Kharas
###################################################

###################################################
# Return min value + threshold 
#   if the min value of the column is < threshold
#   , else return 0
###################################################
min_transform <- function(x, threshold = 0){
  if (min(x) < threshold){
    result <- abs(min(x)) + 1
  }
  else{
    result <- 0
  }
  return(result)
}

###################################################
# Log Transform. Log transformations below 1 return infinity. So, the threshold is 1.
# We make this adjustment to avoid taking log of values < 1 and avoid infinity
# Adding a fixed constant does not change the shape of the distribution of data
# If minimum value is >= 1, min_transform returns 0, thus retaining the original value of x
###################################################
log_transform <- function(x){
  result <- log(x + min_transform(x, 1))
  return(result)
}

###################################################
# Log Transform to the base 2. 
# Log transformations below 1 return infinity. So, the threshold is 1
# If minimum value is >= 1, min_transform returns 0, thus retaining the original value of x
###################################################
log2_transform <- function(x){
  result <- log2(x + min_transform(x, 1))
  return(result)
}

###################################################
# Return actual value from log
# If you adjusted initially to take logs of only positive numbers, 
#  enter that adjustment as the second parameter
###################################################
loginv_transform <- function(x, adjust_min_transform = 0){
  result <- exp(x) - adjust_min_transform
  return(result)
}

# Return actual value from log, base 2
log2inv_transform <- function(x, adjust_min_transform = 0){
  result <- (2^x) - adjust_min_transform
  return(result)
}

###################################################
# Square root transform
# Adding a constant positive number will ensure we dont take a square root < 0,
#    and also won't change the shape of the distribution of the data
###################################################
sqrt_transform <- function(x){
  result <- sqrt(x + min_transform(x, 0))
  return(result)
}

###################################################
# Forward stepping model with diagnostics
# Arrange the data frame such that the last column is the dependent variable
#   and that it has only those columns (or, features) on which you want to rain the model
# Test is the optional holdout dataset. If you don't have one, you won't be able to run holdout diagnostics.
# If you do provide test data, both train and test data should have exactly the same column structure.
###################################################
forward_step <- function(train, test = data.frame(ints=integer())){
  library(MASS)
  fit <- lm(paste(colnames(train[ncol(train)])," ~ ."), data = train)
  model <- stepAIC(fit, direction = "forward")
  
  print("Anova")
  print(model$anova)
  
  print("      ")
  print("Model Summary")
  print(summary(model))
  
  if(nrow(test) > 0){
    pred <- predict(model, newdata = test)
    cr <- cor(test[, ncol(test)], pred)
    
    print("      ")
    print("Holdout R Squared")
    print(cr * cr)
  }
  
  print("      ")
  print("Residual plot and homoskedasticity diagnostics")
  autoplot(model)
  
  print("      ")
  print("Breusch-Pagan test for homoskedasticity")
  bptest(model)
  
  return(model)
}
