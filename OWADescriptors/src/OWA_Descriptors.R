ornes <- function(inputs)
{
  #put the inputs in local variables to work better
  weights = inputs$weights
  n<-length(weights)
  
  #return variable
  result <-0
  
  #Ornes calculation
  for (i in 1:length(weights)){
    actual <- weights[i]
    actual <- actual*((n-i)/(n-1))
    result<-result + actual
    
  }
  
  #put the name for each alternative
  names(result) <- "Ornes"
  return(list(lambda = result))
}

balance <- function(inputs)
{
  #put the inputs in local variables to work better
  weights = inputs$weights
  n<-length(weights)
  
  #return variable
  result <-0
  
  #Balance calculation
  for (i in 1:length(weights)){
    actual <- weights[i]
    actual <- actual*((n+1-(2*i))/(n-1))
    result<-result + actual
    
  }
  
  #put the name for each alternative
  names(result) <- "Balance"
  
  return(list(lambda = result))
}

entropy <- function(inputs)
{
  #put the inputs in local variables to work better
  weights = inputs$weights
  n<-length(weights)
  
  #return variable
  result <-0
  
  #Entropy calculation
  for (i in 1:length(weights)){
    actual <- weights[i]
    if (actual > 0)
      actual <- actual*(log(actual))
    result<-result + actual
    
  }
  result <- -result
  
  #put the name for each alternative
  names(result) <- "Entropy"
  
  return(list(lambda = result))
}

divergence <- function(inputs, ornes)
{
  #put the inputs in local variables to work better
  weights = inputs$weights
  n<-length(weights)
  ornesValue = ornes
  if (is.null(ornesValue))
    stop("Ornes value in Divergence calculation is null ")
  
  #return variable
  result <-0
  
  #Divergence calculation
  for (i in 1:length(weights)){
    actual <- weights[i]
    actual <- actual*((((n-i)/(n-1))-ornes)^2)
    result<-result + actual
    
  }
  
  #put the name for each alternative
  names(result) <- "Divergence"
  
  return(list(lambda = result))
}

owaDescriptorMethod <- function(inputs)
{
  #execute the operations owa_descriptors
  result <- ornes(inputs)$lambda
  result <- c(result, balance(inputs)$lambda)
  result <- c(result, entropy(inputs)$lambda)
  result <- c(result, divergence(inputs, result[1])$lambda)
  
  
  #if there aren't an error, return the values
  for (i in 1:length(result)){
    #if error, stop the execution
    if(is.null(result[i])){
      stop("Error in owa_descriptors operations ")
    }
  }
  return(list(ornes=result[1], balance=result[2], entropy=result[3], divergence=result[4]))
}
