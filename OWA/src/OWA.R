owa_calculation <- function(inputs)
{
  #put the inputs in local variables to work better
  performanceTable = inputs$performanceTable
  weights = inputs$weights
  solverStatus = "Solution found"
  #return variable
  result <-c()
  
  for (i in 1:dim(performanceTable)[1]){
    actual <- performanceTable[i,]
    
    #sort the critera of the same alternative to do the operation
    #na.last = FALSE put the NA in the begging of the sorted vector
    actual <-sort (actual, partial = NULL, na.last = FALSE, decreasing = TRUE, method = c("shell"), index.return = FALSE)
    #if an NA appears put NA as a output result
    if (is.na(actual[1]))
      result <- c(result, actual[1])
    else{
      tempo<-actual * weights
      result <- c(result, sum(tempo))
    }
  }
  #put the name for each alternative
  names(result) <- rownames(performanceTable)
  return(list(lambda = result, solverStatus = solverStatus))
}

owaMethod <- function(inputs)
{
  #execute the operation owa
  result <- owa_calculation(inputs)
  
  #if there aren't an error, return the values
  if(result$solverStatus == "Solution found")
  {
    return(alternativesValues = result$lambda)
  }
  #if error, stop the execution
  else
    stop(result$humanReadableStatus)
}
