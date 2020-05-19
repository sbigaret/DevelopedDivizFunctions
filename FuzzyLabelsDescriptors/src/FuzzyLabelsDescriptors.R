xSpecificity <- function(fuzzyN, a, b){
  #auxiliar function
  # function to search the specificity of a fuzzy number
  # h=1 always in the area calc
  area <- ((fuzzyN[[4]]-fuzzyN[[1]])+(fuzzyN[[3]]-fuzzyN[[2]]))/2
  x <- (area/(b-a))
  x <- 1-x
  return(x)
}

xFuzziness <- function(fuzzyN, a, b){
  #auxiliar function
  # function to search the fuzziness of a fuzzy number
  # h=1 always in the area calc
  area1 <- (fuzzyN[[2]]-fuzzyN[[1]])/2
  area2 <- (fuzzyN[[4]]-fuzzyN[[3]])/2
  x <- area2 + area1
  x <- (1/(b-a))*x
  return(x)
}

descriptorsCalculation <- function(inputs)
{
  #put the inputs in local variables to work better
  fuzzyNumbers = inputs$fuzzyNumbers
  fuzzyNames = inputs$fuzzyNames
  a <- fuzzyNumbers[[1]][[1]]
  b <- fuzzyNumbers[[length(fuzzyNumbers)]][[4]]
  
  # Check variable
  solverStatus = "Solution found"
  #part of the algorithm destinate to prepare the diferent vectors for every necesary calculation
  # prepare the vector with the gravity center for each fuzzy number, named with the fuzzy number id
  xFuVector <- c()
  for (i in 1:length(fuzzyNumbers)) {
    if(anyNA(fuzzyNumbers[[i]]))
      x <- fuzzyNumbers[[i]]
    else
      x <- xFuzziness(fuzzyNumbers[[i]], a, b)
    names(x) <- names(fuzzyNumbers)[[i]]
    xFuVector <- c(xFuVector, x)
  }
  
  #prepare a vector for the COM calculation and get the result
  xSpVector <- c()
  for (i in 1:length(fuzzyNumbers)) {
    if(anyNA(fuzzyNumbers[[i]]))
      x <- fuzzyNumbers[[i]]
    else
      x <- xSpecificity(fuzzyNumbers[[i]], a, b)
    names(x) <- names(fuzzyNumbers)[[i]]
    xSpVector <- c(xSpVector, x)
  }
  
  return(list(lambdaSp = xSpVector, lambdaFu = xFuVector, solverStatus = solverStatus))
}

descriptorsMethod <- function(inputs)
{
  #execute the operation owa
  result <- descriptorsCalculation(inputs)
  
  #if there aren't an error, return the values
  if(result$solverStatus == "Solution found")
  {
    return (list(Fuzziness = result$lambdaFu, Specificity = result$lambdaSp))
  }
  #if error, stop the execution
  else
    stop(result$humanReadableStatus)
}
