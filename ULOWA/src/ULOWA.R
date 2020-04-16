xCog <- function(fuzzyN){
  #auxiliar function
  # function to search the center of a fuzzy number
  if (fuzzyN[[1]] == fuzzyN[[4]])
    y <- 0.5
  else
    y <-(1/6)*((fuzzyN[[3]]-fuzzyN[[2]])/(fuzzyN[[4]]-fuzzyN[[1]])+2)
  x <- (y*(fuzzyN[[3]]+fuzzyN[[2]])+(fuzzyN[[4]]+fuzzyN[[1]])*(1-y))/2
  return(x)
}

similarity <- function(fuzSet1,fuzSet2){
  #auxiliar function
  #function to search the similarity between two fuzzy Numbers
  res <- c()
  for (i in 1:4) {
    res <- c(res , (2-abs(fuzSet2[[i]]-fuzSet1[[i]])-1))
  }
  res <- prod(res)
  res <- res**(1/4)
  return(res)
}

recursiveCore <- function(acVec, weights, i){
  #Recursive part inside the algorithm for ULOWA result calculation
  #used to assossiate the current weight with the center of two fuzzy numbers by pairs
  if (i==length(acVec))
    return(acVec[[i]])
  previous <- recursiveCore(acVec,weights, i+1)
  res <- weights[[i]]*acVec[[i]]+(1-weights[[i]])*previous
  return(res)
}

ulowa_calculation <- function(actual,weights,fuzzyNumbers){
  #part of the algorithm destinate to prepare the diferent vectors for every necesary calculation
  # prepare the vector with the gravity center for each fuzzy number, named with the fuzzy number id
  xCogVector <- c()
  for (i in 1:length(fuzzyNumbers)) {
    x <- xCog(fuzzyNumbers[[i]])
    names(x) <- names(fuzzyNumbers)[[i]]
    xCogVector <- c(xCogVector, x)
  }
  #prepare the vector that change the label for each criteria of the alternative for each gravity center number
  xVecCalc <- c()
  for (i in 1:length(actual)) {
    x <- xCogVector[[actual[[i]]]]
    names(x) <- actual[[i]]
    xVecCalc <- c(xVecCalc, x)
  }
  #do the calculation mixim, weights, and gravity centers of each labels
  res <- recursiveCore(xVecCalc,weights,1)
  #search the fuzzy number with more similarity with our result
  res <- c(res,res,res,res)
  xSimName <-""
  aux <- 0
  for (i in 1:length(xCogVector)) {
    x <- similarity(fuzzyNumbers[[i]],res)
    if (aux<=x){
      aux <- x
      xSimName <- names(fuzzyNumbers)[[i]]
    }
  }
  #return the id of similarest fuzzy number for our result
  return(xSimName)  
}


ulowaGetDataReady <- function(inputs)
{
  #put the inputs in local variables to work better
  performanceTable = inputs$performanceTable
  weights = inputs$weights
  fuzzyNumbers = inputs$fuzzyNumbers
  fuzzyNames = inputs$fuzzyNames
  # make the list of (fuzzyId-number) to sort the alternatives
  ordFuzzyVector <- c()
  fuzzyId <- c()
  for (i in 1:length(fuzzyNumbers)) {
    ordFuzzyVector <- c(ordFuzzyVector, as.integer(i))
    fuzzyId <- c(fuzzyId,names(fuzzyNumbers[i]))
  }
  names(ordFuzzyVector) <- fuzzyId
  # Check variable
  solverStatus = "Solution found"
  #return variable
  result <-c()
  for (i in 1:dim(performanceTable)[1]){
    actual <- performanceTable[i,]
    # assocate the label as a ordFuzzyVector, in this case we can ordered automatically, below we undo this, only to ordered optimously
    ac1 <- c()
    for (i in 1:length(actual)) {
      if (!is.na(actual[[i]]))
        ac1 <- c(ac1, ordFuzzyVector[actual[[i]]])
      else
        ac1 <- c(ac1,actual[[i]])
    }
    #sort the critera of the same alternative to do the operation
    #na.last = FALSE put the NA in the begging of the sorted vector
    actual <-sort (ac1, partial = NULL, na.last = FALSE, decreasing = TRUE, method = c("shell"), index.return = FALSE)
    #if an NA appears put NA as a output result
    if (is.na(actual[1]))
      result <- c(result, "NaN")
    else{
      # get only the labels ordered
      actual <- (names(actual))
      # calculation
      tempo <- ulowa_calculation(actual,weights,fuzzyNumbers)
      # get complete name
      tempo <- fuzzyNames[[tempo]]
      result <- c(result, tempo)
    }
  }
  #put the name for each alternative
  names(result) <- rownames(performanceTable)
  return(list(lambda = result, solverStatus = solverStatus))
}

ulowaMethod <- function(inputs)
{
  #execute the operation owa
  result <- ulowaGetDataReady(inputs)
  
  #if there aren't an error, return the values
  if(result$solverStatus == "Solution found")
  {
    return(alternativesValues = result$lambda)
  }
  #if error, stop the execution
  else
    stop(result$humanReadableStatus)
}
