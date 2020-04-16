# non used control function in this version
nCorrect <- function(criteriaWeights){
  criteriaObj <- as.list(xmcdaData$criteria$getActiveCriteria())
  criteria <- c()
  for (i in 1:length(criteriaObj)) {
    criteria <- c(criteria,criteriaObj[[i]]$id())
  }
  print(criteriaObj)
  unique(criteria)
  
  print(length(criteriaObj))
  criteriaObj <- as.list(xmcdaData$criteria$getActiveCriteria())
  if(length(criteriaWeights) != length(criteriaObj))
    return(FALSE)
  return (TRUE)
}

checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
    # all parameters in the order in which the R MCDA function takes them
    weights <- NULL

    #############################################
    # get weights
    #############################################
    
    hasWeights <- (xmcdaData$criteriaValuesList$size() >= 1)
    
    if (!hasWeights)
      stop("Error: No weigths table supplied ")
    
    if (xmcdaData$criteriaValuesList$size() > 1)
      stop("Error: More than one weigths table supplied ")
    
    if (hasWeights){
      weights = xmcdaData$criteriaValuesList$get(as.integer(0));
      if (!(weights$isNumeric())){
        stop("Error: The weights must be numeric values only ")
      }
    }
    
    # get the criteria weights
    criteriaWeights <-getNumericCriteriaValuesList(xmcdaData)[[1]]
    
    # check if there are any NA values in the weights vector
    if (anyNA(criteriaWeights))
      stop("Error: there is a missing value (NA) in owa weights vector ")
    
    # check if the sumatory of all weights is =1
    if(sum(criteriaWeights) != 1)
      stop("Error: the summatory of all active weights must be exacly 1.0 ")
    
    criteriaObj <- as.list(xmcdaData$criteria$getActiveCriteria())
    if(length(criteriaWeights) != length(criteriaObj))
      #if (!nCorrect(criteriaWeights))
      stop("Error: There are some errors in Weights ID's format ")
    
    # return results
    return(list(weights = criteriaWeights))
}
