checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
    # all parameters in the order in which the R MCDA function takes them
    weights <- NULL

    #############################################
    # get weights
    #############################################
    
    hasWeights <- (xmcdaData$criteriaSetsValuesList$size() >= 1)
    
    if (!hasWeights)
      stop("Error: No weigths table supplied. ")
    
    if (xmcdaData$criteriaSetsValuesList$size() > 1)
      stop("Error: More than one weigths table supplied. ")
    
    criteriaSetValues = xmcdaData$criteriaSetsValuesList$get(as.integer(0))
    qvalues <- as.list(criteriaSetValues$values())[[1]]
    
    if (!(qvalues$isNumeric())){
      stop("Error: The weights must be numeric values only. ")
    }
    
    #convert values to double and get it
    qvalues$convertToDouble()
    values <- qvalues$rawValues()
    weights <- vector()
    for(i in 1:values$size()) {
      weights <- c(weights, values$get(as.integer(i-1)))
    }
    
    # check if there are any NA values in the weights vector
    if (anyNA(weights))
      stop("Error: there is a missing value (NA) in owa weights vector. ")
    
    # check if the summatory of all weights is =1
    if(sum(weights) != 1)
      stop("Error: the summatory of all active weights must be exacly 1.0. ")
    
    # return results
    return(list(weights = weights))
}
