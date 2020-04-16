checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
    # all parameters in the order in which the R MCDA function takes them

    performanceTable <- NULL
    weights <- NULL
    alternativesIDs <- NULL
    criteriaIDs <- NULL

    #############################################
    # get performance table
    #############################################
    
    performanceTableList <- getNumericPerformanceTableList(xmcdaData)
    
    # we assume that the first performance table is the actual performance table
    
    if(length(performanceTableList) == 0)
      stop("Error: no performance table supplied ")
    else if (length(performanceTableList) > 1)
      stop("Error: more than 1 performance table supplied ")
    
    performanceTable <- performanceTableList[[1]]
    
    #############################################
    # get criteria
    #############################################
    
    activeCriteria <- getActiveCriteria(xmcdaData)
    
    # intersection between active criteria and those present in the weights and the performance table for the filtering
    
    filteredPerformanceTableCriteriaIDs <- intersect(activeCriteria$criteriaIDs, colnames(performanceTable))
    
    # check that we still have active criteria
    if (length(filteredPerformanceTableCriteriaIDs)==0)
      stop("Error: All criteria of the performance table are inactive ")
    
    #############################################
    # get alternatives
    #############################################
    
    activeAlternatives <- getActiveAlternatives(xmcdaData)
    
    # intersection between active alternatives and those present in the performance table
    
    filteredAlternativesIDs <- intersect(activeAlternatives$alternativesIDs,rownames(performanceTable))
    
    #check if there are any differences in alternatives id names
    if (length(filteredAlternativesIDs) != length(as.list(activeAlternatives$alternatives)))
      stop("Error: there are some different id's in alternatives table and the alternatives in Performance table ")
    
    # check that we still have active alternatives
    if (length(filteredAlternativesIDs)==0)
      stop("Error: All alternatives of the performance table are inactive ")
    
    # build filtered performance table and weights vector (for all cases)
    
    filteredPerformanceTable <- performanceTable[filteredAlternativesIDs, filteredPerformanceTableCriteriaIDs]
    
    
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
    
    #filter weights
    filteredweights <- intersect(activeCriteria$criteriaIDs, names(criteriaWeights))
    filteredweights <- criteriaWeights[filteredweights]
    
    # check if there are any NA values in the weights vector
    if (anyNA(filteredweights))
      stop("Error: there is a missing value (NA) in owa weights vector ")
    
    # check if the sumatory of all weights is =1
    if(sum(filteredweights) != 1)
      stop("Error: the summatory of all active weights must be exacly 1.0 ")
    
    # check if there are the same amount of weights and active criteria
    if (length(filteredweights) != (activeCriteria$numberOfCriteria)-length(filteredweights))
      stop("Error: different number of active weights and active criteria ")
    
    # return results
    
    return(list(performanceTable = filteredPerformanceTable,
                weights = filteredweights))
}
