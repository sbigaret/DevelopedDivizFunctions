#Calculation function for orness value provided option
ornessCalculation <- function(orness, nCriteria){
  #create names for each weight
  prefix <- "pos"
  result <-c()
  for (i in 1:nCriteria) {
    x <- as.character(i)
    if(i<10)
      x <- paste("0", x, sep="")
    x <- paste(prefix, x, sep="")
    result <- c(result, x)
  }
  
  #weights value calculation 
  vec<-c()
  alpha <- 1/orness-1
  for (i in 1:nCriteria) {
    r1 <- i/nCriteria
    r2 <- (i-1)/nCriteria
    res <- (r1^alpha)-(r2^alpha)
    vec <-c(vec,res)
  }
  if (alpha == 0)
    vec[1] <- 1
  
  #merge names to values
  names(vec)<-result
  
  return(vec)
}

checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
    # all parameters in the order in which the R MCDA function takes them

    performanceTable <- NULL
    weights <- NULL
    alternativesIDs <- NULL
    criteriaIDs <- NULL
    orness <- NULL
    weights <- NULL

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
    parameters <- getProgramParametersList(xmcdaData)
    hasParameters <- (length(parameters) == 1)

    if(hasParameters){
      parameters <- parameters[[1]]
      #control if the document structure is correct
      aux <- names(parameters)
      if (!("orness" %in% aux))
        stop("Error: Estructural error in program parameters file ")
      
      #we only wait almost one program parameters list
      for(j in 1:length(parameters))
      {
        param_name <- names(parameters)[j]
        if(param_name == "orness"){
          orness <- parameters$orness[[1]]
          #control if orness parameter is double and inside the [0:1] range
          if (!is.double(orness) || (orness < 0) || (orness > 1))
            stop("Error: Orness value must be a real number inside the ]0:1[ range")
        }
      }
    }
    
    if (!hasWeights && !hasParameters)
      stop("Error: No weights supplied ")
    
    if (hasWeights && hasParameters)
      stop("Error: too many weights options supplied (weights table and orness, only one is needed) ")
    
    
    #case: weights table is provided
    if(hasWeights){
      if (xmcdaData$criteriaValuesList$size() > 1)
        stop("Error: More than one weights table supplied ")
      
      weights = xmcdaData$criteriaValuesList$get(as.integer(0));
      if (!(weights$isNumeric())){
        stop("Error: The weights must be numeric values only ")
      }
      
      # get the criteria weights
      criteriaWeights <-getNumericCriteriaValuesList(xmcdaData)[[1]]
      
      #filter weights
      filteredweights <- intersect(activeCriteria$criteriaIDs, names(criteriaWeights))
      filteredweights <- criteriaWeights[filteredweights]
      
      # check if there are the same amount of weights and active criteria
      if (length(filteredweights) != (activeCriteria$numberOfCriteria)-length(filteredweights))
        stop("Error: different number of active weights and active criteria ")
      
    }else{
      #case program Parameter is provided
      nCriteria <- activeCriteria$numberOfCriteria
      filteredweights <- ornessCalculation(orness, nCriteria)
      
      # check if there are the same amount of weights and active criteria
      if (length(filteredweights) != (activeCriteria$numberOfCriteria))
        stop("Error: different number of active weights and active criteria ")
    }
    
    # check if there are any NA values in the weights vector
    if (anyNA(filteredweights))
      stop("Error: there is a missing value (NA) in owa weights vector ")
    
    # check if the sumatory of all weights is =1
    if(sum(filteredweights) != 1)
      stop("Error: the summatory of all active weights must be exacly 1.0 ")
    
    
    # return results
    return(list(performanceTable = filteredPerformanceTable,
                weights = filteredweights))
}
