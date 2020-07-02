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

getPerformanceTableList <- function(xmcda){
  out <- list()
  if (xmcda$performanceTablesList$size()==0){
    return(out)
    stop("Error: no performance table has been supplied")
  }
  
  for (k in 1:xmcda$performanceTablesList$size()){
    performanceTable <-xmcda$performanceTablesList$get(as.integer(k-1))
    if(!is.null(performanceTable)){
      alternativesList <- as.list(performanceTable$getAlternatives())
      criteriaList <- as.list(performanceTable$getCriteria())
      doublePerformanceTable<-performanceTable
      
      pT <- matrix(NA, length(alternativesList), length(criteriaList))
      # fill this "empty" matrix with the values present in the performanceTable object
      for (i in 1:length(alternativesList)){
        for (j in 1:length(criteriaList)){
          val <- doublePerformanceTable$getValue(alternativesList[[i]],criteriaList[[j]])
          if(!is.null(val))
            pT[i,j] <- val
        }
      }
      # name the rows and the columns of pT
      rownames(pT) <- sapply(alternativesList, function(alternative) alternative$id())
      colnames(pT) <- sapply(criteriaList, function(criterion) criterion$id())
      
      # add pT to the list of performance tables
      out <- c(out, setNames(list(pT), performanceTable$id()))
    }  
  }
  return(out)
}

getCategoryValuesList <- function(xmcda){
  out <- list()
  categories <- as.list(xmcda$categories$getActiveCategories())
  # if categories values have been provided
  if(xmcda$categoriesValuesList$size() == 1)
  {
    categoriesValues <- xmcda$categoriesValuesList$get(as.integer(0))
    keys <- as.list(categoriesValues$keySet())
    if(length(keys) != length(categories))
      stop("Error: Different id in categories and the id in their values tables ")
    values <- as.list(categoriesValues$values())
    ranks <- c()
    name <- c()
    for (i in 1:length(values)){
      category <- as.list(values[[i]])
      aux <- c()
      for (j in 1:length(category)){
        aux[j] <- category[[j]]$getValue()
      }
      aux <- list(aux)
      names(aux) <- keys[[i]]$id()
      # Find if there are an active category with the same id that our categories values
      conditionalName <- NULL
      for (j in 1:length(categories)){
        if (categories[[j]]$equals(keys[[i]])){
          conditionalName <- categories[[j]]$name()
          names(conditionalName) <- categories[[j]]$id()
        }
      }
      if (!is.null(conditionalName)){
        # Save the data if all is correct
        ranks <- c(ranks,(aux))
        name <- c(name, conditionalName)
      }
    }
    out <- c(out, list(ranks = ranks, names = name))
  }else
    stop("Error: Incorrect number of fuzzy number tables supplied ")
  return(out)
}

# If there are any error in fuzzy Numbers return the error code to stop the execution in the main function 
fuzzyCorrectness <- function(ranks, names){
  # If there are any NA or non numeric value in the fuzzyNumber return 4
  if(!is.numeric(ranks[[1]]) || anyNA(ranks[[1]]))
    return(4)
  #return 5
  if (length(ranks) != length(names))
    return (3)
  #If id in categories and their values are different return 3
  for (i in 2:length(ranks)){
    # If there are any NA or non numeric value in the fuzzyNumber return 4
    if(!is.numeric(ranks[[i]]) || anyNA(ranks[[i]]))
      return(4)
    # If the fuzzyNumber values aren't ordered return 1
    if (!all(ranks[[i]] == cummax(ranks[[i]])))
      return(1)
    
    if (ranks[[i]][1] != ranks[[i-1]][3] || ranks[[i]][2] != ranks[[i-1]][4])
      return(2)
  }
  return(0)
}



checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
    # all parameters in the order in which the R MCDA function takes them
    performanceTable <- NULL
    weights <- NULL
    alternativesIDs <- NULL
    criteriaIDs <- NULL

    #############################################
    # get performance table
    #############################################
    
    performanceTableList <- getPerformanceTableList(xmcdaData)
    
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
        stop("Error: missing parameter 'orness' program parameters file")
      
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
    
    
    #case: weights table is provided and/or program parameters is inacive 
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
      #case program Parameter is provided and is active
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
    
    
    #############################################
    # get fuzzy numbers
    #############################################
    
    fuzzy <- getCategoryValuesList(xmcdaData)
    
    rturn <- fuzzyCorrectness(fuzzy$ranks, unname(fuzzy$names))
    if (rturn == 1)
      stop("Error: There are, almost one, fuzzyNumber not ordered ")
    if (rturn == 2)
      stop("Error: Incorrect fuzzy Number, rank must be well covered ")
    if (rturn == 4)
      stop("Error: There are non numeric values or NA in almost one fuzzyNumber ")
    if (rturn == 3)
      stop("Error: different number of fuzzyNumbers names labels and fuzzy names sets of value ")
    
    # return results
    
    return(list(performanceTable = filteredPerformanceTable,
                weights = filteredweights, fuzzyNumbers = fuzzy$ranks, fuzzyNames = fuzzy$names))
}
