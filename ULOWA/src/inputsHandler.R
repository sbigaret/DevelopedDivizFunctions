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