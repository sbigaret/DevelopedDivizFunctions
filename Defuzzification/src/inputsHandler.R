# function to get a list of alternatives values from a java xmcda tree
getAlternativesValuesList <- function(xmcda){
  out <- list()
  
  if (xmcda$alternativesValuesList$size()==0){
    return(out)
    stop("Error: no alternativesValues has been supplied")
  }
  
  for (k in 1:xmcda$alternativesValuesList$size()){
    alternativesValues <-xmcda$alternativesValuesList$get(as.integer(k-1))
    
    if(!is.null(alternativesValues)){
      # check if alternativesValues is numeric
      alternativesList <- as.list(alternativesValues$getAlternatives())
      doubleAlternativesValues<-alternativesValues
      aV<-c()
      
      # fill this "empty" vector with the values in the alternativesValues object
      for (j in 1:length(alternativesList)){
        aV<-c(aV,as.list(doubleAlternativesValues$get(alternativesList[[j]]))[[1]]$getValue())
      }
    }
    # name the elements of pT
    names(aV) <- sapply(alternativesList, function(alternative) alternative$id())
    # add aV to the list of alternativesValues
    out <- c(out, setNames(list(aV), alternativesValues$id()))
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
          conditionalName <- categories[[j]]$id()
          names(conditionalName) <- categories[[j]]$name()
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
    alternativesTable <- NULL
    fuzzy <- NULL
    
    #############################################
    # get alternatives table
    #############################################
    
    hasTooAlternatives <- (xmcdaData$alternativesValuesList$size() > 1)
    if (hasTooAlternatives)
      stop("Error: More than one Alternatives Table supplied ")
    hasLessAlternatives <- (xmcdaData$alternativesValuesList$size() <= 0)
    if (hasLessAlternatives)
      stop("Error: No Alternatives Table supplied ")
    alter <- as.list( getAlternativesValuesList(xmcdaData)[[1]])
    
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
    
    x <- names(fuzzy$names)
    for (i in 1:length(alter)) {
      if(!is.element(alter[[i]],x))
        stop("Error: there are almost one alternative didn't aspectected in fuzzyNumbers table ")
    }
    
    # return results
    return(list(fuzzyNumbers = fuzzy$ranks, fuzzyNames = fuzzy$names, alternatives = alter))
}
