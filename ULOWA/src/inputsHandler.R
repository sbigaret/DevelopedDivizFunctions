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
    stop("Error: no performance table has been supplied. ")
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
      stop("Error: no performance table supplied. ")
    else if (length(performanceTableList) > 1)
      stop("Error: more than 1 performance table supplied. ")
    
    performanceTable <- performanceTableList[[1]]
    
    #############################################
    # get criteria
    #############################################
    
    activeCriteria <- getActiveCriteria(xmcdaData)
    
    # intersection between active criteria and those present in the weights and the performance table for the filtering
    filteredPerformanceTableCriteriaIDs <- intersect(activeCriteria$criteriaIDs, colnames(performanceTable))
    
    # check that we still have active criteria
    if (length(filteredPerformanceTableCriteriaIDs)==0)
      stop("Error: All criteria of the performance table are inactive. ")
    
    #############################################
    # get alternatives
    #############################################
    
    activeAlternatives <- getActiveAlternatives(xmcdaData)
    
    # intersection between active alternatives and those present in the performance table
    
    filteredAlternativesIDs <- intersect(activeAlternatives$alternativesIDs,rownames(performanceTable))
    
    #check if there are any differences in alternatives id names
    if (length(filteredAlternativesIDs) != length(as.list(activeAlternatives$alternatives)))
      stop("Error: there are some different id's in alternatives table and the alternatives in Performance table. ")
    
    # check that we still have active alternatives
    if (length(filteredAlternativesIDs)==0)
      stop("Error: All alternatives of the performance table are inactive. ")
    
    # build filtered performance table and weights vector (for all cases)
    
    filteredPerformanceTable <- performanceTable[filteredAlternativesIDs, filteredPerformanceTableCriteriaIDs]
    
    #############################################
    # get weights
    #############################################
    
    hasWeights <- (xmcdaData$criteriaSetsValuesList$size() >= 1)
    parameters <- getProgramParametersList(xmcdaData)
    hasParameters <- (length(parameters) == 1)
    
    if(hasParameters){
      parameters <- parameters[[1]]
      #control if the document structure is correct
      aux <- names(parameters)
      if ((length(aux) != 2) || !("active" %in% aux) || !("orness" %in% aux))
        stop("Error: Estructural error in program parameters file. ")
      
      #we only wait almost one program parameters list
      for(j in 1:length(parameters))
      {
        param_name <- names(parameters)[j]
        if(param_name == "active"){
          activeParameters <- parameters$active[[1]]
          #control if the active parameter is boolean
          if (!is.logical(activeParameters))
            stop("Error: Activation parameter in orness program parameters structure must be boolean. ")
        }
        else if(param_name == "orness"){
          orness <- parameters$orness[[1]]
          #control if orness parameter is double and inside the [0:1] range
          if (!is.double(orness) || (orness < 0) || (orness > 1))
            stop("Error: Orness value must be a real number inside the [0:1] range. ")
        }
      }
    }
    
    if (!hasWeights && !hasParameters)
      stop("Error: No weigths supplied. ")
    
    if (!hasWeights & hasParameters && !activeParameters)
      stop("Error: inactive orness supplied without alternatives. ")
    
    if (hasWeights && hasParameters && activeParameters)
      stop("Error: too many weights options supplied (weigths table and orness, only one is needed). ")
    
    
    #case: weights table is provided and/or program parameters is inactive 
    if(hasWeights){
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
      
      # check if there are the same amount of weights and active criteria
      if (length(weights) != (activeCriteria$numberOfCriteria-1))
        stop("Error: different number of active weights and active criteria. ")
      
    }else{
      #case program Parameter is provided and is active
      nCriteria <- activeCriteria$numberOfCriteria
      weights <- ornessCalculation(orness, nCriteria)
      
      # check if there are the same amount of weights and active criteria
      if (length(weights) != (activeCriteria$numberOfCriteria))
        stop("Error: different number of active weights and active criteria. ")
    }
    
    # check if there are any NA values in the weights vector
    if (anyNA(weights))
      stop("Error: there is a missing value (NA) in owa weights vector. ")
    
    # check if the summatory of all weights is =1
    if(sum(weights) != 1)
      stop("Error: the summatory of all active weights must be exacly 1.0. ")
    
    
    #############################################
    # get fuzzy numbers
    #############################################
    
    fuzzy <- xmcdaData$criteriaScalesList$get(as.integer(0))
    criterionScales <- fuzzy$get(as.integer(0))
    criterionID <- criterionScales$getCriterion()$id()
    
    # check if more than one criteria scale
    if (criterionScales$size()> 1)
      stop("Error: More than one criteriaScale supplied. ")
    
    scale <- criterionScales$get(as.integer(0))
    prefDirection <- scale$getPreferenceDirection()
    
    # check if there are erroneous format
    if (!(scale %instanceof% J("org/xmcda/QualitativeScale")))
      stop("Error: detected error at scale format composition. ")
    
    fuzzyN <- vector()
    labels <- vector()
    # decompose and check label by label
    for (i in 1:scale$size()){
      valuedLabel <- scale$get((as.integer(i-1)))
      label <- valuedLabel$getLabel()
      labels <- c(labels, label)
      plf <- valuedLabel$getValue()$getValue()
      
      # check if a label is not  fuzzy Number
      if(!(plf %instanceof% J("org.xmcda.value.FuzzyNumber")))
        stop("Error: There is an error in a almost one valuedLabel. ")
      plf <- plf$getFunction()
      
      # check if there are any non numeric value in the label
      if(!plf$abscissaIsNumeric() || !plf$ordinateIsNumeric())
        stop(sprintf("Error: the numericity in (%s) fuzzy label is not guaranteed, check the segments of the labels. ",label))
      
      # check if there are problems in the continuity of the labels
      if(!(.jcall("org/xmcda/utils/Functions", returnSig="Z", "isContinuous", plf)))
        stop(sprintf("Error: the continuity in (%s) fuzzy label is not guaranteed, check the segments of the labels. ",label))
      
      # get the points that describe the labels 
      points <- .jcall("org/xmcda/utils/Functions", returnSig="Ljava/util/List;", "getEndPoints", plf)
      act <- vector()
      
      # get the axis values of each point that describe the label 
      for (idx in 1:points$size()){
        x <- points$get(as.integer(idx-1))$getAbscissa()$getValue()
        y <- points$get(as.integer(idx-1))$getOrdinate()$getValue()
        
        # check if there are any NaN value in it
        if (is.na(x))
          stop(sprintf("Error: there are Na values in (%s) fuzzy label. ",label))
        
        # check if the user give a different value of ordinate values that ones predefined by the function [0,1]
        if (y!=1 & y!=0)
          putProgramExecutionResult(xmcdaMessages, "Warning: at fuzzy numbers, there are ordenate axis values different of 0 or 1, this operators will not take care of it and will supose 0 or 1 values in ordinate axis. ")  
        
        act <- c(act, x)
      }
      
      # control that the segments are well specified
      if (length(act) > 4 | length(act) < 3)
        stop(sprintf("Error: in (%s) fuzzy label, number of segments erroneous, must be 3 for trapezoidal or 2 for triangular labels. ",label))
      if (length(act) == 3)
        act <- c(act[1:2], act[2], act[3])
      fuzzyN <- c(fuzzyN, list(act))
    }
    
    # control the coherence between labels and check if they are ordered
    for (z in 2:length(fuzzyN)){
      if (!all(fuzzyN[[z]] == cummax(fuzzyN[[z]])))
        stop("Error: There are, almost one, fuzzy label not ordered. ")
      
      if (fuzzyN[[z]][1] != fuzzyN[[z-1]][3] || fuzzyN[[z]][2] != fuzzyN[[z-1]][4])
        stop(sprintf("Error: the fuzzy sets do not define a fuzzy partition, which is a requirement of the operator, problem between (%s) and (%s) fuzzy labels. ",labels[[z-1]], labels[[z]]))
    }
    names(fuzzyN) <- labels
    
    # return results
    
    return(list(performanceTable = filteredPerformanceTable, weights = weights, fuzzyNumbers = fuzzyN))
}
