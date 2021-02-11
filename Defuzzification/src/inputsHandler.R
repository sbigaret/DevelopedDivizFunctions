# function to get a list of alternatives values from a java xmcda tree
getAlternativesValuesList <- function(xmcda){
  out <- list()
  
  if (xmcda$alternativesValuesList$size()==0){
    return(out)
    stop("Error: no alternativesValues has been supplied. ")
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

checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
    # all parameters in the order in which the R MCDA function takes them
    alternativesTable <- NULL
    fuzzy <- NULL
    
    #############################################
    # get alternatives table
    #############################################
    
    hasTooAlternatives <- (xmcdaData$alternativesValuesList$size() > 1)
    if (hasTooAlternatives)
      stop("Error: More than one Alternatives Table supplied. ")
    hasLessAlternatives <- (xmcdaData$alternativesValuesList$size() <= 0)
    if (hasLessAlternatives)
      stop("Error: No Alternatives Table supplied. ")
    alter <- as.list( getAlternativesValuesList(xmcdaData)[[1]])
    
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
    # check if all alternatives are inside the fuzzy membership
    for (fz in 1:length(alter)){
      if (!is.element(alter[[as.integer(fz)]], names(fuzzyN)))
        stop(sprintf("Error: (%s) alternative value don't corresponds with any fuzzy label. ",alter[[fz]] ))
    }
    
    # return results
    return(list(fuzzyNumbers = fuzzyN, alternatives = alter))
}
