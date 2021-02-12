xCog <- function(fuzzyN){
  #auxiliar function
  # function to search the center of a fuzzy number
  if (fuzzyN[[1]] == fuzzyN[[4]])
    y <- 0.5
  else
    y <-(1/6)*(((fuzzyN[[3]]-fuzzyN[[2]])/(fuzzyN[[4]]-fuzzyN[[1]]))+2)
  x <- ((y*(fuzzyN[[3]]+fuzzyN[[2]]))+((fuzzyN[[4]]+fuzzyN[[1]])*(1-y)))/2
  return(x)
}

xCom <- function(fuzzyN){
  #auxiliar function
  # function to search a fuzzy number COM value
  x <- (fuzzyN[[2]] + fuzzyN[[3]])/2
  return(x)
}

xOrd <- function(fuzzyN){
  #auxiliar function
  # function to search the fuzzy number ordinal value
  nam <- names(fuzzyN)
  x <-c()
  for (i in 1:length(fuzzyN)) {
    x <- c(x, i)
  }
  names(x) <- nam
  return(x)
}

defuzzyCalculation <- function(inputs)
{
  #put the inputs in local variables to work better
  fuzzyNumbers = inputs$fuzzyNumbers
  alternatives = inputs$alternatives
  
  # make the list of (fuzzyId-alternative)
  defuzzyId <- c()
  for (i in 1:length(alternatives)) {
    #if the alternative value is a  NA, pass NA
    x <- alternatives[[i]]
    names(x) <- names(alternatives)[[i]]
    defuzzyId <- c(defuzzyId, x)
  }
  
  # Check variable
  solverStatus = "Solution found"
  #part of the algorithm destinate to prepare the different vectors for every necessary calculation
  # prepare the vector with the gravity center for each fuzzy number, named with the fuzzy number id
  xCogVector <- c()
  for (i in 1:length(fuzzyNumbers)) {
    x <- xCog(fuzzyNumbers[[i]])
    names(x) <- names(fuzzyNumbers)[[i]]
    xCogVector <- c(xCogVector, x)
  }
  
  #prepare a vector for the COM calculation and get the result
  xComVector <- c()
  for (i in 1:length(fuzzyNumbers)) {
    x <- xCom(fuzzyNumbers[[i]])
    names(x) <- names(fuzzyNumbers)[[i]]
    xComVector <- c(xComVector, x)
  }
  
  #prepare a vector for the Ordinal calculation and get the result
  xOrdVector <- c()
  x <- xOrd(fuzzyNumbers)
  names(x) <- names(fuzzyNumbers)
  xOrdVector <- c(xOrdVector, x)  
  
  
  #return variables
  resultCog <-c()
  resultCom <-c()
  resultOrd <-c()
  
  #prepare the results vectors with their results and their alternative name
  for (i in 1:length(defuzzyId)) {
    #if the alternative value is a  NA, return NA
    if (is.na(defuzzyId[[i]])){
      x1 <- defuzzyId[[i]]
      x2 <- defuzzyId[[i]]
      x3 <- defuzzyId[[i]]
    }
    else{
      x1 <- xCogVector[defuzzyId[[i]]]
      x2 <- xComVector[defuzzyId[[i]]]
      x3 <- xOrdVector[defuzzyId[[i]]]
    }
    names(x1) <- names(defuzzyId)[[i]]
    names(x2) <- names(defuzzyId)[[i]]
    names(x3) <- names(defuzzyId)[[i]]
    resultCog <- c(resultCog, x1)
    resultCom <- c(resultCom, x2)
    resultOrd <- c(resultOrd, x3)
  }
  
  return(list(lambdaCog = resultCog, lambdaCom = resultCom, lambdaOrd = resultOrd, solverStatus = solverStatus))
}

defuzzificationMethod <- function(inputs)
{
  #execute the operation owa
  result <- defuzzyCalculation(inputs)
  
  #if there aren't an error, return the values
  if(result$solverStatus == "Solution found")
  {
    return (list(defuzzificationCOG = result$lambdaCog, defuzzificationCOM = result$lambdaCom, defuzzificationORD = result$lambdaOrd))
  }
  #if error, stop the execution
  else
    stop(result$humanReadableStatus)
}
