XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  alternativesValues <- "alternativesValues",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

#Setter adapted to Integers
putIAlternativesValues <- function(xmcda,alternativesValues){
  javaAlternativesValues<-.jnew("org/xmcda/AlternativesValues")
  for (i in 1:length(alternativesValues)){
    javaAlternativesValues$put(.jnew("org/xmcda/Alternative",names(alternativesValues)[i]),.jnew("java/lang/Integer",alternativesValues[i]))
  }
  xmcda$alternativesValuesList$add(javaAlternativesValues)
  return(xmcda)
}


# function to put alternativesValues in the java xmcda tree
convert <- function(alternativesValues, programExecutionResult) {
  # converts the outputs of the computation to XMCDA objects
  
  # translate the results into XMCDA v3
  xmcdaAlternativesValuesCog<-.jnew("org/xmcda/XMCDA")
  xmcdaAlternativesValuesCom<-.jnew("org/xmcda/XMCDA")
  xmcdaAlternativesValuesOrd<-.jnew("org/xmcda/XMCDA")
  
  tmp1<- handleException(
    function() return(
      putAlternativesValues(xmcdaAlternativesValuesCog,alternativesValues$defuzzificationCOG)
    ),
    programExecutionResult,
    humanMessage = "Could not put overall values in tree, reason: "
  )
  
  tmp2<- handleException(
    function() return(
      putAlternativesValues(xmcdaAlternativesValuesCom,alternativesValues$defuzzificationCOM)
    ),
    programExecutionResult,
    humanMessage = "Could not put overall values in tree, reason: "
  )
  
  tmp3<- handleException(
    function() return(
      putIAlternativesValues(xmcdaAlternativesValuesOrd,alternativesValues$defuzzificationORD)
    ),
    programExecutionResult,
    humanMessage = "Could not put overall values in tree, reason: "
  )
  
  # if an error occurs, return null, else a dictionnary "xmcdaTag -> xmcdaObject"
  if (is.null(tmp1) || is.null(tmp2) || is.null(tmp3)){
    return(null)
  } else{
    return (list(defuzzificationCOG = xmcdaAlternativesValuesCog, defuzzificationCOM = xmcdaAlternativesValuesCom, defuzzificationORD = xmcdaAlternativesValuesOrd ))
  }
  
}
