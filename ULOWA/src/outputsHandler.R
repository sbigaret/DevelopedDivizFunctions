XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  alternativesValues <- "alternativesValues",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

# function to put alternativesValues in the java xmcda tree

putAlternativesStringValues <- function(xmcda,alternativesValues){
  javaAlternativesValues<-.jnew("org/xmcda/AlternativesValues")
  for (i in 1:length(alternativesValues)){
    javaAlternativesValues$put(.jnew("org/xmcda/Alternative",names(alternativesValues)[i]),.jnew("java/lang/String", alternativesValues[i]))
  }
  xmcda$alternativesValuesList$add(javaAlternativesValues)
  return(xmcda)
}


convert <- function(alternativesValues, programExecutionResult) {
  # converts the outputs of the computation to XMCDA objects
  
  # translate the results into XMCDA v3
  xmcdaAlternativesValues<-.jnew("org/xmcda/XMCDA")
  tmp<-handleException(
    function() return(
      putAlternativesStringValues(xmcdaAlternativesValues,alternativesValues)
    ),
    programExecutionResult,
    humanMessage = "Could not put overall values in tree, reason: "
  )
  # if an error occurs, return null, else a dictionnary "xmcdaTag -> xmcdaObject"
  
  if (is.null(tmp)){
    return(null)
  } else{
    return (list(alternativesValues = xmcdaAlternativesValues))
  }
  
}
