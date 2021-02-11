XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  alternativesValues <- "alternativesValues",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

convert <- function(alternativesValues, programExecutionResult) {
  # converts the outputs of the computation to XMCDA objects
  
  # translate the results into XMCDA v3
  xmcdaAlternativesValues<-.jnew("org/xmcda/XMCDA")
  
  tmp<-handleException(
    function() return(
      putAlternativesValues(xmcdaAlternativesValues,alternativesValues)
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
