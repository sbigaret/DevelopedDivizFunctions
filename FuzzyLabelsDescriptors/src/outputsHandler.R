XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  alternativesValues <- "alternativesValues",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

# function to put alternativesValues in the java xmcda tree
convert <- function(alternativesValues, programExecutionResult) {
  # converts the outputs of the computation to XMCDA objects
  
  # translate the results into XMCDA v3
  xmcdaAlternativesValuesFu<-.jnew("org/xmcda/XMCDA")
  xmcdaAlternativesValuesSp<-.jnew("org/xmcda/XMCDA")
  
  tmp1<- handleException(
    function() return(
      putAlternativesValues(xmcdaAlternativesValuesFu,alternativesValues$Fuzziness)
    ),
    programExecutionResult,
    humanMessage = "Could not put overall values in tree, reason: "
  )
  
  tmp2<- handleException(
    function() return(
      putAlternativesValues(xmcdaAlternativesValuesSp,alternativesValues$Specificity)
    ),
    programExecutionResult,
    humanMessage = "Could not put overall values in tree, reason: "
  )
  
  # if an error occurs, return null, else a dictionnary "xmcdaTag -> xmcdaObject"
  
  if (is.null(tmp1) || is.null(tmp2)){
    return(null)
  } else{
    return (list(fuzziness = xmcdaAlternativesValuesFu, specificity = xmcdaAlternativesValuesSp ))
  }
  
}
