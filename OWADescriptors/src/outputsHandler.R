XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  ornes <- "criteriaValues",
  balance <- "criteriaValues",
  entropy <- "criteriaValues",
  divergence <- "criteriaValues",
  messages = "programExecutionResult"
)


xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}


convert <- function(descriptorsValues, programExecutionResult) {
  # converts the outputs of the computation to XMCDA objects
  
  # translate the results into XMCDA v3
  xmcdaOrnes<-.jnew("org/xmcda/XMCDA")
  xmcdaBalance<-.jnew("org/xmcda/XMCDA")
  xmcdaEntropy<-.jnew("org/xmcda/XMCDA")
  xmcdaDivergence<-.jnew("org/xmcda/XMCDA")
  
  tmp<-handleException(
    function() return(c(
      putCriteriaValues(xmcdaOrnes,descriptorsValues$ornes),
      putCriteriaValues(xmcdaBalance,descriptorsValues$balance),
      putCriteriaValues(xmcdaEntropy,descriptorsValues$entropy),
      putCriteriaValues(xmcdaDivergence,descriptorsValues$divergence)
    )
    ),
    programExecutionResult,
    humanMessage = "Could not put overall values in tree, reason: "
  )
  
  # if an error occurs, return null, else a dictionnary "xmcdaTag -> xmcdaObject"
  
  if (is.null(tmp)){
    return(null)
  } else{
    return (list(ornes = xmcdaOrnes, balance=xmcdaBalance, entropy=xmcdaEntropy, divergence=xmcdaDivergence))
  }
  
}
