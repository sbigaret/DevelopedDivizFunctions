loadXMCDAv3 <- function(xmcdaData, inDirectory, filename, mandatory, programExecutionResult, tag){
  if (!file.exists(paste(inDirectory,filename,sep="/")))
  {
    if (mandatory)
    {
      putProgramExecutionResult(programExecutionResult, errors = paste("Could not find the mandatory file ", filename, sep=""))
    }
  } else {
    handleException(
      function() return(
        readXMCDA(file=paste(inDirectory,filename,sep="/"), xmcda=xmcdaData, tag=tag)
      ),
      programExecutionResult,
      humanMessage = paste("Unable to read & parse the file ", filename, ", reason: ", sep="")
    )
  }
}
