pml_write_files = function(x, model){
  
  # allows for separate files for each model
  if (missing(model))
    modelFolder = FALSE
  else {
    modelFolder = file.exists(paste0("results\\", model))
    if (!modelFolder)
      modelFolder = dir.create(paste0("results\\", model), recursive = TRUE)
  }

  # write files
  for(i in seq_along(x)){
    if (modelFolder)
      filename = paste0("results\\", model, "\\problem_id_",i,".txt")
    else
      filename = paste0("problem_id_",i,".txt")
    
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

learningCurves <- function(trainingSet, validationSet, modelCall,
                           modelErrorCall, start = 1, 
                           end = ifelse(!missing(trainingSet), nrow(trainingSet), NA),
                           nPoints = 10, plot= FALSE) {
  
  if (missing(trainingSet) | missing(validationSet) | missing(modelCall) | 
        missing(modelErrorCall))
    stop("trainingSet, validationSet, modelCall and modelErrorCall are necessary.")
  
  if (start <= 0 | end <= 0 | nPoints <= 0)
    stop("start, end and nPoints must be positive.")
  
  stepSize <- (end - start) / (nPoints - 1)
  stepPoints <- start + floor((seq(nPoints) - 1) * stepSize)
  err.train <- numeric(nPoints)
  err.cv <- numeric(nPoints)
  
  for (i in seq(nPoints)) {
    trainingSubset <- trainingSet[sample(nrow(trainingSet), stepPoints[i]), ]
    fit <- modelCall(trainingSubset)
    err.train[i]<- modelErrorCall(fit, trainingSubset)
    err.cv[i] <- modelErrorCall(fit, validationSet)
  }
  
  lc <- data.frame(err.train = err.train, err.cv = err.cv)
  
  if (plot) {
    plot(y=lc$err.train, x=stepPoints,
         type="l", col="blue", ylim=range(floor(lc), ceiling(lc)),
         xlab="Sample size", ylab="Error Measure")
    lines(y=lc$err.cv, x=stepPoints, col="red")
  }
  
  return(lc)
  
}