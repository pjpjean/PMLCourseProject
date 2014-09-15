pml_write_files = function(x, model){
  n = length(x)
  for(i in 1:n){
    if (missing(model))
      filename = paste0("problem_id_",i,".txt")
    else
      filename = paste0(model, "\\problem_id_",i,".txt")
    
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

learningCurves <- function(trainingSet, cvSet, modelCall,
                           modelErrorCall, start = 1, 
                           end = ifelse(!missing(trainingSet), nrow(trainingSet), NA),
                           nPoints = 10, plot= FALSE) {
  
  if (missing(trainingSet) | missing(cvSet) | missing(modelCall) | 
        missing(modelErrorCall))
    stop("trainingSet, cvSet, modelCall and modelErrorCall are necessary.")
  
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
    err.cv[i] <- modelErrorCall(fit, cvSet)
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