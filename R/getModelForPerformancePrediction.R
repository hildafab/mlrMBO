getModelForPerformancePrediction <- function(learner){
  #   cat("Getting the prediction model for learner : ",learner,"\n")
  lrn <- makeLearner(learner)
  vname <- load(file=paste('R/models/',paste(lrn$short.name,'model.RData',sep = '_'),sep = ''))
  model <- get(vname)
  #   print("Obtained prediction model")
  return(model)
}