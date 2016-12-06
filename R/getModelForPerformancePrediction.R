getModelForPerformancePrediction = function(learner){
  lrn = makeLearner(learner)
  data(list = paste0(lrn$short.name, "_model"), envir = environment())
  model = get("model")
  return(model)
}