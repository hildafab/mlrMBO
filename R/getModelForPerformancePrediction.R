getModelForPerformancePrediction = function(learner){
  lrn = makeLearner(learner)
  vname = data(lrn$short.name, "model.RData", sep = "_")
  model = get(vname)
  return(model)
}