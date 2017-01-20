getPerformanceFromMetaInfo = function(points,dataset.metafeatures,par.set){ 
  
   #   print("Getting performance predictions")
  pred.perf = list()
  i = 1
  #for each point
  while(i <= nrow(points)){
    #get algorithm from the point
    point = points[i, ]
    #     print(point)
    selected.learner = as.character(point[['selected.learner']])
    #     print(selected.learner)
    
    endi = i
    while(endi <= nrow(points) & points[endi,][['selected.learner']]==selected.learner){
      endi = endi + 1
    }
    
    endi = endi - 1
    point = points[i:endi,]
    i = endi + 1
    
    #get the model for the algorithm
    model = getModelForPerformancePrediction(selected.learner)
    
    drop.cols = "datasetId"
    dataset.metafeatures = dataset.metafeatures[,!names(dataset.metafeatures) %in% drop.cols]
    
    #construct the feature set from metafeatures and hyper-parameter values
    point = point[, colSums(is.na(point)) != nrow(point),drop = FALSE]
    
    point = point[,!names(point) %in% c('selected.learner'),drop = FALSE]
    
    if(ncol(point)>0){      
      #TODO: Send the trafo details till this function
      
      for(col in names(point)){
        if(!is.null(par.set$pars[[col]]$trafo)){
          point[,col] = par.set$pars[[col]]$trafo(point[,col])
        }
      }
      
      names(point) = gsub(pattern=paste(selected.learner,".",sep=""),replacement="",names(point))
      
      point.to.predict = cbind(point,dataset.metafeatures)
    }else{
      point.to.predict = dataset.metafeatures
    }
    
    
    #FIXME: This is stupid and should not happen, but the code is garbage anyways and has to be rewirrten
    point.to.predict = impute(point.to.predict, classes = list(numeric = imputeConstant(0)))$data
    
    #predict performance
    perf = predict(object=model, newdata=point.to.predict)
    
    pred.perf[[i]]=perf[[2]][[1]]
  }  
  
  #return all performances
  pred.perf = unlist(pred.perf)
  #   print("Obtained performance predictions")
  return(pred.perf)
}
