#Get the meta data from OpenML for specified dataset
getMetaDataForDataset <- function(did,saved=TRUE){
  #   cat("Getting meta data for : ", did, "\n") 
  if(file.exists(paste("savedMetaData/",did,".RData",sep="")) & saved){
    var.name <- load(file=paste("savedMetaData/",did,".RData",sep=""))
    dataset.meta.features <- get(var.name)
    return(dataset.meta.features)
  }
  
  library(OpenML)
  library(reshape)
  datasetId <- did
  #saveOMLConfig(apikey="dce6d7b81d7eb26de554be95c812f0db",overwrite=TRUE)
  dataset.meta.features <- getOMLDataSetQualities(did=datasetId)
  dataset.meta.features$datasetId <- datasetId
  dataset.meta.features <- cast(dataset.meta.features,formula = datasetId ~ name)
  #   print("Meta features obtained")
  save(dataset.meta.features, file=paste("savedMetaData/",did,".RData",sep=""))
  return(dataset.meta.features)
}