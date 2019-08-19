myRF<-function(d,ntree,nodesize){
  RandomForest <- randomForest(label ~ . ,data=d,importance=T,ntree=ntree,nodesize=nodesize,na.action=na.exclude)
  
  imps  <- data.frame(importance(RandomForest));
  impScore <- imps$MeanDecreaseAccuracy
  #return(impScore)
  imps <- imps[order(impScore,decreasing=T),]
  orderedFeatures <- rownames(imps)
  return(list(features=orderedFeatures,mod=RandomForest))
}